package zio.telemetry.opencensus

import zio._
import io.opencensus.trace._
import io.opencensus.trace.propagation.TextFormat
import zio.telemetry.opencensus.Tracing.defaultMapper

trait Tracing {

  def getCurrentSpan: UIO[Span]

  def span[R, E, A](
    name: String,
    kind: Span.Kind = null,
    toErrorStatus: ErrorMapper[E] = defaultMapper[E],
    attributes: Map[String, AttributeValue] = Map()
  )(effect: ZIO[R, E, A]): ZIO[R, E, A]

  def root[R, E, A](
    name: String,
    kind: Span.Kind = null,
    toErrorStatus: ErrorMapper[E] = defaultMapper[E],
    attributes: Map[String, AttributeValue] = Map()
  )(effect: ZIO[R, E, A]): ZIO[R, E, A]

  def fromRemoteSpan[R, E, A](
    remote: SpanContext,
    name: String,
    kind: Span.Kind = Span.Kind.SERVER,
    toErrorStatus: ErrorMapper[E] = defaultMapper[E],
    attributes: Map[String, AttributeValue] = Map()
  )(effect: ZIO[R, E, A]): ZIO[R, E, A]

  def fromRootSpan[C, R, E, A](
    format: TextFormat,
    carrier: C,
    getter: TextFormat.Getter[C],
    name: String,
    kind: Span.Kind = Span.Kind.SERVER,
    toErrorStatus: ErrorMapper[E] = defaultMapper[E],
    attributes: Map[String, AttributeValue] = Map()
  )(effect: ZIO[R, E, A]): ZIO[R, E, A]

  def inject[C](
    format: TextFormat,
    carrier: C,
    setter: TextFormat.Setter[C]
  ): UIO[Unit]

  def putAttributes(
    attrs: Map[String, AttributeValue]
  ): UIO[Unit]

}

object Tracing {

  val live: URLayer[Tracer, Tracing] =
    ZLayer.scoped(ZIO.service[Tracer].flatMap(scoped))

  def scoped(tracer: Tracer): URIO[Scope, Tracing] = {
    val acquire = for {
      currentSpan <- FiberRef.make[Span](BlankSpan.INSTANCE)
    } yield new Tracing { self =>
      override def getCurrentSpan: UIO[Span] = currentSpan.get

      override def span[R, E, A](
        name: String,
        kind: Span.Kind = Span.Kind.SERVER,
        toErrorStatus: ErrorMapper[E],
        attributes: Map[String, AttributeValue]
      )(effect: ZIO[R, E, A]): ZIO[R, E, A] =
        ZIO.scoped[R] {
          for {
            parent <- getCurrentSpan
            span   <- createSpan(parent, name, kind)
            res    <- finalizeSpanUsingEffect(putAttributes(attributes) *> effect, toErrorStatus)(span)
          } yield res
        }

      override def root[R, E, A](
        name: String,
        kind: Span.Kind,
        toErrorStatus: ErrorMapper[E],
        attributes: Map[String, AttributeValue]
      )(effect: ZIO[R, E, A]): ZIO[R, E, A] =
        ZIO.scoped[R] {
          createSpan(BlankSpan.INSTANCE, name, kind).flatMap(span =>
            finalizeSpanUsingEffect(
              putAttributes(attributes) *> effect,
              toErrorStatus
            )(span)
          )
        }

      override def fromRemoteSpan[R, E, A](
        remote: SpanContext,
        name: String,
        kind: Span.Kind,
        toErrorStatus: ErrorMapper[E],
        attributes: Map[String, AttributeValue]
      )(effect: ZIO[R, E, A]): ZIO[R, E, A] =
        ZIO.scoped[R] {
          createSpanFromRemote(remote, name, kind).flatMap(span =>
            finalizeSpanUsingEffect(
              putAttributes(attributes) *> effect,
              toErrorStatus
            )(span)
          )
        }

      override def fromRootSpan[C, R, E, A](
        format: TextFormat,
        carrier: C,
        getter: TextFormat.Getter[C],
        name: String,
        kind: Span.Kind = Span.Kind.SERVER,
        toErrorStatus: ErrorMapper[E] = defaultMapper[E],
        attributes: Map[String, AttributeValue] = Map()
      )(effect: ZIO[R, E, A]): ZIO[R, E, A] =
        ZIO
          .attempt(format.extract(carrier, getter))
          .foldZIO(
            _ => root(name, kind, toErrorStatus)(effect),
            remote => fromRemoteSpan(remote, name, kind, toErrorStatus, attributes)(effect)
          )

      override def putAttributes(
        attributes: Map[String, AttributeValue]
      ): UIO[Unit] =
        for {
          current <- getCurrentSpan
          _       <- ZIO.succeed(attributes.foreach { case (k, v) =>
                       current.putAttribute(k, v)
                     })
        } yield ()

      override def inject[C](
        format: TextFormat,
        carrier: C,
        setter: TextFormat.Setter[C]
      ): UIO[Unit] =
        for {
          current <- getCurrentSpan
          _       <- ZIO.succeed(format.inject(current.getContext, carrier, setter))
        } yield ()

      private def createSpan(
        parent: Span,
        name: String,
        kind: Span.Kind
      ): URIO[Scope, Span] =
        ZIO.acquireRelease(
          ZIO.succeed(
            tracer
              .spanBuilderWithExplicitParent(name, parent)
              .setSpanKind(kind)
              .startSpan()
          )
        )(span => ZIO.succeed(span.end()))

      private def createSpanFromRemote(
        parent: SpanContext,
        name: String,
        kind: Span.Kind
      ): URIO[Scope, Span] =
        ZIO.acquireRelease(
          ZIO.succeed(
            tracer
              .spanBuilderWithRemoteParent(name, parent)
              .setSpanKind(kind)
              .startSpan()
          )
        )(span => ZIO.succeed(span.end()))

      private def finalizeSpanUsingEffect[R, E, A](
        effect: ZIO[R, E, A],
        toErrorStatus: ErrorMapper[E]
      )(span: Span): ZIO[R, E, A] =
        for {
          r <- currentSpan
                 .locally(span)(effect)
                 .tapErrorCause(setErrorStatus(span, _, toErrorStatus))
        } yield r

      private def setErrorStatus[E](
        span: Span,
        cause: Cause[E],
        toErrorStatus: ErrorMapper[E]
      ): UIO[Unit] = {
        val errorStatus =
          cause.failureOption.flatMap(toErrorStatus.lift).getOrElse(Status.UNKNOWN)
        ZIO.succeed(span.setStatus(errorStatus))
      }

    }

    def release(tracing: Tracing) =
      tracing.getCurrentSpan.flatMap(span => ZIO.succeed(span.end()))

    ZIO.acquireRelease(acquire)(release)
  }

  def defaultMapper[E]: ErrorMapper[E] =
    Map.empty

}
