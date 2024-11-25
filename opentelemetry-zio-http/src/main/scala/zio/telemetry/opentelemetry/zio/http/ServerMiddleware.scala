package zio.telemetry.opentelemetry.zio.http

import zio._
import zio.http._
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.telemetry.opentelemetry.tracing.propagation.TraceContextPropagator
import zio.telemetry.opentelemetry.context.IncomingContextCarrier
import io.opentelemetry.api.trace.SpanKind
import zio.telemetry.opentelemetry.tracing.StatusMapper

object ServerMiddleware {

  def tracing(
    spanName: Request => String = Defaults.spanName,
    allowedRequestHeaders: Set[Header] = Defaults.allowedRequestHeaders,
    allowedResponseHeaders: Set[Header] = Defaults.allowedResponseHeaders
  ): URLayer[Tracing, Middleware[Any]] =
    ZLayer.fromZIO(
      for {
        tracing <- ZIO.service[Tracing]
      } yield new Middleware[Any] {

        override def apply[Env1 <: Any, Err](routes: Routes[Env1, Err]): Routes[Env1, Err] =
          routes.transform[Env1] { h =>
            handler { (request: Request) =>
              val result = for {
                response <- h(request)
                // set attributes
                

              } yield response

              result @@ tracing.aspects.extractSpan(
                TraceContextPropagator.default,
                headersCarrier(request.allHeaders),
                spanName(request),
                spanKind = SpanKind.SERVER
              )
            }
          }
      }
    )

  object Defaults {
    // https://opentelemetry.io/docs/specs/semconv/http/http-spans/#name
    // The recommended format is: "{method} {target}"
    // NOTE: it seems we can't build a proper {target} from Request here
    // because it requires to be aware of the structure of a particular url.
    // Probably worth rising an issue in zio-http to add the url template info into Request
    def spanName: Request => String                = request => s"${request.method.name}"
    def allowedRequestHeaders: Set[Header]         = Set.empty
    def allowedResponseHeaders: Set[Header]        = Set.empty
    def statusMapper[E]: StatusMapper[E, Response] = StatusMapper.default
  }



  private def headersCarrier(headers: Headers): IncomingContextCarrier[Headers] =
    new IncomingContextCarrier[Headers] {

      override val kernel: Headers = headers

      override def getAllKeys(carrier: Headers): Iterable[String] =
        carrier.headers.map(_.headerName)

      override def getByKey(carrier: Headers, key: String): Option[String] =
        carrier.get(key)

    }

}
