package zio.telemetry.opentelemetry.example

import zio.config.magnolia._
import zio.config.typesafe.TypesafeConfig
import zio.telemetry.opentelemetry.example.config.AppConfig
import zio.telemetry.opentelemetry.example.http.{BackendHttpApp, BackendHttpServer}
import zio._
import zio.telemetry.opentelemetry.context.ContextStorage
import zio.telemetry.opentelemetry.OpenTelemetry
import zio.telemetry.opentelemetry.example.otel.OtelSdk
import zio.telemetry.opentelemetry.metrics.Meter

object BackendApp extends ZIOAppDefault {

  private val configLayer = TypesafeConfig.fromResourcePath(descriptor[AppConfig])

  private val instrumentationScopeName = "zio.telemetry.opentelemetry.example.BackendApp"
  private val resourceName             = "opentelemetry-example-backend"

  val tickRefLayer: ULayer[Ref[Long]] =
    ZLayer(
      for {
        ref <- Ref.make(0L)
        _   <- ref
                 .update(_ + 1)
                 .repeat[Any, Long](Schedule.spaced(1.second))
                 .forkDaemon
      } yield ref
    )

  val globalTickCounterLayer: RLayer[Meter with Ref[Long], Unit] =
    ZLayer.scoped(
      for {
        meter <- ZIO.service[Meter]
        ref   <- ZIO.service[Ref[Long]]
        _     <- meter.observableCounter("tick_counter") { om =>
                   for {
                     tick <- ref.get
                     _    <- om.record(tick)
                   } yield ()
                 }
      } yield ()
    )

  override def run: ZIO[Scope, Any, ExitCode] =
    ZIO
      .serviceWithZIO[BackendHttpServer](_.start.exitCode)
      .provide(
        configLayer,
        BackendHttpServer.live,
        BackendHttpApp.live,
        OtelSdk.custom(resourceName),
        OpenTelemetry.tracing(instrumentationScopeName),
        OpenTelemetry.meter(instrumentationScopeName),
        OpenTelemetry.logging(instrumentationScopeName),
        OpenTelemetry.baggage(),
        globalTickCounterLayer,
        tickRefLayer,
        ContextStorage.fiberRef
      )

}
