package tech.kevinbreidenbach.swaggerella

import tech.kevinbreidenbach.swaggerella.config.SwaggerellaConfig
import tech.kevinbreidenbach.swaggerella.core.CodeGenerator

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      config <- SwaggerellaConfig.fromArgs(args)
      _      <- CodeGenerator.generate(config)
    } yield ExitCode.Success
  }.handleErrorWith { error =>
    IO.consoleForIO.errorln(s"Error: ${error.getMessage}").as(ExitCode.Error)
  }
}
