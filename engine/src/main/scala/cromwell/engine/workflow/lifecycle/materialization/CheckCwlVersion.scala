package cromwell.engine.workflow.lifecycle.materialization

import cats.Monad
import cats.data.{EitherT, NonEmptyList}
import cats.effect.IO
import cwl.CwlDecoder.Parse
import cwl.{CommandLineTool, ExpressionTool, Workflow}
import shapeless.Poly1
import mouse.all._

object CheckCwlVersion extends Poly1{
  import cwl.CwlVersion._
  import Case.Aux
  val acceptableCwlVersion = Set(Version1, Draft3)

  def evaluateCwlVersion(version: Option[CwlVersion]): Parse[Unit] = {
    version match {
      case Some(v) if acceptableCwlVersion.contains(v) => Monad[Parse].unit
      case None => Monad[Parse].unit
      case Some(_) =>
        EitherT.fromEither[IO](
          Left(s"Cromwell does not support CWL version $version, however it does support versions: ${acceptableCwlVersion.mkString(", ")}" |> NonEmptyList.one)
        ): Parse[Unit]
    }
  }

  implicit def wf: Aux[Workflow, Parse[Unit]] = at[Workflow] {
    wf => evaluateCwlVersion(wf.cwlVersion)
  }

  implicit def clt: Aux[CommandLineTool, Parse[Unit]] = at[CommandLineTool] {
    clt => evaluateCwlVersion(clt.cwlVersion)
  }

  implicit def et: Aux[ExpressionTool, Parse[Unit]] = at[ExpressionTool] {
    et => evaluateCwlVersion(et.cwlVersion)
  }
}
