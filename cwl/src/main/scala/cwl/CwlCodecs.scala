package cwl

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import eu.timepit.refined.string._
import io.circe.refined._
import io.circe.literal._
import common.Checked

object CwlCodecs {

  import cwl.decoder._
  implicit val cwlTypeDecoder = Decoder.enumDecoder(CwlType)
  implicit val cwlVersionDecoder = Decoder.enumDecoder(CwlVersion)
  implicit val scatterMethodDecoder = Decoder.enumDecoder(ScatterMethod)
  implicit val linkMergeMethodDecoder = Decoder.enumDecoder(LinkMergeMethod)

  def decodeCwl(cwlWorkflow: String): Either[NonEmptyList[String], Cwl] = {
    //try to parse both and combine errors if they fail
    (decode[Workflow](cwlWorkflow), decode[CommandLineTool](cwlWorkflow), decode[ExpressionTool](cwlWorkflow)) match {
      case (Right(wf), _, _) => Coproduct[Cwl](wf).asRight
      case (_, Right(clt), _) => Coproduct[Cwl](clt).asRight
      case (_, _, Right(et)) => Coproduct[Cwl](et).asRight
      case (Left(wfError), Left(cltError), Left(etError)) =>
        NonEmptyList.of(
          s"Workflow parsing error: ${wfError.show}",
          s"Command Line Tool parsing error: ${cltError.show}",
          s"Expression Tool parsing error: ${etError.show}"
        ).asLeft
    }
  }


}
