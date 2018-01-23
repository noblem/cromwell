package cwl

import cats.data.NonEmptyList
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import eu.timepit.refined.string._
import io.circe.refined._
import io.circe.literal._
import common.Checked
import shapeless.Coproduct
import cats.data.NonEmptyList
import io.circe._
import io.circe.parser._
import io.circe.shapes._
import io.circe.generic.auto._
import eu.timepit.refined.string._
import io.circe.refined._
import io.circe.literal._
import shapeless.Coproduct
import cats.syntax.either._
import cats.syntax.show._
import io.circe.shapes._

object CwlCodecs {

  import cwl.decoder._
  implicit val cwlTypeDecoder = Decoder.enumDecoder(CwlType)
  implicit val cwlVersionDecoder = Decoder.enumDecoder(CwlVersion)
  implicit val scatterMethodDecoder = Decoder.enumDecoder(ScatterMethod)
  implicit val linkMergeMethodDecoder = Decoder.enumDecoder(LinkMergeMethod)

  implicit val wfD = implicitly[Decoder[Workflow]]
  implicit val cltD = implicitly[Decoder[CommandLineTool]]
  implicit val etD = implicitly[Decoder[ExpressionTool]]

  def decodeCwl(in: String): Checked[CwlFile] = decodeAccumulating[CwlFile](in).leftMap(_.map(_.getMessage).map(s"error parsing: $in" + _)).toEither


}
