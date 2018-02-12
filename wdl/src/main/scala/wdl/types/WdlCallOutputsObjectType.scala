package wdl.types

import wdl.WdlCall
import wdl.values.WdlCallOutputsObject
import wom.types.WomObjectTypeLike
import wom.values.WomValue

case class WdlCallOutputsObjectType(call: WdlCall) extends WomObjectTypeLike {
  val toDisplayString: String = "Object"

  override def coercion(): PartialFunction[Any, WomValue] = {
    case o: WdlCallOutputsObject => o
  }
}
