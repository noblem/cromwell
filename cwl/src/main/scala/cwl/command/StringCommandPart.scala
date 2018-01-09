package cwl.command

import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import cwl.command.StringCommandPart._
import wom._
import wom.callable.RuntimeEnvironment
import wom.expression.IoFunctionSet
import wom.graph.LocalName
import wom.values.WomValue

case class StringCommandPart(literal: String) extends CommandPart {
  override def instantiate(inputsMap: Map[LocalName, WomValue],
                           functions: IoFunctionSet,
                           valueMapper: (WomValue) => WomValue,
                           runtimeEnvironment: RuntimeEnvironment): ErrorOr[InstantiatedCommand] =
    InstantiatedCommand(literal.quoteForShell).validNel
}

object StringCommandPart {

  implicit class ShellQuoteHelper(val s: String) extends AnyVal {
    // Escape any double quote characters in the string and surround with double quotes.
    def quoteForShell: String = '"' + s.replaceAll("\"", "\\\"") + '"'
  }
}
