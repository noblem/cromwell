package cwl

import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._
import common.validation.ErrorOr._
import common.validation.Validation._
import wom.util.JsEncoder
import wom.values.{WomFile, WomGlobFile, WomMaybeListedDirectory, WomMaybePopulatedFile, WomSingleFile, WomUnlistedDirectory, WomValue}

import scala.collection.JavaConverters._

class CwlJsEncoder extends JsEncoder {
  /**
    * Overrides encoding to also support wom file or directory values.
    */
  override def encode(value: WomValue): ErrorOr[AnyRef] = {
    value match {
      case file: WomFile => encodeFileOrDirectory(file)
      case _ => super.encode(value)
    }
  }

  /**
    * Encodes a sequence of wom file or directory values.
    */
  def encodeFileOrDirectories(values: Seq[WomFile]): ErrorOr[Array[java.util.Map[String, AnyRef]]] = {
    values.toList.traverse(encodeFileOrDirectory).map(_.toArray)
  }

  /**
    * Encodes a wom file or directory value.
    */
  def encodeFileOrDirectory(value: WomFile): ErrorOr[java.util.Map[String, AnyRef]] = {
    value match {
      case directory: WomUnlistedDirectory => encodeDirectory(WomMaybeListedDirectory(directory.hostPath))
      case file: WomSingleFile => encodeFile(WomMaybePopulatedFile(file.hostPath))
      case glob: WomGlobFile => encodeFile(WomMaybePopulatedFile(glob.hostPath))
      case directory: WomMaybeListedDirectory => encodeDirectory(directory)
      case file: WomMaybePopulatedFile => encodeFile(file)
    }
  }

  /**
    * Encodes a wom file.
    */
  def encodeFile(file: WomMaybePopulatedFile): ErrorOr[java.util.Map[String, AnyRef]] = {
    val lifted: ErrorOr[Map[String, Option[AnyRef]]] = Map(
      "class" -> validate(Option("File")),
      "location" -> validate(Option(file.hostPath)),
      "path" -> validate(Option(file.hostPath)),
      "basename" -> validate(Option(File.basename(file.hostPath))),
      "dirname" -> validate(Option(File.dirname(file.hostPath))),
      "nameroot" -> validate(Option(File.nameroot(file.hostPath))),
      "nameext" -> validate(Option(File.nameext(file.hostPath))),
      "checksum" -> validate(file.checksumOption),
      "size" -> validate(file.sizeOption.map(Long.box)),
      "secondaryFiles" -> encodeFileOrDirectories(file.secondaryFiles).map(Option(_)),
      "format" -> validate(file.formatOption),
      "contents" -> validate(file.contentsOption)
    ).sequence

    flattenToJava(lifted)
  }

  /**
    * Encodes a wom directory.
    */
  def encodeDirectory(directory: WomMaybeListedDirectory): ErrorOr[java.util.Map[String, AnyRef]] = {
    val lifted: ErrorOr[Map[String, Option[AnyRef]]] = Map(
      "class" -> validate(Option("Directory")),
      "location" -> validate(directory.hostPathOption),
      "path" -> validate(Option(directory.hostPath)),
      "basename" -> validate(Option(Directory.basename(directory.hostPath))),
      "listing" -> directory.listingOption.traverse(encodeFileOrDirectories)
    ).sequence

    flattenToJava(lifted)
  }

  /**
    * Flattens the None values out of a scala map to a java version compatible with javascript.
    */
  def flattenToJava(lifted: ErrorOr[Map[String, Option[AnyRef]]]): ErrorOr[java.util.Map[String, AnyRef]] = {
    val flattened: ErrorOr[Map[String, AnyRef]] = lifted.map(
      _ collect { case (key, Some(value)) => (key, value) }
    )

    flattened.map(_.asJava)
  }
}
