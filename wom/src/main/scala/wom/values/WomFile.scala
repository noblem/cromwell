package wom.values

import wom.types._

import scala.util.{Success, Try}

sealed trait WomFile extends WomValue {
  def hostPath: String

  def containerPath: String

  def womFileType: WomFileType

  final def womType: WomType = womFileType

  // Note this picks the host path as the "true" path of this file since container paths are ephemeral and might even
  // be reassigned if a file is localized to different containers. There are subtypes of `WomFile` such as `WomGlobFile`
  // that only have container paths and will need to override this.
  override def valueString = hostPath

  /**
    * Converts the location using f() recursively converting any files referred to by this file.
    *
    * A caller may need a modification to the path, for example:
    *
    * - Convert a cloud path to a relative path for command line instantiation.
    * - Convert a cloud path to a path on a mounted disk.
    *
    * WomFile such as WomMaybePopulatedFile and WomMaybeListedDirectory may contain references to other WomFile. mapFile
    * will traverse any files definied within file, also converting the values within those files too.
    *
    * @param f The function to update the location.
    * @return A new WomFile with the updated location.
    * @see [[wom.values.WomValue.collectAsSeq]]
    * @see [[wom.WomFileMapper.mapWomFiles]]
    */
  def assignContainerPath(f: String => String): WomFile

  /**
    * Does the opposite of the method above.
    */
  def assignHostPath(f: String => String): WomFile

  /**
    * Returns the WomPrimitiveFile instances recursively referenced by this instance.
    *
    * WomMaybeListedDirectory instances return either just the directory as an WomUnlistedDirectory, or if there is a
    * listing then returns the recursive listing. WomMaybePopulatedFile instances return that instance as a
    * WomSingleFile plus any primitives recursively discovered in secondaryFiles.
    *
    * WomPrimitiveFile instances return just the instance.
    */
  def flattenFiles: Seq[WomPrimitiveFile] = {
    this match {
      case womMaybeListedDirectory: WomMaybeListedDirectory =>
        womMaybeListedDirectory.listingOption.getOrElse(Nil).toList match {
          case Nil => womMaybeListedDirectory.hostPathOption.toList.map(WomUnlistedDirectory(_))
          case list => list.flatMap(_.flattenFiles)
        }
      case womMaybePopulatedFile: WomMaybePopulatedFile =>
        val primaryFiles: Seq[WomPrimitiveFile] = womMaybePopulatedFile.hostPathOption.toList.map(WomSingleFile(_))
        womMaybePopulatedFile.secondaryFiles.foldLeft(primaryFiles) {
          (womFiles, secondaryFile) =>
            womFiles ++ secondaryFile.flattenFiles
        }
      case womPrimitiveFile: WomPrimitiveFile => List(womPrimitiveFile)
    }
  }
}

object WomFile {
  def apply(fileType: WomFileType, value: String) = {
    fileType match {
      case WomUnlistedDirectoryType => WomUnlistedDirectory(value)
      case WomSingleFileType => WomSingleFile(value)
      case WomGlobFileType => WomGlobFile(value)
      case WomMaybeListedDirectoryType => WomMaybeListedDirectory(value)
      case WomMaybePopulatedFileType => WomMaybePopulatedFile(value)
    }
  }
}

sealed trait WomPrimitiveFile extends WomFile with WomPrimitive

/**
  * A directory represented only by a path to a directory.
  *
  * Should not be passed into command line generation. Instead, the execution engine should create a WomListedDirectory
  * locating the files/directories within the `value` and filling in the listing.
  *
  * @param hostPath The location of the directory, possibly in the cloud.
  */
final case class WomUnlistedDirectory(hostPath: String, containerPathOption: Option[String] = None) extends WomPrimitiveFile {

  override val womFileType: WomFileType = WomUnlistedDirectoryType

  override def containerPath = containerPathOption.get

  override def toWomString = s""""$hostPath""""

  override def add(rhs: WomValue): Try[WomValue] = rhs match {
    case r: WomString => Success(this.copy(hostPath = hostPath + r.value))
    case r: WomOptionalValue => evaluateIfDefined("+", r, add)
    case _ => invalid(s"$hostPath + $rhs")
  }

  override def equals(rhs: WomValue): Try[WomBoolean] = rhs match {
    case r: WomUnlistedDirectory => Success(WomBoolean(this.equals(r)))
    case r: WomString => Success(WomBoolean(hostPath.equals(r.value)))
    case r: WomOptionalValue => evaluateIfDefined("==", r, equals)
    case _ => invalid(s"$hostPath == $rhs")
  }

  override def assignContainerPath(f: String => String): WomUnlistedDirectory = {
    this.copy(containerPathOption = Option(f(hostPath)))
  }

  override def assignHostPath(f: String => String): WomUnlistedDirectory = {
    // 😬😬😬
    this.copy(hostPath = (containerPathOption map f).get)
  }
}

/**
  * A file with no additional files.
  *
  * @param hostPath The location of the file, possibly in the cloud.
  * @param containerPathOption The location of the file in the container, never in the cloud.
  */
final case class WomSingleFile(hostPath: String, containerPathOption: Option[String] = None) extends WomPrimitiveFile {

  override def containerPath: String = ???

  override val womFileType: WomFileType = WomSingleFileType

  override def toWomString = s""""$hostPath""""

  override def add(rhs: WomValue): Try[WomValue] = rhs match {
    case r: WomString => Success(this.copy(hostPath = hostPath + r.value))
    case r: WomOptionalValue => evaluateIfDefined("+", r, add)
    case _ => invalid(s"$hostPath + $rhs")
  }

  override def equals(rhs: WomValue): Try[WomBoolean] = rhs match {
    case r: WomSingleFile => Success(WomBoolean(this.equals(r)))
    case r: WomString => Success(WomBoolean(hostPath.equals(r.value)))
    case r: WomOptionalValue => evaluateIfDefined("==", r, equals)
    case _ => invalid(s"$hostPath == $rhs")
  }

  override def assignContainerPath(f: String => String): WomSingleFile = {
    this.copy(containerPathOption = Option(f(hostPath)))
  }

  override def assignHostPath(f: String => String): WomSingleFile = {
    // 😬😬😬
    this.copy(hostPath = containerPathOption.map(f).get)
  }
}

/**
  * A glob that will be expanded into an array of files from the path in value.
  *
  * Ex:
  * {{{
  *   Array[File] myBams = glob("outdir/\*.bam")
  * }}}
  *
  * @param containerPath The path of the glob within the container.
  */
final case class WomGlobFile(containerPath: String) extends WomPrimitiveFile {

  // Supposedly this is always container paths.
  override val hostPath: String = null

  override def valueString: String = containerPath

  override val womFileType: WomFileType = WomGlobFileType

  override def toWomString = s"""glob("$containerPath")"""

  override def add(rhs: WomValue): Try[WomValue] = rhs match {
    case r: WomString => Success(this.copy(containerPath + r.value))
    case r: WomOptionalValue => evaluateIfDefined("+", r, add)
    case _ => invalid(s"$containerPath + $rhs")
  }

  override def equals(rhs: WomValue): Try[WomBoolean] = rhs match {
    case r: WomGlobFile => Success(WomBoolean(containerPath.equals(r.containerPath) && womType.equals(r.womType)))
    case r: WomString => Success(WomBoolean(containerPath.toString.equals(r.value.toString)))
    case r: WomOptionalValue => evaluateIfDefined("==", r, equals)
    case _ => invalid(s"$containerPath == $rhs")
  }

  // Shouldn't be called, right?
  override def assignContainerPath(f: String => String): WomGlobFile =
    throw new UnsupportedOperationException("WomGlobFile is always a container path")

  override def assignHostPath(f: String => String): WomGlobFile =
    throw new UnsupportedOperationException("WomGlobFile is always a container path")

}


/**
  * A directory possibly with a listing of other files/directories.
  *
  * @param hostPathOption   The location of the directory, possibly in the cloud.
  * @param containerPathOption The location of the directory with respect to the container.
  * @param listingOption An optional listing of files/directories, either supplied by a user or generated by the engine.
  */
final case class WomMaybeListedDirectory(hostPathOption: Option[String] = None,
                                         containerPathOption: Option[String] = None,
                                         listingOption: Option[Seq[WomFile]] = None) extends WomFile {

  override def hostPath: String = hostPathOption.getOrElse(
    throw new UnsupportedOperationException(s"hostPath is not available: $this"))

  override def containerPath: String = containerPathOption.getOrElse(
    throw new UnsupportedOperationException(s"containerPath is not available: $this"))

  override val womFileType: WomFileType = WomMaybeListedDirectoryType

  // TODO: WOM: WOMFILE: This isn't even close to a WDL representation (and maybe belongs in WDL?) of this class, but w/o it other areas of the code crash
  override def toWomString = s""""$hostPath""""

  override def assignContainerPath(f: String => String): WomMaybeListedDirectory = {
    this.copy(containerPathOption = hostPathOption.map(f), listingOption = listingOption.map(_.map(_.assignContainerPath(f))))
  }

  override def assignHostPath(f: String => String): WomMaybeListedDirectory = {
    this.copy(hostPathOption = containerPathOption.map(f), listingOption = listingOption.map(_.map(_.assignHostPath(f))))
  }
}

object WomMaybeListedDirectory {
  def apply(value: String): WomMaybeListedDirectory = WomMaybeListedDirectory(hostPathOption = Option(value))
}

/**
  * A file possibly populated with a path plus optional checksum/size/etc.
  *
  * @param hostPathOption      The location of the file as visible from the host, possibly in the cloud.
  * @param containerPathOption The location of the file as visible from the container, never a cloud path.
  * @param checksumOption      An optional checksum of the file contents.
  * @param sizeOption          An optional size of the file contents in bytes.
  * @param formatOption        An optional format description of the file contents.
  * @param contentsOption      The optional text contents of the file.
  * @param secondaryFiles      Any files associated with this file.
  */
final case class WomMaybePopulatedFile(hostPathOption: Option[String] = None,
                                       containerPathOption: Option[String] = None,
                                       checksumOption: Option[String] = None,
                                       sizeOption: Option[Long] = None,
                                       formatOption: Option[String] = None,
                                       contentsOption: Option[String] = None,
                                       secondaryFiles: Seq[WomFile] = Vector.empty) extends WomFile {

  override def hostPath: String = hostPathOption.getOrElse(
    throw new UnsupportedOperationException(s"hostPath is not available: $this"))

  override def containerPath: String = containerPathOption.getOrElse(
    throw new UnsupportedOperationException(s"containerPath is not available: $this"))

  override val womFileType: WomFileType = WomMaybePopulatedFileType

  // TODO: WOM: WOMFILE: This isn't even close to a WDL representation (and maybe belongs in WDL?) of this class, but w/o it other areas of the code crash
  override def toWomString = s""""$hostPath""""

  override def assignContainerPath(f: String => String): WomMaybePopulatedFile = {
    this.copy(containerPathOption = hostPathOption.map(f), secondaryFiles = secondaryFiles.map(_.assignContainerPath(f)))
  }

  override def assignHostPath(f: String => String): WomMaybePopulatedFile = {
    this.copy(hostPathOption = containerPathOption.map(f), secondaryFiles = secondaryFiles.map(_.assignHostPath(f)))
  }
}

object WomMaybePopulatedFile {
  def apply(value: String): WomMaybePopulatedFile = WomMaybePopulatedFile(hostPathOption = Option(value))
}
