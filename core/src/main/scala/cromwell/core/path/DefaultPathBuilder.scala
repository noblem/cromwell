package cromwell.core.path

import java.net.URI
import java.nio.file.FileSystems

import com.google.common.net.UrlEscapers

import scala.util.Try

/**
  * PathBuilder using the default FileSystem to attempt to build a Path.
  */
case object DefaultPathBuilder extends PathBuilder {
  override lazy val name = FileSystems.getDefault.getClass.getSimpleName

  def build(path: java.nio.file.Path): DefaultPath = DefaultPath(path)

  override def build(pathAsString: String): Try[DefaultPath] = Try {
    val uri = URI.create(UrlEscapers.urlFragmentEscaper().escape(pathAsString))
    Option(uri.getScheme) match {
      case Some("file") | None =>

        if (pathAsString.startsWith("file://")) {
          // NOTE: Legacy support for old paths generated as URIs by the old .toRealString
          val host = Option(uri.getHost) getOrElse ""
          val path = Option(uri.getPath) getOrElse ""
          DefaultPath(FileSystems.getDefault.getPath(host, path))
        } else {
          DefaultPath(FileSystems.getDefault.getPath(pathAsString))
        }

      case _ => throw new RuntimeException(s"Cannot build a local path from $pathAsString")
    }
  }

  /**
    * Bypasses build()'s URI scheme detection and attempts to directly create a path, providing behavior similar to
    * java.nio.Paths.get.
    *
    * Works even for bad paths like "gs:myfilename.txt", generated by:
    * centaur 546c001, wf sub_function, call sub, input swappedFile, only when run on JES
    */
  def get(first: String, more: String*): Path = DefaultPath(FileSystems.getDefault.getPath(first, more: _*))

  def createTempDirectory(prefix: String): DefaultPath = DefaultPath(java.nio.file.Files.createTempDirectory(prefix))

  def createTempFile(prefix: String = "", suffix: String = "", parent: Option[Path] = None): Path = {
    parent match {
      case Some(dir) => dir.createTempFile(prefix, suffix)
      case _ => DefaultPath(java.nio.file.Files.createTempFile(prefix, suffix))
    }
  }
}

case class DefaultPath private[path](nioPath: NioPath) extends Path {
  override def newPath(nioPath: NioPath): DefaultPath = DefaultPath(nioPath)

  override def pathAsString: String = nioPath.toString

  override def pathWithoutScheme: String = nioPath.toAbsolutePath.toString
}
