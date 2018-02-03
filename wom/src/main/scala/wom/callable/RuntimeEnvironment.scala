package wom.callable

/**
  * Parameter documentation quoted from CWL Spec.
  *
  * @param hostOutputPath runtime.outdir: an absolute host path to the designated output directory
  * @param hostTempPath runtime.tmpdir: an absolute host path to the designated temporary directory
  * @param cores runtime.cores: number of CPU cores reserved for the tool process
  * @param ram runtime.ram: amount of RAM in mebibytes (2**20) reserved for the tool process
  * @param outputPathSize runtime.outdirSize: reserved storage space available in the designated output directory
  * @param tempPathSize runtime.tmpdirSize: reserved storage space available in the designated temporary directory
  */
case class RuntimeEnvironment(hostOutputPath: String,
                              hostTempPath: String,
                              cores: Int,
                              ram: Double,
                              outputPathSize: Long,
                              tempPathSize: Long,
                              containerOutputPath: Option[String] = None,
                              containerTempPath: Option[String] = None)

