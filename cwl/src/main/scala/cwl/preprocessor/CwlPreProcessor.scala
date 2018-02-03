package cwl.preprocessor

import better.files.{File => BFile}
import cats.data.NonEmptyList
import cats.syntax.either._
import common.validation.ErrorOr.ErrorOr
import common.validation.Parse
import common.validation.Validation._
import common.validation.Parse._
import cwl.command.ParentName
import cwl.{CwlDecoder, FileAndId, FullyQualifiedName}
import cwl.preprocessor.CwlPreProcessor._
import io.circe.Json
import io.circe.optics.JsonPath._
import mouse.all._
import org.slf4j.LoggerFactory

object CwlPreProcessor {
  private val Log = LoggerFactory.getLogger("CwlPreProcessor")
  private val LocalScheme = "file://"

  private [preprocessor] object CwlReference {
    def fromString(in: String) = {
      in.startsWith(LocalScheme).option {
        FullyQualifiedName.maybeApply(in)(ParentName.empty) match {
          case Some(FileAndId(file, _, _)) => CwlReference(BFile(file.stripFilePrefix), in)
          case _ => CwlReference(BFile(in.stripFilePrefix), in)
        }
      }
    }

    def apply(file: BFile, pointer: Option[String]) = {
      // prepends file:// to the absolute file path
      val prefixedFile = s"$LocalScheme${file.pathAsString}"
      val fullReference = pointer.map(p => s"$prefixedFile#$p").getOrElse(prefixedFile)
      new CwlReference(file, fullReference)
    }
  }

  /**
    * Saladed CWLs reference other local CWL "node" (workflow or tool) using a URI as follow:
    * file:///path/to/file/containing/node.cwl[#pointer_to_node]
    * #pointer_to_node to node is optional, and will specify which workflow or tool is being targeted in the file.
    *
    * e.g:
    *   {
    *     "class": "Workflow",
    *     "id": "file:///path/to/workflow/workflow.cwl",
    *     ...
    *     "steps": [
    *       {
    *         "run": "file:///path/to/workflow/multi_tools.cwl#my_tool",
    *         ...
    *       }
    *     ]  
    *   }
    *
    * This snippet contains 2 references, one that is the ID of this workflow, the other one is the run step pointing to "my_tool" in "/path/to/workflow/multi_tools.cwl"
    *
    * @param file: the file containing the referenced node. e.g: File(/path/to/file/containing/node.cwl)
    * @param fullReference: the full reference string as it is found in the saladed json. e.g: "file:///path/to/file/containing/node.cwl#pointer_to_node"
    */
  private [preprocessor] case class CwlReference(file: BFile, fullReference: String) {
    override def toString = fullReference
    val pointer: Option[String] = fullReference.split("#") match {
      case Array(_, p) => Option(p)
      case _ => None
    }
  }

  private [preprocessor] type BreadCrumb = List[CwlReference]
  private [preprocessor] type ProcessedReferences = Map[CwlReference, Json]
  private [preprocessor] type UnProcessedReferences = Map[CwlReference, Json]

  /**
    * A Cwl json that has been processed (saladed and flattened), as well as its processed dependencies.
    */
  private case class ProcessedJsonAndDependencies(processedJson: Json, processedDependencies: ProcessedReferences)

  val saladCwlFile: BFile => Parse[String] = { file =>
    Log.info(s"Salading ${file.pathAsString}")
    CwlDecoder.saladCwlFile(file) 
  }

  implicit class PrintableJson(val json: Json) extends AnyVal {
    def printCompact = io.circe.Printer.noSpaces.pretty(json)
  }

  private [preprocessor] implicit class EnhancedCwlId(val id: String) extends AnyVal {
    def asReference: Option[CwlReference] = CwlReference.fromString(id)
    def stripFilePrefix = id.stripPrefix(LocalScheme)
  }
}

/**
  * Class to create a standalone version of a CWL file.
  * @param saladFunction function that takes a file and produce a saladed version of the content
  */
class CwlPreProcessor(saladFunction: BFile => Parse[String] = saladCwlFile) {

  /**
    * Pre-process a CWL file and create a standalone, runnable (given proper inputs), inlined version of its content.
    *
    * The general idea is to work on CwlReferences, starting from the one coming to this function in the form of file and optional root.
    * The goal is to look at the steps in this workflow that point to other references, and recursively flatten them until we can replace the step with
    * its flat version.
    *
    * There are 3 pieces of information that are carried around during this process:
    *  1) ProcessedReferences: A Map[CwlReference, Json] of CwlReference for which we have the fully processed (saladed AND flattened) Json value.
    *
    *  2) UnProcessedReferences: A Map[CwlReference, Json] of CwlReference for which we have the saladed but NOT flattened Json value.
    *     This can happen because a file can contain multiple tools / workflows. When we salad / parse this file, we get (CwlReference, Json) pairs
    *     for all the workflow / tools in the file, but they are not flattened yet.
    *     We keep this to avoid having to re-salad / re-parse files unnecessarily.
    *
    *  3) BreadCrumb: A List[CwlReference] used to follow the trail of CwlReferences that we are processing as we recurse down.
    *     This is used to be able to detect circular dependencies (if the cwlReference being processed is in that list, then we have a circular dependency) .
    *
    */
  def preProcessCwlFile(file: BFile, cwlRoot: Option[String]): Parse[Json] = {
    val reference = CwlReference(file, cwlRoot)
    def flatten: Parse[ProcessedJsonAndDependencies] = flattenCwlReference(CwlReference(file, cwlRoot), Map.empty, Map.empty, Set.empty)
    
    for {
      original <- parseYaml(file.contentAsString)
      flattened <- original
        .asObject
        .flatMap(_.kleisli("id").flatMap(_.asString)) match {
        case Some(id) if id == reference.fullReference => flatten.map(_.processedJson)
        // This by passes the pre-processing if the file has an id which doesn't match the file path,
        // meaning it has already been saladed / pre-processed elsewhere
        case _ => original.validParse
      }
    } yield flattened
  }

  /**
    * Convenience method to get the processed workflow as a string.
    */
  def preProcessCwlFileToString(file: BFile, cwlRoot: Option[String]): ErrorOr[String] = {
    preProcessCwlFile(file, cwlRoot).value.unsafeRunSync().map(_.printCompact).toValidated
  }

  /**
    * Flatten the cwl reference given already known processed references.
    */
  private def flattenCwlReference(cwlReference: CwlReference,
                                  unProcessedReferences: UnProcessedReferences,
                                  processedReferences: ProcessedReferences,
                                  breadCrumbs: Set[CwlReference]): Parse[ProcessedJsonAndDependencies] = {
    for {
      // parse the file containing the reference
      parsed <- saladAndParse(cwlReference.file)
      // Get a Map[CwlReference, Json] from the parsed file. If the file is a JSON object and only contains one node, the map will only have 1 element 
      newUnProcessedReferences = mapIdToContent(parsed).toMap
      // The reference json in the file
      referenceJson <- newUnProcessedReferences
        .collectFirst({ case (ref, json) if ref.pointer == cwlReference.pointer => json })
        .toParse(s"Cannot find a tool or workflow with ID ${cwlReference.fullReference} in file ${cwlReference.file.pathAsString}")
      // Process the reference json
      processed <- flattenJson(referenceJson, newUnProcessedReferences ++ unProcessedReferences, processedReferences, breadCrumbs + cwlReference)
    } yield processed
  }

  /**
    * Given a reference from a step's run field, flattens it and return it
    * @param unProcessedReferences references that have been parsed and saladed (we have the json), but not flattened yet.
    * @param checkedProcessedReferences references that are fully processed
    * @param cwlReference reference being processed
    * @return a new ProcessedReferences Map including this cwlReference processed along with all the dependencies
    *         that might have been processed recursively.
    */
  private def processCwlRunReference(unProcessedReferences: UnProcessedReferences,
                                     breadCrumbs: Set[CwlReference])
                                    (checkedProcessedReferences: Parse[ProcessedReferences],
                                     cwlReference: CwlReference): Parse[ProcessedReferences] = {
    checkedProcessedReferences flatMap { processedReferences =>
      // If the reference has already been processed, no need to do anything
      if (processedReferences.contains(cwlReference)) processedReferences.validParse
      // If the reference is in the bread crumbs it means we circled back to it: fail the pre-processing
      else if (breadCrumbs.contains(cwlReference)) s"Found a circular dependency on $cwlReference".invalidParse
      else {
        // Otherwise let's see if we already have the json for it or if we need to process the file
        val result: Parse[ProcessedJsonAndDependencies] = unProcessedReferences.get(cwlReference) match {
          case Some(unProcessedReferenceJson) =>
            // Found the json in the unprocessed map, no need to reparse the file, just flatten this json
            flattenJson(unProcessedReferenceJson, unProcessedReferences, processedReferences, breadCrumbs)
          case None =>
            // This is the first time we're seeing this reference, we need to parse its file and flatten it
            flattenCwlReference(cwlReference, unProcessedReferences, processedReferences, breadCrumbs)
        }

        result map {
          // Return everything we've got (the previously known "processedReferences" + our new processed reference + everything that was processed to get to it)
          case ProcessedJsonAndDependencies(processed, newReferences) => processedReferences ++ newReferences + (cwlReference -> processed)
        }
      }
    }
  }

  /**
    * Given a Json representing a tool or workflow, flattens it and return the other processed references that were generated.
    * @param saladedJson json to process
    * @param unProcessedReferences references that have been parsed and saladed (we have the json), but not flattened yet
    * @param processedReferences references that are fully flattened
    * @param breadCrumbs list of references that brought us here
    */
  private def flattenJson(saladedJson: Json,
                          unProcessedReferences: UnProcessedReferences,
                          processedReferences: ProcessedReferences,
                          breadCrumbs: Set[CwlReference]): Parse[ProcessedJsonAndDependencies] = {
    val foldFunction = processCwlRunReference(unProcessedReferences, breadCrumbs) _

    // TODO: it would be nice to accumulate failures here somehow (while still folding and be able to re-use
    // successfully processed references, so I don't know if ErrorOr would work)
    findRunReferences(saladedJson.json).foldLeft(processedReferences.validParse)(foldFunction) map { newKnownReferences =>

      // Provide a function to swap the run reference with its json content
      val lookupFunction = {
        json: Json => {
          val fromMap = for {
            asString <- json.asString
            reference <- asString.asReference
            embbeddedJson <- newKnownReferences.get(reference)
          } yield embbeddedJson.json

          fromMap.getOrElse(json)
        }
      }

      ProcessedJsonAndDependencies(root.steps.each.run.json.modify(lookupFunction)(saladedJson.json), newKnownReferences)
    }
  }

  /**
    * Salad and parse a string to Json
    */
  private def saladAndParse(file: BFile): Parse[Json] = for {
    saladed <- saladFunction(file)
    saladedJson <- parseJson(saladed)
  } yield saladedJson
  
  private def parseJson(in: String): Parse[Json] = {
    Parse.checkedParse(io.circe.parser.parse(in).leftMap(error => NonEmptyList.one(error.message)))
  }

  private def parseYaml(in: String): Parse[Json] = {
    Parse.checkedParse(io.circe.yaml.parser.parse(in).leftMap(error => NonEmptyList.one(error.message)))
  }

  /**
    * Given a json, collects all "steps.run" values that are JSON Strings, and convert them to CwlReferences.
    * A saladed JSON is assumed.
    */
  private def findRunReferences(json: Json): List[CwlReference] = {
    json.asArray match {
      case Some(cwls) => cwls.toList.flatMap(findRunReferences)
      case _ => root.steps.each.run.string.getAll(json).flatMap(_.asReference).distinct
    }
  }

  /**
    * Given a json, collect all tools or workflows and map them with their reference id.
    * A saladed JSON is assumed.
    */
  private def mapIdToContent(json: Json): List[(CwlReference, Json)] = {
    json.asArray match {
      case Some(cwls) => cwls.toList.flatMap(mapIdToContent)
      case None => root.id.string.getOption(json).flatMap(_.asReference).map(_ -> json).toList
    }
  }
}