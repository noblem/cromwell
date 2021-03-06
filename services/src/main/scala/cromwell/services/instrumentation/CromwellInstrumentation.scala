package cromwell.services.instrumentation

import akka.actor.{Actor, ActorRef, Cancellable}
import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory
import cromwell.services.instrumentation.CromwellInstrumentation.{InstrumentationPath, _}
import cromwell.services.instrumentation.InstrumentationService.InstrumentationServiceMessage
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration._
import scala.language.implicitConversions

object CromwellInstrumentation {
  
  val InstrumentationRate = ConfigFactory.load()
    .getConfig("system")
    .as[Option[FiniteDuration]]("instrumentation-rate")
    .getOrElse(5.seconds)

  type InstrumentationPath = NonEmptyList[String]
  
  implicit def stringToNel(str: String): NonEmptyList[String] = NonEmptyList.of(str)
  
  implicit class EnhancedStatsDPath(val path: InstrumentationPath) extends AnyVal {
    def withStatusCodeFailure(code: Option[Int]) = code
      .map(c => path.concat(NonEmptyList.of(c.toString)))
      .getOrElse(path)

    def withThrowable(failure: Throwable, statusCodeExtractor: Throwable => Option[Int]) = {
      path.withStatusCodeFailure(statusCodeExtractor(failure))
    }
  }
}

trait CromwellInstrumentation {
  def serviceRegistryActor: ActorRef
  
  /**
    * Builds a bucket from path.
    * The cromwell bucket prefix is always prepended:
    * cromwell.[prefix].path
    */
  final private def makeBucket(path: InstrumentationPath, prefix: Option[String]): CromwellBucket = {
    CromwellBucket(List("cromwell") ++ prefix, path)
  }
  
  /**
    * Creates an increment message for the given bucket
    */
  private final def incrementMessage(path: InstrumentationPath, prefix: Option[String]): InstrumentationServiceMessage = {
    InstrumentationServiceMessage(CromwellIncrement(makeBucket(path, prefix)))
  }

  /**
    * Increment the counter for the given bucket
    */
  protected final def increment(path: InstrumentationPath, prefix: Option[String] = None): Unit = {
    serviceRegistryActor ! incrementMessage(path, prefix)
  }

  /**
    * Creates a gauge message for the given bucket
    */
  private final def gaugeMessage(path: InstrumentationPath, value: Long, prefix: Option[String]) = {
    InstrumentationServiceMessage(CromwellGauge(makeBucket(path, prefix), value))
  }

  /**
    * Set the bucket to the gauge value
    */
  protected final def sendGauge(path: InstrumentationPath, value: Long, prefix: Option[String] = None): Unit = {
    serviceRegistryActor ! gaugeMessage(path, value, prefix)
  }

  /**
    * Creates a timing message for the given bucket and duration
    */
  private final def timingMessage(path: InstrumentationPath, duration: FiniteDuration, prefix: Option[String]) = {
    InstrumentationServiceMessage(CromwellTiming(makeBucket(path, prefix), duration))
  }

  /**
    * Add a timing information for the given bucket
    */
  protected final def sendTiming(path: InstrumentationPath, duration: FiniteDuration, prefix: Option[String] = None) = {
    serviceRegistryActor ! timingMessage(path, duration, prefix)
  }
}

/**
  * Helper trait to provide a scheduler function that can be used for instrumentation purposes
  */
trait CromwellInstrumentationScheduler { this: Actor =>
  // The system dispatcher should be fine ? Do we need a special one for instrumentation ?
  implicit private val instrumentationEc = context.system.dispatcher

  private var scheduledInstrumentationTimers: Vector[Cancellable] = Vector.empty
  
  def scheduleInstrumentation(f: => Unit) = {
    scheduledInstrumentationTimers = scheduledInstrumentationTimers :+ context.system.scheduler.schedule(InstrumentationRate, InstrumentationRate)(f)
  }

  override def postStop(): Unit = {
    scheduledInstrumentationTimers foreach { _.cancel() }
  }
}