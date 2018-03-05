/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package datalog

import inox.utils.ASCIIHelpers.{Cell, Row}
import stainless.utils.JsonConvertions._

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

import scala.util.{Right, Left}

object DatalogReport {

  sealed abstract class Status {
    //def isUnknown = this == Unknown
    def isEmbeddable = this == Embeddable
    def isNonEmbeddable = this == NonEmbeddable
  }

  case object Embeddable extends Status
  case object NonEmbeddable extends Status

  implicit val statusDecoder: Decoder[Status] = deriveDecoder
  implicit val statusEncoder: Encoder[Status] = deriveEncoder

  case class Record(
      id: Identifier,
      pos: inox.utils.Position,
      time: Long,
      status: Status,
      verdict: String,
      kind: String,
      derivedFrom: Identifier
  ) extends AbstractReportHelper.Record

  implicit val recordDecoder: Decoder[Record] = deriveDecoder
  implicit val recordEncoder: Encoder[Record] = deriveEncoder

  def parse(json: Json) = json.as[(Seq[Record], Set[Identifier])] match {
    case Right((records, sources)) => new DatalogReport(records, sources)
    case Left(error) => throw error
  }

}

// Variant of the report without the checker, where all the data is mapped to text
class DatalogReport(val results: Seq[DatalogReport.Record],
                    val sources: Set[Identifier])
    extends BuildableAbstractReport[DatalogReport.Record, DatalogReport] {
  import DatalogReport._

  override val encoder = recordEncoder

  override def build(results: Seq[Record], sources: Set[Identifier]) =
    new DatalogReport(results, sources)

  override val name: String = DatalogComponent.name

  lazy val totalValid = results count { _.status.isEmbeddable }
  lazy val totalValidFromCache = 0
  lazy val totalInvalid = results count { _.status.isNonEmbeddable }
  lazy val totalUnknown = 0
  lazy val totalTime = (results map { _.time }).sum

  override lazy val annotatedRows = results map {
    case Record(id, pos, time, status, verdict, kind, _) =>
      val level = levelOf(status)
      val symbol = if (status.isEmbeddable) "\u2713" else "\u2717"
      val extra = Seq(s"$symbol $verdict")

      RecordRow(id, pos, level, extra, time)
  }

  private def levelOf(status: Status) = status match {
    case Embeddable => Level.Normal
    //case Unknown => Level.Warning
    case NonEmbeddable => Level.Error
  }

  override lazy val stats =
    ReportStats(results.size,
                totalTime,
                totalValid,
                totalValidFromCache,
                totalInvalid,
                totalUnknown)

}
