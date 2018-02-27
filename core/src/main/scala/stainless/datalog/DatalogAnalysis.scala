/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package datalog

trait DatalogAnalysis extends AbstractAnalysis {
  val program: StainlessProgram

  //import checker._
  //import context._
  import program._
  import program.trees._

  type Duration = Long
  //type Record = (TerminationGuarantee, Duration)
  val results: Map[FunDef, Duration] //TODO which type here?
  val sources: Set[Identifier] // set of functions that were considered for the analysis

  override val name: String = DatalogComponent.name

  override type Report = DatalogReport

  override def toReport = new DatalogReport(records, sources)

  private lazy val records = results.toSeq map { case (fd, time) =>
    DatalogReport.Record(fd.id, fd.getPos, time, DatalogReport.Embeddable, "fooooo", "barrrr", derivedFrom = fd.source)
  }
}

