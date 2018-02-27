/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package datalog

import frontend.CallBackWithRegistry
import utils.CheckFilter

import io.circe.Json

/** Callback for datalog */
final class DatalogCallBack(override val context: inox.Context) extends CallBackWithRegistry with CheckFilter {
  private implicit val debugSection = DebugSectionDatalog

  override type Report = DatalogReport 
  //override type Report = NoReport
  override val cacheSubDirectory = DatalogComponent.name
  override def parseReportCache(json: Json): Report = DatalogReport.parse(json)

  override def onCycleBegin(): Unit = DatalogComponent.onCycleBegin()

  override def solve(program: Program { val trees: extraction.xlang.trees.type }): Report = {
    context.reporter.debug(
      s"Emitting datalog embedding of the following program: " +
      "\n\tfunctions = [" + (program.symbols.functions.keySet mkString ", ") + "]" +
      "\n\tclasses   = [" + (program.symbols.classes.keySet mkString ", ") + "]"
    )
    
    DatalogComponent(program, context).toReport
  }

}
