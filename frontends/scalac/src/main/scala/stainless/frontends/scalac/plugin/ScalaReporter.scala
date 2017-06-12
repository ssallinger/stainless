/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package frontends.scalac
package plugin

import inox.utils._

import scala.tools.nsc._

import scala.collection.mutable.{Map => MutableMap}

import scala.language.implicitConversions

class ScalaReporter(global: Global, dbgs: Set[inox.DebugSection]) extends inox.Reporter(dbgs) {

  private val sourceCache: MutableMap[java.io.File, scala.reflect.internal.util.SourceFile] = MutableMap.empty

  private implicit def fileToScalaFile(file: java.io.File): scala.reflect.internal.util.SourceFile =
    sourceCache.getOrElseUpdate(file, new scala.reflect.internal.util.BatchSourceFile(io.AbstractFile.getFile(file)))

  private implicit def inoxPosToScalaPos(pos: inox.utils.Position): global.Position = pos match {
    case inox.utils.NoPosition => global.NoPosition
    case inox.utils.RangePosition(sl, sc, sp, el, ec, ep, file) =>
      new scala.reflect.internal.util.RangePosition(file, sp, sp, ep)
    case inox.utils.OffsetPosition(s, c, p, file) =>
      new scala.reflect.internal.util.OffsetPosition(file, p)
  }

  def emit(msg: Message): Unit = msg match {
    case Message(ERROR | FATAL | INTERNAL, pos, msg) => global.reporter.error(pos, msg.toString)
    case Message(WARNING, pos, msg) => global.reporter.warning(pos, msg.toString)
    case Message(DEBUG(_), pos, msg) => global.reporter.echo(pos, "[DEBUG] " + msg)
    case Message(_, pos, msg) => global.reporter.echo(pos, msg.toString)
  }
}
