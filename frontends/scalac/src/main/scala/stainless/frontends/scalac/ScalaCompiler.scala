/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package frontends.scalac

import extraction.xlang.{trees => xt}
import scala.tools.nsc.{Global, Settings => NSCSettings, CompilerCommand, SubComponent}
import scala.reflect.internal.Positions

import scala.collection.mutable.ListBuffer

class ScalaCompiler(settings: NSCSettings, ctx: inox.Context)
  extends Global(settings, new SimpleReporter(settings, ctx.reporter))
     with Positions {

  object stainlessExtraction extends {
    val global: ScalaCompiler.this.type = ScalaCompiler.this
    val ctx = ScalaCompiler.this.ctx
  } with SubComponent with CodeExtraction {
    val phaseName = "stainless"
    val runsAfter = List[String]("refchecks")
    val runsRightAfter = None

    var units: List[CompilationUnit] = Nil

    def extractProgram = try {
      val unitsAcc     = new ListBuffer[xt.UnitDef]
      val classesAcc   = new ListBuffer[xt.ClassDef]
      val functionsAcc = new ListBuffer[xt.FunDef]

      for (u <- units) {
        val (unit, classes, functions) = extractUnit(u)

        unitsAcc += unit
        classesAcc ++= classes
        functionsAcc ++= functions
      }

      val program: inox.Program { val trees: xt.type } = new inox.Program {
        val trees: xt.type = xt
        val ctx = ScalaCompiler.this.ctx
        val symbols = xt.NoSymbols.withClasses(classesAcc).withFunctions(functionsAcc)
      }

      (unitsAcc.toList, program)
    } catch {
      case e: ImpureCodeEncounteredException =>
        reporter.debug("Extraction failed because of:")
        reporter.debug(e.pos, e.getMessage, e)
        throw new inox.FatalError(e.getMessage)
    }

    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

    class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
      def apply(unit: CompilationUnit): Unit = units ::= unit
    }
  }

  override protected def computeInternalPhases() : Unit = {
    val phs = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      patmat                  -> "translate match expressions",
      superAccessors          -> "add super accessors in traits and nested classes",
      extensionMethods        -> "add extension methods for inline classes",
      pickler                 -> "serialize symbol tables",
      refChecks               -> "reference/override checking, translate nested objects",
      stainlessExtraction     -> "extracts stainless trees out of scala trees"
    )
    phs foreach { phasesSet += _._1 }
  }

  class Run extends super.Run {
    override def progress(current: Int, total: Int) {
      ctx.reporter.onCompilerProgress(current, total)
    }
  }
}

object ScalaCompiler {
  def apply(ctx: inox.Context, compilerOpts: List[String]): (
    List[xt.UnitDef],
    Program { val trees: xt.type }
  ) = {
    val timer = ctx.timers.frontend.start()

    val settings = new NSCSettings

    def getFiles(path: String): Option[Array[String]] =
      scala.util.Try(new java.io.File(path).listFiles().map(_.getAbsolutePath)).toOption

    val scalaLib = Option(scala.Predef.getClass.getProtectionDomain.getCodeSource).map {
      _.getLocation.getPath
    }.orElse(for {
      // we are in an Eclipse environment, look in plugins for the Scala lib
      eclipseHome <- Option(System.getenv("ECLIPSE_HOME"))
      pluginsHome = eclipseHome + "/plugins"
      plugins <- getFiles(pluginsHome)
      path <- plugins.find(_ contains "scala-library")
    } yield path).getOrElse(ctx.reporter.fatalError(
      "No Scala library found. If you are working in Eclipse, " +
      "make sure you set the ECLIPSE_HOME environment variable to your Eclipse installation home directory."
    ))

    settings.classpath.value = scalaLib
    settings.usejavacp.value = false
    settings.deprecation.value = true
    settings.Yrangepos.value = true
    settings.skip.value = List("patmat")

    val command = new CompilerCommand(compilerOpts, settings) {
      override val cmdName = "stainless"
    }

    if (command.ok) {
      val compiler = new ScalaCompiler(settings, ctx)
      val run = new compiler.Run
      run.compile(command.files)
      timer.stop()

      val result = compiler.stainlessExtraction.extractProgram
      if (ctx.reporter.errorCount > 0) ctx.reporter.fatalError("There were some errors.")
      else result
    } else {
      ctx.reporter.fatalError("No input program.")
    }
  }
}
