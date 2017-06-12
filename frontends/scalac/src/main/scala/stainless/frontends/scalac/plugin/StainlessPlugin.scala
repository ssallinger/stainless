/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package frontends.scalac
package plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins._

class StainlessPlugin(val global: Global) extends Plugin {
  import global._

  val name = "stainless"
  val description = "formal verification for Scala"

  val components: List[PluginComponent] = List(component)

  private lazy val ctx = mkContext(global.currentRun.units.toList.map(_.source.file.file))
  private lazy val server = new Server(ctx)

  private object component extends {
    val global: StainlessPlugin.this.global.type = StainlessPlugin.this.global
    val ctx = StainlessPlugin.this.ctx
  } with PluginComponent with CodeExtraction {
    val runsAfter = List("refchecks")
    val phaseName = StainlessPlugin.this.name

    def newPhase(prev: Phase) = new StdPhase(prev) {
      override def name = StainlessPlugin.this.name

      def apply(u: CompilationUnit): Unit = {
        val (unit, classes, functions) = extractUnit(u)
        server.register(u.source.file.file, unit, classes, functions)
      }
    }
  }

  private var inoxOptions: inox.Options = inox.Options.empty

  private def mkContext(files: Seq[java.io.File]): inox.Context = {
    val reporter = new ScalaReporter(global, inoxOptions.options.collectFirst {
      case inox.OptionValue(stainless.Main.optDebug, sections) => sections.asInstanceOf[Set[inox.DebugSection]]
    }.getOrElse(Set[inox.DebugSection]()))

    reporter.whenDebug(inox.DebugSectionOptions) { debug =>
      debug("Options considered:")
      for (io <- inoxOptions.options) debug(io.toString)
    }

    inox.Context(
      reporter = reporter,
      options = inoxOptions + stainless.Main.optFiles(files),
      interruptManager = new inox.utils.InterruptManager(reporter)
    )
  }

  override def processOptions(options: List[String], error: String => Unit): Unit = {
    val reporter = new ScalaReporter(global, Set())

    val inoxOptionss: Seq[Option[inox.OptionValue[_]]] = options.map { opt =>
      inox.OptionsHelpers.nameValue(opt) match {
        case Some((name, value)) =>
          stainless.Main.options.keys.find(_.name == name) match {
            case Some(df) =>
              scala.util.Try(df.parse(value)(reporter)).toOption

            case None =>
              reporter.error(s"Unknown option: $name.")
              None
          }

        case None =>
          reporter.error(s"Malformed option $opt.")
          None
      }
    }

    this.inoxOptions = inox.Options(inoxOptionss.flatten)
  }
}
