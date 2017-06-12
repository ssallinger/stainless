/* Copyright 2009-2016 EPFL, Lausanne */

package stainless

import extraction.xlang.trees._

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet, ListBuffer}

import java.util.concurrent.Executors

class Server(ctx: inox.Context) {

  private val pool = Executors.newFixedThreadPool(5)

  private class VerificationTask(program: Program, funs: Seq[Identifier]) extends Runnable {

  }

  private class TerminationTask(program: Program, funs: Seq[Identifier]) extends Runnable {

  }

  // FIXME: remove files once partial program verification is supported
  private val files = new ListBuffer[java.io.File]

  private val units: MutableMap[java.io.File, (UnitDef, Seq[ClassDef], Seq[FunDef])] = MutableMap.empty
  private val classes: MutableMap[Identifier, ClassDef] = MutableMap.empty
  private val functions: MutableMap[Identifier, FunDef] = MutableMap.empty

  def compile(files: Seq[java.io.File]): Unit = files.foreach { f =>
    this.units.get(f).foreach { case (_, classes, functions) =>
      this.classes --= classes.map(_.id)
      this.functions --= functions.map(_.id)
    }

    this.units -= f

    this.files += f
  }

  private val pending: MutableMap[Identifier, MutableSet[Identifier]] = MutableMap.empty

  def register(file: java.io.File, unit: UnitDef, cls: Seq[ClassDef], funs: Seq[FunDef]): Unit = {
    units(file) = (unit, cls, funs)
    classes ++= cls.map(cd => cd.id -> cd)
    functions ++= funs.map(fd => fd.id -> fd)

    // 1. register new dependencies based on class invariants and equality functions
    for (cd <- cls) {
      pending(cd.id) = MutableSet.empty[Identifier] ++ cd.flags.collect {
        case HasADTInvariant(id) => id
        case HasADTEquality(id) => id
      }
    }

    // 2. register new dependencies from functions to other functions and classes
    for (fd <- funs) {
      val deps: MutableSet[Identifier] = MutableSet.empty

      new TreeTraverser {
        override def traverse(e: Expr): Unit = e match {
          case FunctionInvocation(id, _, _) =>
            deps += id
            super.traverse(e)
          case _ => super.traverse(e)
        }

        override def traverse(pat: Pattern): Unit = pat match {
          case UnapplyPattern(_, id, _, _) =>
            deps += id
            super.traverse(pat)
          case _ => super.traverse(pat)
        }

        override def traverse(tpe: Type): Unit = tpe match {
          case ClassType(id, _) =>
            deps += id
            super.traverse(tpe)
          case _ => super.traverse(tpe)
        }
      }.traverse(fd)

      pending(fd.id) = deps
    }

    // 3. compute fixpoint of all (transitive) dependencies
    var changed: Boolean = true
    while (changed) {
      changed = false

      for ((id, deps) <- pending) {
        val newDeps = deps.flatMap(did => pending.get(did).map(_.toSet).getOrElse(Set.empty))
        if (!(newDeps subsetOf deps)) {
          changed = true
          deps ++= newDeps
        }
      }
    }

    // 4. compute all functions ready to be processed
    val ready = for ((id, deps) <- pending if deps subsetOf functions.keySet) yield functions(id)

    // FIXME: enable verification of partial programs

    files -= file
    if (files.isEmpty) {
      val program = new inox.Program {
        val trees: extraction.xlang.trees.type = extraction.xlang.trees
        val ctx = Server.this.ctx
        val symbols = NoSymbols.withClasses(classes.values.toSeq).withFunctions(functions.values.toSeq)
      }

      val exProgram = program.transform(extraction.extractor)

      val toCheck = exProgram.symbols.functions.values.toSeq
        .filterNot(fd => (fd.flags contains "library") || (fd.flags contains "unchecked"))
        .sortBy(_.getPos)
        .map(_.id)

      pool.execute(new VerificationTask(exProgram, toCheck))
      pool.execute(new TerminationTask(exProgram, toCheck))
    }
  }
}
