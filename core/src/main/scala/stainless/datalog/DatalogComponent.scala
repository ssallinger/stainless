/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package datalog
import extraction.xlang.{trees => xt}
import scala.language.existentials
import java.io._

object DebugSectionDatalog extends inox.DebugSection("datalog")

object DatalogComponent extends SimpleComponent {
  override val name = "datalog"
  override val description = "Emit datalog embedding"

  override val trees: stainless.trees.type = stainless.trees //TODO which type of trees do we need

  override type Analysis = DatalogAnalysis

  override val lowering = inox.ast.SymbolTransformer(new ast.TreeTransformer {
    val s: extraction.trees.type = extraction.trees
    val t: extraction.trees.type = extraction.trees
  })

  implicit val debugSection = DebugSectionDatalog
  
  private def embed(funs: Seq[Identifier], p: StainlessProgram, ctx: inox.Context) : Unit = {
    
    val embeddings = DatalogEmbedder.embed(p)
    
    val file = new File("datalogEmbedding.flix")
    val bw = new BufferedWriter(new FileWriter(file))
    embeddings.foreach(bw.write)
    bw.close()
  
  }
  
  override def apply(funs: Seq[Identifier], p: StainlessProgram, ctx: inox.Context): DatalogAnalysis = {
    import p._
    import p.trees._
    import p.symbols._
    import ctx._
    
    //val toEmbed = filter(p, ctx)(funs)
    
    embed(funs, p, ctx)
    
    //return dummy result, ignore for now (interesting result, i.e., the embedding, is written to file)
    val dummy: Long = 0

    val res = p.symbols.functions.values map(fd => fd -> dummy) 

    //val res = toEmbed map { fd => fd -> dummy 
      //val (time, tryStatus) = timers.termination.runAndGetTime { c.terminates(fd) }
      //tryStatus match {
        //case Success(status) => fd -> (status, time)
        //case Failure(e) => reporter.internalError(e)
      //}
    //}
    
    new DatalogAnalysis {
      override val program: p.type = p
      override val sources = funs.toSet
      override val results : Map[FunDef, Long] = res.toMap
    }
  }
  
}
