package stainless
package datalog

object DatalogTrees {
  class DatalogNode(val name: String,
                    val id: Int,
                    val parentId: Int,
                    val astOrder: Int,
                    val nodeType: String) {
    //NB: for some Nodes order of parameters matters!
    private var params = List[DatalogParameter]()
    def addParameter(name: String, value: String) = {
      params = params :+ new DatalogParameter(name, value)
    }

    override def toString: String = {
      val nameStr = name match {
        case "_" => "_"
        case n => s""""$n""""
      }
      s"""Node(${nameStr}, ${id}, ${parentId}, $astOrder, "$nodeType").\n""" + embedParameters(
        params,
        id,
        0)
    }
  }

  class DatalogParameter(val name: String, val value: String)

  private def embedParameters(params: List[DatalogParameter],
                              nodeId: Int,
                              parameterIndex: Int): String = params match {
    case Nil => ""
    case head :: tail =>
      s"""Parameter("${head.name}", ${nodeId}, $parameterIndex, "${head.value}").\n""" + embedParameters(
        tail,
        nodeId,
        parameterIndex + 1)
  }

}
