package stainless
package datalog
import extraction.xlang.{trees => xt}
import scala.language.existentials
import DatalogTrees._

trait DatalogEmbedder {

  val program: Program

  import program._
  import program.symbols._
  import program.trees._

  //we directly construct list of nodes for embedding (instead of tree structure)
  private var nodes = List[DatalogNode]()
  private var newNodeId = 0
  private def getNewNodeId() = {
    val nextId = newNodeId
    newNodeId = newNodeId + 1
    nextId
  }

  private def emitNode(name: String,
                       parentId: Int,
                       astOrder: Int,
                       nodeType: String,
                       position: String) = {
    val newId = getNewNodeId()
    val newNode = new DatalogNode(name, newId, parentId, astOrder, nodeType)
    newNode.addParameter("position", position)
    addNode(newNode)
    newNode
  }

  private def addNode(n: DatalogNode): Unit = {
    nodes = nodes :+ n
  }

  def emit(): List[DatalogNode] = {
    for (fun <- program.symbols.functions.values) {
      emit(fun, -1, -1)
    }

    for (adt <- program.symbols.adts.values) {
      emit(adt, -1, -1)
    }
    nodes
  }

  def emit(vd: ValDef, parentId: Int, astOrder: Int): Unit = {
    val node = emitNode(vd.id.name, parentId, astOrder, "ValDef", vd.getPos.toString)
    node.addParameter("id", vd.id.globalId.toString)
    emit(vd.toVariable, node.id, 0)
  }

  def emit(tpd: TypeParameterDef, parentId: Int, astOrder: Int): Unit = {
    val node = emitNode(tpd.id.name, parentId, astOrder, "TypeParameterDef", tpd.getPos.toString)
    node.addParameter("id", tpd.id.globalId.toString)
    emit(tpd.tp, node.id, 0) //emit corresponding type
  }
  
  def emit(e: Expr, parentId: Int, astOrder: Int): Unit = {
    //TODO define predicate that says that all of those are expressions
    e match {
      case Assume(pred, body) =>
        emitBinaryOp("Expr(Assume)", pred, body, parentId, astOrder, e.getPos.toString)
      case Variable(id, tpe, _) =>
        val node = emitNode("", parentId, astOrder, "Expr(Variable)", e.getPos.toString)
        node.addParameter("variableId", id.globalId.toString)
        emit(tpe, node.id, 0)
      case Let(vd, value, body) =>
        val node = emitNode("", parentId, astOrder, "Expr(Let)", e.getPos.toString)
        emit(vd, node.id, 0)
        emit(value, node.id, 1)
        emit(body, node.id, 2)
      case Application(callee, args) =>
        val node = emitNode("", parentId, astOrder, "Expr(Application)", e.getPos.toString)
        emit(callee, node.id, 0)
        var counter = 1
        for (arg <- args) {
          emit(arg, node.id, counter)
          counter = counter + 1
        }
      case Lambda(args, body) =>
        val node = emitNode("", parentId, astOrder, "Expr(Lambda)", e.getPos.toString)
        emit(body, node.id, 0)
        var counter = 1 //change order to be able to distinguish from astOrder
        for (arg <- args) {
          emit(arg, node.id, counter)
          counter = counter + 1
        }
      case Forall(args, body) =>
        val node = emitNode("", parentId, astOrder, "Expr(Forall)", e.getPos.toString)
        emit(body, node.id, 0)
        var counter = 1
        for (arg <- args) {
          emit(arg, node.id, counter)
          counter = counter + 1
        }
      case Choose(res, pred) =>
        val node = emitNode("", parentId, astOrder, "Expr(Choose)", e.getPos.toString)
        emit(res, node.id, 0)
        emit(pred, node.id, 1)
      case FunctionInvocation(id, tps, args) =>
        val node =
          emitNode("", parentId, astOrder, "Expr(FunctionInvocation)", e.getPos.toString)
        node.addParameter("invokedFunction", id.globalId.toString)
        var counter = 0
        for (tp <- tps) {
          emit(tp, node.id, counter)
          counter = counter + 1
        }
        for (arg <- args) {
          emit(arg, node.id, counter)
          counter = counter + 1
        }
      case IfExpr(cond, thenn, elze) =>
        emitTernaryOp("Expr(IfExpr)", cond, thenn, elze, parentId, astOrder, e.getPos.toString)
      case CharLiteral(value) =>
        emitLiteral("Expr(CharLiteral)", value.toString, parentId, astOrder, e.getPos.toString)
      case bv @ BVLiteral(value, size) =>
        val node = emitNode("", parentId, astOrder, "Expr(BVLiteral)", e.getPos.toString)
        node.addParameter("value", bv.toBigInt.toString)
        node.addParameter("size", size.toString)
      case IntegerLiteral(value) =>
        emitLiteral("Expr(IntegerLiteral)", value.toString, parentId, astOrder, e.getPos.toString)
      case FractionLiteral(num, denom) =>
        val node = emitNode("", parentId, astOrder, "Expr(FractionLiteral)", e.getPos.toString)
        node.addParameter("numerator", num.toString)
        node.addParameter("denominator", denom.toString)
      case BooleanLiteral(value) =>
        emitLiteral("Expr(BooleanLiteral)", value.toString, parentId, astOrder, e.getPos.toString) //TODO does LatMap expect false or False?
      case UnitLiteral() =>
        emitNode("", parentId, astOrder, "Expr(UnitLiteral)", e.getPos.toString)
      case StringLiteral(value) =>
        //handle escape sequences: add extra backslash in front of every backslash
        var valueStr = value.replaceAll("\\\\", "\\\\\\\\")
        for (ch <- List('t', 'n', 'f', 'r', '"', '\''))
          valueStr = valueStr.replaceAll("\\" + ch, "\\\\" + ch)
        emitLiteral("Expr(StringLiteral)", valueStr, parentId, astOrder, e.getPos.toString)
      case GenericValue(tp, id) => //TODO what does id represent? Why not "identifier"?
        val node = emitNode("", parentId, astOrder, "Expr(GenericValue)", e.getPos.toString)
        node.addParameter("id", id.toString)
        emit(tp, node.id, 0)
      case ADT(adt, args) =>
        val node = emitNode("", parentId, astOrder, "Expr(ADT)", e.getPos.toString)
        node.addParameter("adtType", adt.id.globalId.toString)
        var counter = 0
        for (arg <- args) {
          emit(arg, node.id, counter)
          counter = counter + 1
        }
      case IsInstanceOf(expr, tpe) =>
        val node = emitNode("", parentId, astOrder, "Expr(IsInstanceOf)", e.getPos.toString)
        emit(expr, node.id, 0)
        emit(tpe, node.id, 1)
      case AsInstanceOf(expr, tpe) =>
        val node = emitNode("", parentId, astOrder, "Expr(AsInstanceOf)", e.getPos.toString)
        emit(expr, node.id, 0)
        emit(tpe, node.id, 1)
      case ADTSelector(adt, selector) =>
        val node = emitNode("", parentId, astOrder, "Expr(ADTSelector)", e.getPos.toString)
        node.addParameter("selector", selector.globalId.toString)
        emit(adt, node.id, 0)
      case Equals(lhs, rhs) =>
        emitBinaryOp("Expr(Equals)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case And(exprs) =>
        emitOp("Expr(And)", exprs, parentId, astOrder, e.getPos.toString)
      case Or(exprs) =>
        emitOp("Expr(Or)", exprs, parentId, astOrder, e.getPos.toString)
      case Implies(lhs, rhs) =>
        emitBinaryOp("Expr(Implies)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case Not(expr) =>
        emitUnaryOp("Expr(Not)", expr, parentId, astOrder, e.getPos.toString)
      case StringConcat(lhs, rhs) =>
        emitBinaryOp("Expr(StringConcat)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case SubString(expr, start, end) =>
        emitTernaryOp("Expr(SubString)", expr, start, end, parentId, astOrder, e.getPos.toString)
      case StringLength(expr) =>
        emitUnaryOp("Expr(StringLength)", expr, parentId, astOrder, e.getPos.toString)
      case Plus(lhs, rhs) =>
        emitBinaryOp("Expr(Plus)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case Minus(lhs, rhs) =>
        emitBinaryOp("Expr(Minus)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case UMinus(expr) =>
        emitUnaryOp("Expr(UMinus)", expr, parentId, astOrder, e.getPos.toString)
      case Times(lhs, rhs) =>
        emitBinaryOp("Expr(Times)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case Division(lhs, rhs) =>
        emitBinaryOp("Expr(Divide)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case Remainder(lhs, rhs) =>
        emitBinaryOp("Expr(Remainder)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case Modulo(lhs, rhs) =>
        emitBinaryOp("Expr(Modulo)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case LessThan(lhs, rhs) =>
        emitBinaryOp("Expr(LessThan)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case GreaterThan(lhs, rhs) =>
        emitBinaryOp("Expr(GreaterThan)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case LessEquals(lhs, rhs) =>
        emitBinaryOp("Expr(LessEquals)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case GreaterEquals(lhs, rhs) =>
        emitBinaryOp("Expr(GreaterEquals)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case BVNot(expr) =>
        emitUnaryOp("Expr(BVNot)", expr, parentId, astOrder, e.getPos.toString)
      case BVOr(lhs, rhs) =>
        emitBinaryOp("Expr(BVOr)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case BVAnd(lhs, rhs) =>
        emitBinaryOp("Expr(BVAnd)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case BVXor(lhs, rhs) =>
        emitBinaryOp("Expr(BVXor)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case BVShiftLeft(lhs, rhs) =>
        emitBinaryOp("Expr(BVShiftLeft)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case BVAShiftRight(lhs, rhs) =>
        emitBinaryOp("Expr(BVAShiftRight)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case BVLShiftRight(lhs, rhs) =>
        emitBinaryOp("Expr(BVLShiftRight)", lhs, rhs, parentId, astOrder, e.getPos.toString)
      case BVNarrowingCast(expr, newType) =>
        val node = emitNode("", parentId, astOrder, "Expr(BVNarrowingCast)", e.getPos.toString)
        emit(expr, node.id, 0)
        emit(newType, node.id, 1)
      case BVWideningCast(expr, newType) =>
        val node = emitNode("", parentId, astOrder, "Expr(BVWideningCast)", e.getPos.toString)
        emit(expr, node.id, 0)
        emit(newType, node.id, 1)
      case Tuple(exprs) =>
        emitOp("Expr(ExpressionType.Tuple)", exprs, parentId, astOrder, e.getPos.toString)
      case TupleSelect(tuple, index) =>
        val node = emitNode("", parentId, astOrder, "Expr(TupleSelect)", e.getPos.toString)
        node.addParameter("Expr(index)", index.toString)
        emit(tuple, node.id, 0)
      case FiniteSet(elements, base) =>
        val node = emitNode("", parentId, astOrder, "Expr(FiniteSet)", e.getPos.toString)
        var counter = 0
        for (el <- elements) {
          emit(el, node.id, counter)
          counter = counter + 1
        }
        emit(base, node.id, counter)
      case SetAdd(set, elem) =>
        emitBinaryOp("Expr(SetAdd)", set, elem, parentId, astOrder, e.getPos.toString)
      case ElementOfSet(elem, set) =>
        emitBinaryOp("Expr(ElementOfSet)", elem, set, parentId, astOrder, e.getPos.toString)
      case SubsetOf(set1, set2) =>
        emitBinaryOp("Expr(SubsetOf)", set1, set2, parentId, astOrder, e.getPos.toString)
      case SetIntersection(set1, set2) =>
        emitBinaryOp("Expr(SetIntersection)", set1, set2, parentId, astOrder, e.getPos.toString)
      case SetUnion(set1, set2) =>
        emitBinaryOp("Expr(SetUnion)", set1, set2, parentId, astOrder, e.getPos.toString)
      case SetDifference(set1, set2) =>
        emitBinaryOp("Expr(SetDifference)", set1, set2, parentId, astOrder, e.getPos.toString)
      case FiniteBag(elements, base) => //TODO why are there tuples in bag?
        val node = emitNode("", parentId, astOrder, "Expr(FiniteBag)", e.getPos.toString)
        var counter = 0
        for ((el1, el2) <- elements) {
          emit(el1, node.id, counter)
          counter = counter + 1
          emit(el2, node.id, counter)
          counter = counter + 1
        }
        emit(base, node.id, counter)
      case BagAdd(bag, elem) =>
        emitBinaryOp("Expr(BagAdd)", bag, elem, parentId, astOrder, e.getPos.toString)
      case MultiplicityInBag(bag, elem) =>
        emitBinaryOp("Expr(MultiplicityInBag)", bag, elem, parentId, astOrder, e.getPos.toString)
      case BagIntersection(bag1, bag2) =>
        emitBinaryOp("BagIntersection)", bag1, bag2, parentId, astOrder, e.getPos.toString)
      case BagUnion(bag1, bag2) =>
        emitBinaryOp("Expr(BagUnion)", bag1, bag2, parentId, astOrder, e.getPos.toString)
      case BagDifference(bag1, bag2) =>
        emitBinaryOp("Expr(BagDifference)", bag1, bag2, parentId, astOrder, e.getPos.toString)
      case FiniteMap(pairs, default, keyType, valueType) =>
        val node = emitNode("", parentId, astOrder, "Expr(FiniteMap)", e.getPos.toString)
        emit(default, node.id, 0)
        emit(keyType, node.id, 1)
        emit(valueType, node.id, 2)
        var counter = 3
        for ((el1, el2) <- pairs) {
          emit(el1, node.id, counter)
          counter = counter + 1
          emit(el2, node.id, counter)
          counter = counter + 1
        }

      case MapApply(map, key) =>
        emitBinaryOp("Expr(MapApply)", map, key, parentId, astOrder, e.getPos.toString)
      case MapUpdated(map, key, value) =>
        emitTernaryOp("Expr(MapUpdated)", map, key, value, parentId, astOrder, e.getPos.toString)
      case NoTree(tpe) =>
        val node = emitNode("", parentId, astOrder, "Expr(NoTree)", e.getPos.toString)
        emit(tpe, node.id, 0)
      case Error(tpe, description) =>
        val node = emitNode("", parentId, astOrder, "Expr(Error)", e.getPos.toString)
        node.addParameter("description", description)
        emit(tpe, node.id, 0)
      case Require(pred, body) =>
        emitBinaryOp("Expr(Require)", pred, body, parentId, astOrder, e.getPos.toString)
      case Annotated(body, flags) =>
        //TODO do flags have to be stored?
        emitUnaryOp("Expr(Annotated)", body, parentId, astOrder, e.getPos.toString)
      case Ensuring(body, pred) =>
        emitBinaryOp("Expr(Ensuring)", body, pred, parentId, astOrder, e.getPos.toString)
      case Assert(pred, error, body) =>
        val node = emitNode("", parentId, astOrder, "Expr(Assert)", e.getPos.toString)
        node.addParameter("error", error.getOrElse(""))
        emit(pred, node.id, 0)
        emit(body, node.id, 1)
      case MatchExpr(scrutinee, cases) =>
        val node = emitNode("", parentId, astOrder, "Expr(MatchExpr)", e.getPos.toString)
        emit(scrutinee, node.id, 0)
        var counter = 1
        for (c <- cases) {
          emit(c, node.id, counter)
          counter = counter + 1
        }
      case FiniteArray(elems, base) =>
        val node = emitNode("", parentId, astOrder, "Expr(FiniteArray)", e.getPos.toString)
        emit(base, node.id, 0)
        var counter = 0
        for (el <- elems) {
          emit(el, node.id, counter)
          counter = counter + 1
        }
      case LargeArray(elems, default, size, base) =>
        val node = emitNode("", parentId, astOrder, "Expr(LargeArray)", e.getPos.toString)
        emit(default, node.id, 0)
        emit(size, node.id, 1)
        emit(base, node.id, 2)
        var counter = 3
        for ((index, value) <- elems) {
          emit(value, node.id, counter)
          //TODO is this way of embedding it useful for accessing values by index in datalog?
          node.addParameter("index", index.toString)
          node.addParameter("value", counter.toString) //value = astOrder of expr. child that corresponds to value
          counter = counter + 1
        }
      case ArraySelect(array, index) =>
        emitBinaryOp("Expr(ArraySelect)", array, index, parentId, astOrder, e.getPos.toString)
      case ArrayUpdated(array, index, value) =>
        emitTernaryOp("Expr(ArrayUpdated)", array, index, value, parentId, astOrder, e.getPos.toString)
      case ArrayLength(array) =>
        emitUnaryOp("Expr(ArrayLength)", array, parentId, astOrder, e.getPos.toString)
      //case _ => ctx.reporter.info("Warning: Some expression is not embedded!")

    }

  }

  private def emitOp(name: String,
                     exprs: Seq[Expr],
                     parentId: Int,
                     astOrder: Int,
                     position: String) = {
    val node = emitNode("", parentId, astOrder, name, position)
    var counter = 0
    for (expr <- exprs) {
      emit(expr, node.id, counter)
      counter = counter + 1
    }
  }

  private def emitTernaryOp(name: String,
                            first: Expr,
                            second: Expr,
                            third: Expr,
                            parentId: Int,
                            astOrder: Int,
                            position: String) = {
    val node = emitNode("", parentId, astOrder, name, position)
    emit(first, node.id, 0)
    emit(second, node.id, 1)
    emit(third, node.id, 1)
  }

  private def emitBinaryOp(name: String,
                           lhs: Expr,
                           rhs: Expr,
                           parentId: Int,
                           astOrder: Int,
                           position: String) = {
    val node = emitNode("", parentId, astOrder, name, position)
    emit(lhs, node.id, 0)
    emit(rhs, node.id, 1)
  }

  private def emitUnaryOp(name: String,
                          expr: Expr,
                          parentId: Int,
                          astOrder: Int,
                          position: String) = {
    val node = emitNode("", parentId, astOrder, name, position)
    emit(expr, node.id, 0)
  }

  private def emitLiteral(name: String,
                          value: String,
                          parentId: Int,
                          astOrder: Int,
                          position:String) = {
    val node = emitNode("", parentId, astOrder, name, position)
    node.addParameter("value", value)
  }

  def emit(m: MatchCase, parentId: Int, astOrder: Int): Unit = {
    val node = emitNode("", parentId, astOrder, "MatchCase", m.getPos.toString)
    emit(m.pattern, node.id, 0)
    emit(m.rhs, node.id, 1)
    if (!m.optGuard.isEmpty)
      emit(m.optGuard.get, node.id, 2)
  }

  def emit(p: Pattern, parentId: Int, astOrder: Int): Unit = p match {
    case InstanceOfPattern(binder, tpe) =>
      val node = emitNode("", parentId, astOrder, "Pattern(InstanceOfPattern)", p.getPos.toString)
      emit(tpe, node.id, 0)
      if (!binder.isEmpty)
        emit(binder.get, node.id, 1)
    case WildcardPattern(binder) =>
      val node = emitNode("", parentId, astOrder, "Pattern(WildcardPattern)", p.getPos.toString)
      if (!binder.isEmpty)
        emit(binder.get, node.id, 0)
    case ADTPattern(binder, tpe, subPatterns) =>
      val node = emitNode("", parentId, astOrder, "Pattern(ADTPattern)", p.getPos.toString)
      emit(tpe, node.id, 0)
      var counter = 0
      for (subP <- subPatterns) {
        emit(subP, node.id, counter)
        counter = counter + 1
      }
      if (!binder.isEmpty)
        emit(binder.get, node.id, counter)
    case TuplePattern(binder, subPatterns) =>
      val node = emitNode("", parentId, astOrder, "Pattern(TuplePattern)", p.getPos.toString)
      var counter = 0
      for (subP <- subPatterns) {
        emit(subP, node.id, counter)
        counter = counter + 1
      }
      if (!binder.isEmpty)
        emit(binder.get, node.id, counter)
    case LiteralPattern(binder, lit) =>
      val node = emitNode("", parentId, astOrder, "Pattern(LiteralPattern)", p.getPos.toString)
      emit(lit, node.id, 0)
      if (!binder.isEmpty)
        emit(binder.get, node.id, 1)
    case UnapplyPattern(binder, id, tps, subPatterns) => //TODO what is this about? what does identifier represent?
      val node = emitNode("", parentId, astOrder, "Pattern(UnapplyPattern)", p.getPos.toString)
      node.addParameter("identifier", id.globalId.toString)
      var counter = 0
      for (tpe <- tps) {
        emit(tpe, node.id, counter)
        counter = counter + 1
      }
      for (subP <- subPatterns) {
        emit(subP, node.id, counter)
        counter = counter + 1
      }
      if (!binder.isEmpty)
        emit(binder.get, node.id, counter)

  }

  def emit(tpe: Type, parentId: Int, astOrder: Int): Unit = {
    //TODO define predicate that says that all of those are types
    tpe match {
      case Untyped =>
        emitNode("", parentId, astOrder, "Type(Untyped)", tpe.getPos.toString)
      case BooleanType() =>
        emitNode("", parentId, astOrder, "Type(Boolean)", tpe.getPos.toString)
      case UnitType() =>
        emitNode("", parentId, astOrder, "Type(Unit)", tpe.getPos.toString)
      case CharType() =>
        emitNode("", parentId, astOrder, "Type(Char)", tpe.getPos.toString)
      case IntegerType() =>
        emitNode("", parentId, astOrder, "Type(BigInt)", tpe.getPos.toString)
      case RealType() =>
        emitNode("", parentId, astOrder, "Type(Real)", tpe.getPos.toString)
      case StringType() =>
        emitNode("", parentId, astOrder, "Type(String)", tpe.getPos.toString)

      case BVType(size) =>
        val name = size match {
          case 8  => "Byte"
          case 16 => "Short"
          case 32 => "Int"
          case 64 => "Long"
          case _ => """BVType($size)"""
        }
        val node = emitNode("", parentId, astOrder, "Type(" + name + ")", tpe.getPos.toString)
        node.addParameter("size", size.toString)

      case typePar @ TypeParameter(id, _) =>
        val node =
          emitNode(id.name, parentId, astOrder, s"""Type(TypeParameter("${id.name}"))""", tpe.getPos.toString)
        node.addParameter("id", id.globalId.toString)
        if (typePar.isCovariant)
          node.addParameter("variance", "covariant")
        if (typePar.isContravariant)
          node.addParameter("variance", "contravariant")
      //TODO add names of BaseType in the following cases
      case TupleType(bases) =>
        val node = emitNode("", parentId, astOrder, "Type(TypeName.Tuple)", tpe.getPos.toString)
        var counter = 0
        for (b <- bases) {
          emit(b, node.id, counter)
          counter = counter + 1
        }

      case SetType(base) =>
        val node = emitNode("", parentId, astOrder, "Type(SetType)", tpe.getPos.toString)
        emit(base, node.id, 0)
      case BagType(base) =>
        val node = emitNode("", parentId, astOrder, "Type(Bag)", tpe.getPos.toString)
        emit(base, node.id, 0)
      case MapType(from, to) =>
        val node = emitNode("", parentId, astOrder, "Type(MapType)", tpe.getPos.toString)
        emit(to, node.id, 0) //probably counter intuitve order but want consistency with function type
        emit(from, node.id, 1)
      case FunctionType(from, to) =>
        val node = emitNode("", parentId, astOrder, "Type(Function)", tpe.getPos.toString)
        emit(to, node.id, 0)
        var counter = 1
        for (f <- from) {
          emit(f, node.id, counter)
          counter = counter + 1
        }

      case ADTType(id, tps) =>
        val node = emitNode(id.name, parentId, astOrder, s"""Type(ADTType("${id.name}"))""", tpe.getPos.toString)
        node.addParameter("id", id.globalId.toString)
        var counter = 0
        for (t <- tps) {
          emit(t, node.id, counter)
          counter = counter + 1
        }
      case ArrayType(base) =>
        val node = emitNode("", parentId, astOrder, "Type(Array)", tpe.getPos.toString)
        emit(base, node.id, 0)
    }
  }

  def emit(fd: FunDef, parentId: Int, astOrder: Int): Unit = {
    val node = emitNode(fd.id.name, parentId, astOrder, "FunDef", fd.getPos.toString) //TODO what is parent of fundef? and what is the astorder (not that it should matter here)?
    node.addParameter("id", fd.id.globalId.toString)
    var counter = 0
    for (param <- fd.params) {
      emit(param, node.id, counter)
      counter = counter + 1
    }

    
    for (tparam <- fd.tparams) {
      emit(tparam, node.id, counter)
      counter = counter + 1
    }

    for (param <- fd.params) {
      emit(param, node.id, counter)
      counter = counter + 1
    }

    emit(fd.fullBody, node.id, counter)
    counter = counter + 1

    emit(fd.returnType, node.id, counter)
    counter = counter + 1

    for (callee <- program.symbols.callees(fd.id)) {
      node.addParameter("calls", callee.globalId.toString)
    }

  }

  def emit(adt: ADTDefinition, parentId: Int, astOrder: Int): Unit = {
    adt match {
      case sort: ADTSort =>
        val node = emitNode(adt.id.name, parentId, astOrder, "ADTSort", adt.getPos.toString)
        node.addParameter("id", adt.id.globalId.toString)
        for (con <- sort.cons) {
          node.addParameter("constructor", con.globalId.toString)
        }
        var counter = 0
        for (tparam <- adt.tparams) {
          emit(tparam, node.id, counter)
          counter = counter + 1
        }
        

      case cons: ADTConstructor =>
        val node =
          emitNode(adt.id.name, parentId, astOrder, "ADTConstructor", adt.getPos.toString)
        node.addParameter("id", adt.id.globalId.toString)
        var counter = 0
        for (tparam <- adt.tparams) {
          emit(tparam, node.id, counter)
          counter = counter + 1
        }

        for (field <- cons.fields) {
          emit(field, node.id, counter)
          counter = counter + 1
        }

        if (!cons.sort.isEmpty)
          node.addParameter("parent", cons.sort.get.globalId.toString)
    }
  }

}

object DatalogEmbedder {

  def embed(p: StainlessProgram): List[DatalogNode] = {
    object embedder extends DatalogEmbedder {
      val program: p.type = p
    }

    embedder.emit()
  }

}
