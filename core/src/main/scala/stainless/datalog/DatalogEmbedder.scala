package stainless
package datalog
import extraction.xlang.{trees => xt}
import scala.language.existentials

trait DatalogEmbedder {

    val program: Program

    import program._
    import program.symbols._
    import program.trees._
    
    private var embeddings = List[String]()
    private var newNodeId = 0 
    private def getNewNodeId() = {
        val nextId = newNodeId
        newNodeId = newNodeId + 1
        nextId
    }
    
    def emit() : List[String] = {
        for (fun <- program.symbols.functions.values) {
            emit(fun, -1, -1)
        }
        
        for (adt <- program.symbols.adts.values) {
            emit(adt, -1, -1)    
        }
        embeddings
    }

    def emit(vd: ValDef, parentId: Int, astOrder: Int): Unit = {
      val newId = getNewNodeId()
      emitNode(vd.id.name, newId, parentId, astOrder, "ValDef")
      emitParameter("id", newId, 0, vd.id.globalId.toString)
      emit(vd.toVariable, newId, 0)
    }
    
    def emit(tpd: TypeParameterDef, parentId: Int, astOrder: Int): Unit = {
      val newId = getNewNodeId()        
      emitNode(tpd.id.name, newId, parentId, astOrder, "TypeParameterDef")
      emitParameter("id", newId, 0, tpd.id.globalId.toString)
      emit(tpd.tp, newId, 0) //emit corresponding type
    }
    
    def emit(e: Expr, parentId: Int, astOrder: Int): Unit = {
    //TODO define predicate that says that all of those are expressions
      e match {
        case Assume(pred, body) =>
            emitBinaryOp("Assume", pred, body, parentId, astOrder)
        case Variable(id, tpe, _) => 
            val newId = getNewNodeId()
            emitNode("", newId,  parentId, astOrder, "Variable")
            emitParameter("variableId", newId, 0, id.globalId.toString)
            emit(tpe, newId, 0)
        case Let(vd, value, body) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "Let")
            emit(vd, newId, 0)
            emit(value, newId, 1)
            emit(body, newId, 2)
        case Application(callee, args) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "Application")
            emit(callee, newId, 0)
            var counter = 1
            for (arg <- args) {
                emit(arg, newId, counter)
                counter = counter + 1
            }
        case Lambda(args, body) => 
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "Lambda")
            emit(body, newId, 0)
            var counter = 1 //change order to be able to distinguish from astOrder
            for (arg <- args) {
                emit(arg, newId, counter)
                counter = counter + 1
            }
        case Forall(args, body) => 
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "Forall")
            emit(body, newId, 0)
            var counter = 1 
            for (arg <- args) {
                emit(arg, newId, counter)
                counter = counter + 1
            }  
        case Choose(res, pred) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "Choose")
            emit(res, newId, 0)
            emit(pred, newId, 1)  
        case FunctionInvocation(id, tps, args) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "FunctionInvocation")
            emitParameter("invokedFunction", newId, 0, id.globalId.toString)
            var counter = 0
            for (tp <- tps) {
                emit(tp, newId, counter)
                counter = counter + 1
            }
            for (arg <- args) {
                emit(arg, newId, counter)
                counter = counter + 1
            } 
        case IfExpr(cond, thenn, elze) =>
            emitTernaryOp("IfExpr", cond, thenn, elze, parentId, astOrder)
        case CharLiteral(value) =>
            emitLiteral("CharLiteral", value.toString, parentId, astOrder)
        case bv @ BVLiteral(value, size) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "BVLiteral")
            emitParameter("value", newId, 0, bv.toBigInt.toString)
            emitParameter("size", newId, 1, size.toString)
        case IntegerLiteral(value) =>
            emitLiteral("IntegerLiteral", value.toString, parentId, astOrder)
        case FractionLiteral(num, denom) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "FractionLiteral")
            emitParameter("numerator", newId, 0, num.toString)
            emitParameter("denominator", newId, 1, denom.toString)
        case BooleanLiteral(value) =>
            emitLiteral("BooleanLiteral", value.toString, parentId, astOrder)//TODO does LatMap expect false or False?
        case UnitLiteral() =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "UnitLiteral")
        case StringLiteral(value) =>
            //handle escape sequences: add extra backslash in front of every backslash
            var valueStr = value.replaceAll("\\\\", "\\\\\\\\")
            for(ch <- List('b', 't', 'n', 'f', 'r', '"', '\''))
                valueStr = valueStr.replaceAll("\\"+ch, "\\\\"+ch)
            emitLiteral("StringLiteral", valueStr, parentId, astOrder)
        case GenericValue(tp, id) => //TODO what does id represent? Why not "identifier"?
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "GenericValue")
            emitParameter("id", newId, 0, id.toString)
            emit(tp, newId, 0)
        case ADT(adt, args) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "ADT")
            emitParameter("adtType", newId, 0, adt.id.globalId.toString)
            var counter = 0
            for (arg <- args) {
                emit(arg, newId, counter)
                counter = counter + 1
            }
        case IsInstanceOf(expr, tpe) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "IsInstanceOf")
            emit(expr, newId, 0)
            emit(tpe, newId, 1)
        case AsInstanceOf(expr, tpe) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "AsInstanceOf")
            emit(expr, newId, 0)
            emit(tpe, newId, 1)
        case ADTSelector(adt, selector) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "ADTSelector")
            emit(adt, newId, 0)
            emitParameter("selector", newId, 0, selector.globalId.toString)
        case Equals(lhs, rhs) =>
            emitBinaryOp("Equals", lhs, rhs, parentId, astOrder)
        case And(exprs) =>
            emitOp("And", exprs, parentId, astOrder)
        case Or(exprs) =>
            emitOp("Or", exprs, parentId, astOrder)
        case Implies(lhs, rhs) =>
            emitBinaryOp("Implies", lhs, rhs, parentId, astOrder) 
        case Not(expr) =>
            emitUnaryOp("Not", expr, parentId, astOrder)
        case StringConcat(lhs, rhs) =>
            emitBinaryOp("StringConcat", lhs, rhs, parentId, astOrder) 
        case SubString(expr, start, end) =>
            emitTernaryOp("SubString", expr, start, end, parentId, astOrder) 
        case StringLength(expr) =>
            emitUnaryOp("StringLength", expr, parentId, astOrder)
        case Plus(lhs, rhs) =>
            emitBinaryOp("Plus", lhs, rhs, parentId, astOrder) 
        case Minus(lhs, rhs) =>
            emitBinaryOp("Minus", lhs, rhs, parentId, astOrder) 
        case UMinus(expr) =>
            emitUnaryOp("UMinus", expr, parentId, astOrder)
        case Times(lhs, rhs) =>
            emitBinaryOp("Times", lhs, rhs, parentId, astOrder)
        case Division(lhs, rhs) =>
            emitBinaryOp("Divide", lhs, rhs, parentId, astOrder) 
        case Remainder(lhs, rhs) =>
            emitBinaryOp("Remainder", lhs, rhs, parentId, astOrder) 
        case Modulo(lhs, rhs) =>
            emitBinaryOp("Modulo", lhs, rhs, parentId, astOrder)
        case LessThan(lhs, rhs) =>
            emitBinaryOp("LessThan", lhs, rhs, parentId, astOrder)
        case GreaterThan(lhs, rhs) =>
            emitBinaryOp("GreaterThan", lhs, rhs, parentId, astOrder)
        case LessEquals(lhs, rhs) =>
            emitBinaryOp("LessEquals", lhs, rhs, parentId, astOrder)
        case GreaterEquals(lhs, rhs) =>
            emitBinaryOp("GreaterEquals", lhs, rhs, parentId, astOrder)
        case BVNot(expr) =>
            emitUnaryOp("BVNot", expr, parentId, astOrder)
        case BVOr(lhs, rhs) => 
            emitBinaryOp("BVOr", lhs, rhs, parentId, astOrder)
        case BVAnd(lhs, rhs) => 
            emitBinaryOp("BVAnd", lhs, rhs, parentId, astOrder)
        case BVXor(lhs, rhs) => 
            emitBinaryOp("BVXor", lhs, rhs, parentId, astOrder)
        case BVShiftLeft(lhs, rhs) => 
            emitBinaryOp("BVShiftLeft", lhs, rhs, parentId, astOrder)
        case BVAShiftRight(lhs, rhs) => 
            emitBinaryOp("BVAShiftRight", lhs, rhs, parentId, astOrder)
        case BVLShiftRight(lhs, rhs) => 
            emitBinaryOp("BVLShiftRight", lhs, rhs, parentId, astOrder)
        case BVNarrowingCast(expr, newType) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "BVNarrowingCast")
            emit(expr, newId, 0)
            emit(newType, newId, 1)
        case BVWideningCast(expr, newType) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "BVWideningCast")
            emit(expr, newId, 0)
            emit(newType, newId, 1)
        case Tuple(exprs) =>
            emitOp("Tuple", exprs, parentId, astOrder)
        case TupleSelect(tuple, index) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "TupleSelect")
            emit(tuple, newId, 0)
            emitParameter("index", newId, 0, index.toString)
        case FiniteSet(elements, base) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "FiniteSet")
            var counter = 0
            for (el <- elements) {
                emit(el, newId, counter)
                counter = counter + 1
            }
            emit(base, newId, counter)
        case SetAdd(set, elem) =>
            emitBinaryOp("SetAdd", set, elem, parentId, astOrder)
        case ElementOfSet(elem, set) =>
            emitBinaryOp("ElementOfSet", elem, set, parentId, astOrder)
        case SubsetOf(set1, set2) =>
            emitBinaryOp("SubsetOf", set1, set2, parentId, astOrder)
        case SetIntersection(set1, set2) =>
            emitBinaryOp("SetIntersection", set1, set2, parentId, astOrder)
        case SetUnion(set1, set2) =>
            emitBinaryOp("SetUnion", set1, set2, parentId, astOrder)
        case SetDifference(set1, set2) =>
            emitBinaryOp("SetDifference", set1, set2, parentId, astOrder)
        case FiniteBag(elements, base) => //TODO why are there tuples in bag?
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "FiniteSet")
            var counter = 0
            for ((el1, el2) <- elements) {
                emit(el1, newId, counter)
                counter = counter + 1
                emit(el2, newId, counter)
                counter = counter + 1
            }
            emit(base, newId, counter)
        case BagAdd(bag, elem) =>
            emitBinaryOp("BagAdd", bag, elem, parentId, astOrder)
        case MultiplicityInBag(bag, elem) =>
            emitBinaryOp("MultiplicityInBag", bag, elem, parentId, astOrder)
        case BagIntersection(bag1, bag2) =>
            emitBinaryOp("BagIntersection", bag1, bag2, parentId, astOrder)
        case BagUnion(bag1, bag2) =>
            emitBinaryOp("BagUnion", bag1, bag2, parentId, astOrder)
        case BagDifference(bag1, bag2) =>
            emitBinaryOp("BagDifference", bag1, bag2, parentId, astOrder)
        case FiniteMap(pairs, default, keyType, valueType) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "FiniteMap")
            emit(default, newId, 0)
            emit(keyType, newId, 1)               
            emit(valueType, newId, 2)
            var counter = 3
            for ((el1, el2) <- pairs) {
                emit(el1, newId, counter)
                counter = counter + 1
                emit(el2, newId, counter)
                counter = counter + 1
            }
            
        case MapApply(map, key) =>
            emitBinaryOp("MapApply", map, key, parentId, astOrder)      
        case MapUpdated(map, key, value) =>
            emitTernaryOp("MapUpdated", map, key, value, parentId, astOrder)
        case NoTree(tpe) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "NoTree")
            emit(tpe, newId, 0)
        case Error(tpe, description) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "Error")
            emit(tpe, newId, 0)
            emitParameter("description", newId, 0, description)
        case Require(pred, body) =>
            emitBinaryOp("Require", pred, body, parentId, astOrder)
        case Annotated(body, flags) =>
            //TODO do flags have to be stored?
            emitUnaryOp("Annotated", body, parentId, astOrder)      
        case Ensuring(body, pred) =>
            emitBinaryOp("Ensuring", body, pred, parentId, astOrder)
        case Assert(pred, error, body) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "Assert")
            emit(pred, newId, 0)
            emit(body, newId, 1)
            emitParameter("error", newId, 0, error.getOrElse(""))
        case MatchExpr(scrutinee, cases) => 
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "MatchExpr")
            emit(scrutinee, newId, 0)
            var counter = 1
            for (c <- cases) {
                emit(c, newId, counter)
                counter = counter + 1
            }
        case FiniteArray(elems, base) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "FiniteArray")
            emit(base, newId, 0)
            var counter = 0
            for(el <- elems) {
                emit(el, newId, counter)
                counter = counter + 1
            } 
        case LargeArray(elems, default, size, base) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "LargeArray")
            emit(default, newId, 0)
            emit(size, newId, 1)
            emit(base, newId, 2)
            var counter = 0
            //represent map: parameterOrder = key, value = astOrder of expr. child that corresponds to value
            for((index, value) <- elems) {
                emit(value, newId, counter)
                emitParameter("value", newId, index, counter.toString)
                counter = counter + 1
            }
        case ArraySelect(array, index) =>
            emitBinaryOp("ArraySelect", array, index, parentId, astOrder)   
        case ArrayUpdated(array, index, value) =>
            emitTernaryOp("ArrayUpdated", array, index, value, parentId, astOrder)
        case ArrayLength(array) =>
            emitUnaryOp("ArrayLength", array, parentId, astOrder)
        //case _ => ctx.reporter.info("Warning: Some expression is not embedded!")
        
                
      }
      
    }
    
    private def emitOp(name: String, exprs: Seq[Expr], parentId: Int, astOrder: Int) = {
        val newId = getNewNodeId()
        emitNode("", newId, parentId, astOrder, name)
        var counter = 0
        for (expr <- exprs) {
            emit(expr, newId, counter)
            counter = counter + 1
        }
    }
    
    private def emitTernaryOp(name: String, first: Expr, second: Expr, third: Expr, parentId: Int, astOrder: Int) : Unit = {
        val newId = getNewNodeId()
        emitNode("", newId, parentId, astOrder, name)
        emit(first, newId, 0)
        emit(second, newId, 1)
        emit(third, newId, 1)
    }
    
    private def emitBinaryOp(name: String, lhs: Expr, rhs: Expr, parentId: Int, astOrder: Int) : Unit = {
        val newId = getNewNodeId()
        emitNode("", newId, parentId, astOrder, name)
        emit(lhs, newId, 0)
        emit(rhs, newId, 1)
    }
    
    private def emitUnaryOp(name: String, expr: Expr, parentId: Int, astOrder: Int) : Unit = {
        val newId = getNewNodeId()
        emitNode("", newId, parentId, astOrder, name)
        emit(expr, newId, 0)
    }
    
    private def emitLiteral(name: String, value: String, parentId: Int, astOrder: Int) : Unit = {
        val newId = getNewNodeId()
        emitNode("", newId, parentId, astOrder, name)
        emitParameter("value", newId, 0, value)
    }
    
    def emit(m: MatchCase, parentId: Int, astOrder: Int): Unit = {
        val newId = getNewNodeId()
        emitNode("", newId, parentId, astOrder, "MatchCase")
        emit(m.pattern, newId, 0)
        emit(m.rhs, newId, 1)
        if(!m.optGuard.isEmpty)
            emit(m.optGuard.get, newId, 2)
    }
    
    def emit(p: Pattern, parentId: Int, astOrder: Int): Unit = p match {
        case InstanceOfPattern(binder, tpe) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "InstanceOfPattern")
            emit(tpe, newId, 0)
            if(!binder.isEmpty)
                emit(binder.get, newId, 1)
        case WildcardPattern(binder) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "WildcardPattern")
            if(!binder.isEmpty)
                emit(binder.get, newId, 0)
        case ADTPattern(binder, tpe, subPatterns) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "ADTPattern")
            emit(tpe, newId, 0)
            var counter = 0
            for(subP <- subPatterns) {
                emit(subP, newId, counter)
                counter = counter + 1
            } 
            if(!binder.isEmpty)
                emit(binder.get, newId, counter)
        case TuplePattern(binder, subPatterns) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "TuplePattern")
            var counter = 0
            for(subP <- subPatterns) {
                emit(subP, newId, counter)
                counter = counter + 1
            } 
            if(!binder.isEmpty)
                emit(binder.get, newId, counter)
        case LiteralPattern(binder, lit) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "LiteralPattern")
            emit(lit, newId, 0)
            if(!binder.isEmpty)
                emit(binder.get, newId, 1)
        case UnapplyPattern(binder, id, tps, subPatterns) => //TODO what is this about? what does identifier represent?
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "UnapplyPattern")
            var counter = 0
            for(tpe <- tps) {
                emit(tpe, newId, counter)
                counter = counter + 1
            } 
            for(subP <- subPatterns) {
                emit(subP, newId, counter)
                counter = counter + 1
            } 
            if(!binder.isEmpty)
                emit(binder.get, newId, counter)
            emitParameter("identifier", newId, 0, id.globalId.toString)
    }
      
    
    def emit(tpe: Type, parentId: Int, astOrder: Int): Unit = {
      //TODO define predicate that says that all of those are types
      tpe match {
        case Untyped => 
            emitNode("", getNewNodeId(), parentId, astOrder, "Untyped")
            
        case BooleanType() =>
            emitNode("", getNewNodeId(), parentId, astOrder, "BooleanType")
        case UnitType() =>
            emitNode("", getNewNodeId(), parentId, astOrder, "UnitType")
        case CharType() =>
            emitNode("", getNewNodeId(), parentId, astOrder, "CharType")
        case IntegerType() =>
            emitNode("", getNewNodeId(), parentId, astOrder, "IntegerType")
        case RealType() =>
            emitNode("", getNewNodeId(), parentId, astOrder, "RealType")
        case StringType() =>
            emitNode("", getNewNodeId(), parentId, astOrder,"StringType")
            
        case BVType(size) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "BVType")
            emitParameter("size", newId, 0, size.toString)
            
        case typePar @ TypeParameter(id, _) =>
            val newId = getNewNodeId()
            emitNode(id.name, newId, parentId, astOrder, "TypeParameter")
            emitParameter("id", newId, 0, id.globalId.toString)
            if(typePar.isCovariant)
                emitParameter("variance", newId, 1, "covariant")
            if(typePar.isContravariant)            
                emitParameter("variance", newId, 1, "contravariant")
        
        case TupleType(bases) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "TupleType")
            var counter = 0
            for (b <- bases) {
                emit(b, newId, counter)
                counter = counter + 1
            }
            
        case SetType(base) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "SetType")
            emit(base, newId, 0)
        case BagType(base) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "BagType")
            emit(base, newId, 0)
        case MapType(from, to) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "MapType")
            emit(to, newId, 0) //probably counter intuitve order but want consistency with function type
            emit(from, newId, 1)
        case FunctionType(from, to) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "FunctionType")
            emit(to, newId, 0)
            var counter = 1
            for (f <- from) {
                emit(f, newId, counter)
                counter = counter + 1
            }
            
        case ADTType(id, tps) => 
            val newId = getNewNodeId()
            emitNode(id.name, newId, parentId, astOrder, "ADTType")
            emitParameter("id", newId, 0, id.globalId.toString)
            var counter = 0
            for (t <- tps) {
                emit(t, newId, counter)
                counter = counter + 1
            }
        case ArrayType(base) =>
            val newId = getNewNodeId()
            emitNode("", newId, parentId, astOrder, "ArrayType")
            emit(base, newId, 0)
      }
    }
    
    def emit(fd: FunDef, parentId: Int, astOrder: Int): Unit = {
      val newId = getNewNodeId()
      emitNode(fd.id.name, newId, parentId, astOrder, "FunDef") //TODO what is parent of fundef? and what is the astorder (not that it should matter here)?
      emitParameter("id", newId, 0, fd.id.globalId.toString)
      var counter = 0
      for (tparam <- fd.tparams) {
        emit(tparam, newId, counter)
        counter = counter + 1
      }
      
      for (param <- fd.params) {
        emit(param, newId, counter)
        counter = counter + 1
      }
      
      emit(fd.fullBody, newId, counter)
      counter = counter + 1
      
      emit(fd.returnType, newId, counter)
      counter = counter + 1
      
      counter = 1
      for (callee <- program.symbols.callees(fd.id)) {
        emitParameter("calls", newId, counter, callee.globalId.toString)
        counter = counter + 1
      }
      
      
    }
    
    def emit(adt: ADTDefinition, parentId: Int, astOrder: Int): Unit = {
        val newId = getNewNodeId()
        adt match {
            case sort: ADTSort =>
                emitNode(adt.id.name, newId, parentId, astOrder, "ADTSort")
                emitParameter("id", newId, 0, adt.id.globalId.toString)
                var counter = 0
                for (tparam <- adt.tparams) {
                  emit(tparam, newId, counter)
                  counter = counter + 1
                }
                //TODO pointers to constructors needed?
                
            case cons: ADTConstructor =>
                emitNode(adt.id.name, newId, parentId, astOrder, "ADTConstructor")
                emitParameter("id", newId, 0, adt.id.globalId.toString)
                var counter = 0
                for (tparam <- adt.tparams) {
                  emit(tparam, newId, counter)
                  counter = counter + 1
                }
                
                for (field <- cons.fields) {
                  emit(field, newId, counter)
                  counter = counter + 1
                }
                
                if(!cons.sort.isEmpty)
                    emitParameter("parent", newId, 1, cons.sort.get.globalId.toString)
            }
    }
    
    /*private def emitNode(id: Identifier, parentId: Int, astOrder: Int, nodeType: String) {
        emitNode(id.name, id.globalId, parentId, astOrder, nodeType)
    }*/
    
    private def emitNode(name: String, id: Int, parentId: Int, astOrder: Int, nodeType: String) = {
        val nameStr = name match {
            case "_" => "_"
            case n => s""""$n""""
        }
        addEmbedding(s"""Node(${nameStr}, ${id}, ${parentId}, $astOrder, "$nodeType").\n""")
    }
    
    private def emitParameter(name: String, nodeId: Int, parameterOrder: Int, value: String) = {
        addEmbedding(s"""Parameter("${name}", ${nodeId}, $parameterOrder, "${value}").\n""")
    }
    
    private def addEmbedding(e: String) : Unit = {
        embeddings = embeddings :+ e
    }
    
}


object DatalogEmbedder {

  def embed(p: StainlessProgram) : List[String] = {
    object embedder extends DatalogEmbedder {
      val program: p.type = p
    }

    embedder.emit()
  }

}
