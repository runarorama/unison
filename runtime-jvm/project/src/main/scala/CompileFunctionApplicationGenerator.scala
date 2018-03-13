package org.unisonweb.codegeneration

object CompileFunctionApplicationGenerator extends OneFileGenerator("CompileFunctionApplication.scala") {
  sealed trait EvalFnType
  object EvalFnType {
    case object Lambda extends EvalFnType
    case object TailCall extends EvalFnType
    case object SelfTailCall extends EvalFnType
  }
  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    b("trait CompileFunctionApplication") {
      indentEqExpr("def staticCall(e: TermC, fn: Lambda, args: Array[Computation], isTail: IsTail): Computation") {
        "if (isTail) staticTailCall(e, fn, args)" <>
        "else staticNonTail(e, fn, args)"
      } <<>>
      indentEqExpr("def staticRecCall(e: TermC, args: Array[Computation], isTail: IsTail): Computation") {
        "if (isTail) staticRecTailCall(e, args)" <>
        "else staticRecNonTail(e, args)"
      } <<>>
      indentEqExpr("def dynamicCall(e: TermC, mkFn: Computation, args: Array[Computation], isTail: IsTail): Computation") {
        "if (isTail) dynamicTailCall(e, mkFn, args)" <>
        "else dynamicNonTailCall(e, mkFn, args)"
      } <<>>
      sourceStatic("staticNonTail",
        classPrefix = "StaticNonTail",
        declArgsPrefix = Some("fn: Lambda"),
        evalFn = "fn",
        evalFnType = EvalFnType.Lambda,
        evalArgsPrefix = Some("fn")
      ) <<>>
      sourceStatic("staticTailCall",
        classPrefix = "StaticTailCall",
        declArgsPrefix = Some("fn: Lambda"),
        evalFn = "tailCall",
        evalFnType = EvalFnType.TailCall,
        evalArgsPrefix = Some("fn")
      ) <<>>
      sourceStatic("staticRecNonTail",
        classPrefix = "StaticRecNonTail",
        declArgsPrefix = None,
        evalFn = "rec",
        evalFnType = EvalFnType.Lambda,
        evalArgsPrefix = Some("rec")
      ) <<>>
      sourceStatic("staticRecTailCall",
        classPrefix = "StaticRecTailCall",
        declArgsPrefix = None,
        evalFn = "selfTailCall",
        evalFnType = EvalFnType.SelfTailCall,
        evalArgsPrefix = None
      ) <<>>
      sourceDynamic(isTail = false) <<>>
      sourceDynamic(isTail = true)
    }

  def sourceStatic(defName: String, classPrefix: String, declArgsPrefix: Option[String], evalFn: String, evalFnType: EvalFnType, evalArgsPrefix: Option[String]) = {
    val evalArgsPrefixStr = evalArgsPrefix.map(_ + ", ").getOrElse("")

    def evalArgs(stackSize: Int, argCount: Int) =
      evalArgsPrefixStr + (argCount - 1 to 0 by -1).commas(i => s"arg${i}(${xEvalArgs(stackSize)}), r.boxed") + commaIf(argCount) + "r"

    def xEvalArgs(argCount: Int) =
      "rec, " + (0 until argCount).commas(i => s"x${i}, x${i}b") + commaIf(argCount) + "r"

    def evalArgsN(argCount: Int) =
      evalArgsPrefixStr + (argCount - 1 to 0 by -1).commas(i => s"arg${i}(rec, unboxed, boxed, r), r.boxed") + commaIf(argCount) + "r"

    bEq(s"def $defName(e: TermC, " + declArgsPrefix.map(_ + ", ").getOrElse("") + "args: Array[Computation]): Computation") {
      switch("stackSize(e)") {
        (0 to maxInlineStack).eachNL { stackSize =>
          `case`(s"/* stackSize = */ $stackSize") {
            switch("args.length") {
              (1 to maxInlineArgs).eachNL { argCount =>
                `case`(argCount) {
                  val className = s"${classPrefix}S${stackSize}A${argCount}"
                  (0 until argCount).each { i => s"val arg$i = args($i)" } <>
                  b(s"class $className extends Computation${stackSize}(e, ())") {
                    indentEqExpr(applySignature(stackSize)) {
                      s"$evalFn(${evalArgs(stackSize, argCount)})"
                    }
                  } <>
                  s"new $className"
                }
              } <<>>
              `case`("argCount") {
                val className = s"${classPrefix}S${stackSize}AN"
                b(s"class $className extends Computation${stackSize}(e, ())") {
                  bEq(applySignature(stackSize)) {
                    "val unboxedArgs = new Array[Double](argCount)" <>
                    "val boxedArgs = new Array[Param](argCount)" <>
                    "var i = 0" <>
                    b("while (i < argCount)") {
                      s"unboxedArgs(argCount - 1 - i) = " + catchTC(s"args(i)(${xEvalArgs(stackSize)})") <>
                      s"boxedArgs(argCount - 1 - i) = r.boxed" <>
                      "i += 1"
                    } <>
                    s"$evalFn(${evalArgsPrefixStr}unboxedArgs, boxedArgs, r)"
                  }
                } <>
                s"new $className"
              }
            }
          }
        } <<>>
        `case`("stackSize") {
          switch("args.length") {
            (1 to maxInlineArgs).eachNL { argCount =>
              `case`(s"/* argCount = */ $argCount") {
                val className = s"${classPrefix}SNA$argCount"
                (0 until argCount).each { i => s"val arg$i = args($i)" } <>
                b(s"class $className extends ComputationN(stackSize, e, ())") {
                  indentEqExpr(applyNSignature) {
                    s"$evalFn(${evalArgsN(argCount)})"
                  }
                } <>
                s"new $className"
              }
            } <<>>
            `case`("argCount") {
              val className = s"${classPrefix}SNAN"
              b(s"class $className extends ComputationN(stackSize, e, ())") {
                bEq(applyNSignature) {
                  "val unboxedArgs = new Array[Double](argCount)" <>
                  "val boxedArgs = new Array[Param](argCount)" <>
                  "var i = 0" <>
                  b("while (i < argCount)") {
                    s"unboxedArgs(argCount - 1 - i) = " + catchTC(s"args(i)(rec, unboxed, boxed, r)") <>
                    s"boxedArgs(argCount - 1 - i) = r.boxed" <>
                    "i += 1"
                  } <>
                  s"$evalFn(${evalArgsPrefixStr}unboxedArgs, boxedArgs, r)"
                }
              } <>
              s"new $className"
            }
          }
        }
      }
    }
  }

  def sourceDynamic(isTail: Boolean): String = {
    val emptyOrNon = if (isTail) "" else "Non"
    bEq(s"def dynamic${emptyOrNon}TailCall(e: TermC, mkFn: Computation, args: Array[Computation]): Computation") {
      switch("stackSize(e)") {
        (0 to maxInlineStack).eachNL { stackSize =>
          `case`(s"/* stackSize = */ $stackSize") {
            switch("args.length") {
              (1 to maxInlineArgs).eachNL { argCount =>
                `case`(s"/* argCount = */ $argCount") {
                  val className = s"Dynamic${emptyOrNon}TailCallS${stackSize}A${argCount}"
                  b(s"class $className extends Computation$stackSize(e, ())") {
                    (0 until argCount).each(j => s"val arg$j = args($j)") <>
                    bEq(applySignature(stackSize)) {
                      s"val lambda = ${tailEvalBoxed(stackSize, "mkFn")}.asInstanceOf[Lambda]" <> (
                        if (isTail)
                          "tailCall(lambda, " + (argCount-1 to 0 by -1).commas(j => tailEval(stackSize, s"arg$j") + ", r.boxed") + commaIf(argCount) + "r)"
                        else
                          "lambda(lambda, " + (argCount-1 to 0 by -1).commas(j => tailEval(stackSize, s"arg$j") + ", r.boxed") + commaIf(argCount) + "r)"
                        )
                    }
                  } <>
                  s"new $className"
                }
              } <<>>
              `case`("argCount") {
                val className = s"Dynamic${emptyOrNon}TailCallS${stackSize}AN"
                b(s"class $className extends Computation$stackSize(e, ())") {
                  bEq(applySignature(stackSize)) {
                    "val unboxedArgsr = new Array[Double](argCount)" <>
                    "val boxedArgsr = new Array[Param](argCount)" <>
                    s"val lambda = ${tailEvalBoxed(stackSize, "mkFn")}.asInstanceOf[Lambda]" <>
                    "var k = 0" <>
                    b("while (k < argCount)") {
                      "unboxedArgsr(argCount - 1 - k) = " + tailEval(stackSize, "args(k)") <>
                      "boxedArgsr(argCount - 1 - k) = r.boxed" <>
                      "k += 1"
                    } <>
                      (if (isTail)
                        "tailCall(lambda, unboxedArgsr, boxedArgsr, r)"
                      else
                        "lambda(lambda, unboxedArgsr, boxedArgsr, r)")
                  }
                } <>
                s"new $className"
              }
            }
          }
        } <<>>
        `case`("stackSize") {
          switch("args.length") {
            (1 to maxInlineArgs).eachNL { argCount =>
              `case`(s"/* argCount = */ $argCount") {
                val className = s"Dynamic${emptyOrNon}TailCallSNA$argCount"
                b(s"class $className extends ComputationN(stackSize, e, ())") {
                  (0 until argCount).each(j => s"val arg$j = args($j)") <>
                  bEq(applyNSignature) {
                    s"val lambda = ${tailEvalNBoxed("mkFn")}.asInstanceOf[Lambda]" <>
                    (if (!isTail) s"lambda(lambda, " + ((argCount-1) to 0 by -1).commas(j => tailEvalN(s"arg$j") + ", r.boxed") + commaIf(argCount) + "r)"
                    else s"tailCall(lambda, " + ((argCount-1) to 0 by -1).commas(j => tailEvalN(s"arg$j") + ", r.boxed") + commaIf(argCount) + "r)")
                  }
                } <>
                s"new $className"
              }
            } <<>>
            `case`("argCount") {
              val className = s"Dynamic${emptyOrNon}TailCallSNAM"
              b(s"class $className extends ComputationN(argCount, e, ())") {
                bEq(applyNSignature) {
                  "val unboxedArgsr = new Array[Double](argCount)" <>
                  "val boxedArgsr = new Array[Param](argCount)" <>
                  s"val lambda = ${tailEvalNBoxed("mkFn")}.asInstanceOf[Lambda]" <>
                  "var k = 0" <>
                  b("while (k < argCount)") {
                    "unboxedArgsr(argCount - 1 - k) = " + tailEvalN("args(k)") <>
                    "boxedArgsr(argCount - 1 - k) = r.boxed" <>
                    "k += 1"
                  } <>
                  (if (!isTail) "lambda(lambda, unboxedArgsr, boxedArgsr, r)"
                  else "tailCall(lambda, unboxedArgsr, boxedArgsr, r)")
                }
              } <>
              s"new $className"
            }
          }
        }
      }
    }
  }
}
