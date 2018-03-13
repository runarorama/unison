package org.unisonweb.codegeneration

object CompileLet1Generator extends OneFileGenerator("CompileLet1.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    b("trait CompileLet1") {
      bEq("def compileLet1(e: TermC, binding: Computation, body: Computation)") {
        switch("stackSize(e)") {
          (0 until maxInlineStack).eachNL { stackSize =>
            val className = s"Let1S${stackSize}"
            `case`(stackSize) {
              b(s"class $className extends Computation${stackSize}(e,())") {
                bEq(applySignature(stackSize)) {
                  "val b = " + eval(stackSize, "binding") <>
                  "val br = r.boxed" <>
                  "body(rec, b, br, " + xArgs(stackSize) + commaIf(stackSize) + "r)"
                }
              } <>
              s"new $className"
            }
          } <>
          `case`(maxInlineStack) {
            val className = s"Let1S${maxInlineStack}"
            b(s"class $className extends Computation${maxInlineStack}(e,())") {
              bEq(applySignature(maxInlineStack)) {
                "val b = " + eval(maxInlineStack, "binding") <>
                "val br = r.boxed" <>
                ("body(rec, "
                  + array("b" +: (0 until maxInlineStack).map("x" + _)) + ", "
                  + array("br" +: (0 until maxInlineStack).map("x" + _ + "b"))
                  + ", r)")
              }
            } <>
            s"new $className"
          } <<>>
          `case`("stackSize") {
            val className = s"Let1SN"
            b(s"class $className extends ComputationN(stackSize, e, ())") {
              bEq(applyNSignature) {
                "// evaluate binding and push onto stack for evaluating body" <>
                "val b = " + evalN("binding") <>
                "val br = r.boxed" <>
                "body(rec, b +: unboxed, br +: boxed, r)"
              }
            } <>
            s"new $className"
          }
        }
      }
    }
}
