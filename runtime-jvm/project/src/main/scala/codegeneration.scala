package org.unisonweb

package object codegeneration {
  // JVM max is <256 args per java method
  val maxInlineStack = 6
  val maxInlineArgs = 4
  val maxInlineTC = 3

  assert(maxInlineTC <= maxInlineArgs)

  def applySignature(i: Int): String =
    "def apply(rec: Lambda, " + signatureXArgs(i) + commaIf(i) + "r: R): D"

  def applyNSignature: String =
    "def apply(rec: Lambda, unboxed: Array[Double], boxed: Array[Param], r: R): D"

  def applySignatureP(i: Int): String =
    "def apply(rec: Lambda" + commaIf(i) + signatureXArgs(i) + "): P"

  def applyNSignatureP: String =
    "def apply(rec: Lambda, unboxed: Array[Double], boxed: Array[Param]): P"

  def indent(level: Int, lines: Seq[String]): String =
    lines.flatMap(multilines =>
      multilines.split('\n').map(
        line => ("  " * level) ++ line
      )
    ).mkString("\n")

  def lines(s: String*) = s.mkString("\n")
  def braced(s: String) = s.b
  def commaIf(b: Boolean): String = if (b) ", " else ""
  def commaIf(i: Int): String = commaIf(i > 0)
  def includeIf(b: Boolean)(s: String): String = includeIfElse(b)(s, "")
  def includeIfElse(b: Boolean)(s1: String, s2: String): String = if (b) s1 else s2
  def includeIf(i: Int)(s: String): String = includeIf(i > 0)(s)
  def includeLineIf(b: Boolean)(s: String): String = includeIfElse(b)(s + "\n", "")
  def includeLineIf(i: Int)(s: String): String = includeLineIf(i > 0)(s)
  /** I get this wrong every time and want to fix it once and for all. */
  def switch(s: String)(body: String) = s"($s: @annotation.switch) match " + body.b
  def match1(s: String)(body: String) = s"$s match { $body }"
  def `match`(s: String)(body: String) = s"$s match " + body.b
  def `case`(expr: String)(body: String): String = s"case $expr =>" <> body.indent
  def `case`(i: Int)(body: String): String = s"case $i =>" <> body.indent
  def caseInline(i: Int)(body: String): String = s"case $i => $body"
  def caseInline(expr: String)(body: String): String = s"case $expr => $body"
  def b(s: String)(body: String) = s + " " + body.b
  def bEq(s: String)(body: String) = s + " = " + body.b
  def indentEqExpr(s: String)(expr: String) = s + " =" <> expr.indent

  // tailEval(4, "foo") = "foo(rec, x0, x0b, x1, x1b, x2, x2b, x3, x3b, r)"
  def tailEval(i: Int, expr: String): String =
   expr + "(rec, "+xArgs(i)+commaIf(i)+ "r)"

  // tailEvalN(2, "foo") = "foo(rec, unboxed(0), boxed(0), unboxed(1), boxed(1), r)"
  def tailEvalN(i: Int, expr: String): String =
    expr + "(rec, "+xsArgs(i)+commaIf(i)+ "r)"

  /* tailEvalN("foo") = "foo(rec, unboxed, boxed, r)" */
  def tailEvalN(expr: String): String =
    expr + "(rec, unboxed, boxed, r)"

  // tailEval(4, "foo") = "foo(rec, x0, x0b, x1, x1b, x2, x2b, x3, x3b)"
  def tailEvalP(i: Int, expr: String): String =
   expr + "(rec" + commaIf(i)+xArgs(i)+ ")"

  // tailEvalN(2, "foo") = "foo(rec, unboxed(0), boxed(0), unboxed(1), boxed(1))"
  def tailEvalNP(i: Int, expr: String): String =
    expr + "(rec"+commaIf(i)+xsArgs(i) + ")"

  /* tailEvalN("foo") = "foo(rec, unboxed, boxed)" */
  def tailEvalNP(expr: String): String =
    expr + "(rec, unboxed, boxed)"

  def catchTC(expr: String) =
    s"try { $expr } catch { case e: TC => loop(r) }"

  def tailEvalBoxed(i: Int, expr: String) =
    "{ " + tailEval(i, expr) + "; r.boxed }"

  def tailEvalNBoxed(expr: String) =
    "{ " + tailEvalN(expr) + "; r.boxed }"

  def eval(i: Int, expr: String) = catchTC(tailEval(i, expr))

  def evalN(expr: String) = catchTC(expr + "(rec, unboxed, boxed, r)")

  private // don't ask why it's called this
  def indentify(level: Int, lines: String): String =
    indent(level, lines :: Nil)

  implicit class Codegen1(val r: Seq[Int]) extends AnyVal {
    def each(f: Int => String): String = r map f mkString "\n"
    def eachNL(f: Int => String): String = r map f mkString "\n\n"
    def semis(f: Int => String): String = r map f mkString "; "
    def commas(f: Int => String): String = r map f mkString ", "
  }

  implicit class Codegen2(val s: String) extends AnyVal {
    def indentBy(by: Int) = indentify(by, s)
    def indent = indentify(1, s)
    def bracedBy(by: Int) = "{" <> s.indentBy(by + 1) <> "}".indentBy(by) // K&R FTW
    def b = s.bracedBy(0)
    def <>(s2: String) = s + "\n" + s2
    def <<>>(s2: String) = s + "\n\n" + s2
    def nl(s2: String) = s <> s2
    def <>| = s + "\n"
    def <<>>| = s + "\n\n"
  }

  def array(ss: Seq[String]): String =
    s"Array(${ss.mkString(", ")})"

  def xArgs(count: Int): String = xArgs(0, count)
  def xArgs(start: Int, count: Int): String =
   (start until (start+count)).commas(i => s"x$i, x${i}b")
  def signatureXArgs(count: Int) = (0 until count).commas(i => s"x$i: D, x${i}b: P")
  def xsArgs(count: Int): String = xsArgs(0, count)
  private def xsArgs(start: Int, count: Int): String =
   start until (start + count) commas (i => s"unboxed($i), boxed($i)")
}
