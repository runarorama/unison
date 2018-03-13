package org.unisonweb

import org.unisonweb.util.PrettyPrint
import org.unisonweb.EasyTest._
import org.unisonweb.ABT.Name._
import org.unisonweb.Term._
import org.unisonweb.util.Builtins._

object TailCallTests {
  val tests = suite("tailcalls")(
    test("tc1") { implicit T =>
      // f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 a b =
      //   if0 a b (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 (dec a) (b + 1))
      //
      // f 0 0 0 0 0 0 0 0 0 10 10 = 20

      val f =
        Term.LetRec(
          Name("f") ->
            Term.Lam("x1", "x2", "x3", "x4", "x5",
                     "x6", "x7", "x8", "x9", "x10",
                     "a", "b") {
              Term.If0('a, 'b, 'f.v('x1, 'x2, 'x3, 'x4, 'x5,
                                    'x6, 'x7, 'x8, 'x9, 'x10,
                                    'a.v - 1, 'b.v + 1))
            }
        )('f)

      val tm = f(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10)
      val resultTerm = compilation.normalize(builtins)(tm)

      note(PrettyPrint.prettyTerm(resultTerm))
      equal(resultTerm, Term.Num(20))
    }
  )
}
