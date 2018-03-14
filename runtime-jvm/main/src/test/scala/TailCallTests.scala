package org.unisonweb

import org.unisonweb.util.PrettyPrint
import org.unisonweb.EasyTest._
import org.unisonweb.ABT.Name._
import org.unisonweb.Term._
import org.unisonweb.util.Builtins._

object TailCallTests {
  val tests = suite("tailcalls")(
//    test("self-recursive function with important data in args 11 & 12 ") { implicit T =>
//      // f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 a b =
//      //   if0 a b (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 (a - 1) (b + 1))
//      //
//      // f 0 0 0 0 0 0 0 0 0 0 10 10 = 20
//
//      val f =
//        Term.LetRec(
//          Name("f") ->
//            Term.Lam("x1", "x2", "x3", "x4", "x5",
//                     "x6", "x7", "x8", "x9", "x10",
//                     "a", "b") {
//              Term.If0('a, 'b, 'f.v('x1, 'x2, 'x3, 'x4, 'x5,
//                                    'x6, 'x7, 'x8, 'x9, 'x10,
//                                    'a.v - 1, 'b.v + 1))
//            }
//        )('f)
//
//      val tm = f(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10)
//      val resultTerm = compilation.normalize(builtins)(tm)
//
//      note(PrettyPrint.prettyTerm(resultTerm))
//      equal(resultTerm, Term.Num(20))
//    },
//    test("self-recursive function with important data in args 1 & 2 of 12") { implicit T =>
//      // f a b x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
//      //   if0 a b (f (a - 1) (b + 1) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
//      //
//      // f 10 10 0 0 0 0 0 0 0 0 0 0 = 20
//
//      val f =
//        Term.LetRec(
//          Name("f") ->
//            Term.Lam("a", "b",
//              "x1", "x2", "x3", "x4", "x5",
//              "x6", "x7", "x8", "x9", "x10") {
//              Term.If0('a, 'b, 'f.v('a.v - 1, 'b.v + 1,
//                                    'x1, 'x2, 'x3, 'x4, 'x5,
//                                    'x6, 'x7, 'x8, 'x9, 'x10))
//            }
//        )('f)
//
//      val tm = f(10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
//      val resultTerm = compilation.normalize(builtins)(tm)
//
//      note(PrettyPrint.prettyTerm(resultTerm))
//      equal(resultTerm, Term.Num(20))
//    },
    test("recursive function of 4 args") { implicit t =>
      // f a x y b  = if0 a b (f (a - 1) x y (b + 1))
      //
      // f 10 x y 10 = 20

      val f =
        Term.LetRec(
//          'f.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f.v('a.v - 1, 'x, 'y, 'b.v + 1)) },
//          'f.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f.v('a.v - 1, 'x, 'y, 'b.v + 1)) },
//          'f.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f.v('a.v - 1, 'x, 'y, 'b.v + 1)) },
          'f.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f.v('a.v - 1, 'x, 'y, 'b.v + 1)) }
        )('f)

      val tm = f(10, 0, 0, 10)
      val resultTerm = compilation.normalize(builtins)(tm)

      note(PrettyPrint.prettyTerm(resultTerm))
      equal(resultTerm, Term.Num(20))
    }
//    test("co-recursive functions of 4 args") { implicit t =>
//      // f a x y b  = if0 a b (f (a - 1) x y (b + 1))
//      //
//      // f 10 x y 10 = 20
//
//      val f =
//        Term.LetRec(
//          'f.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'g.v('a.v - 1, 'x, 'y, 'b.v + 1)) },
//          'g.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f.v('a.v - 1, 'x, 'y, 'b.v + 1)) }
//        )('f)
//
//      val tm = f(10, 0, 0, 10)
//      val resultTerm = compilation.normalize(builtins)(tm)
//
//      note(PrettyPrint.prettyTerm(resultTerm))
//      equal(resultTerm, Term.Num(20))
//    },
//    test("four co-recursive functions of 4 args :P") { implicit t =>
//      // f a x y b  = if0 a b (f (a - 1) x y (b + 1))
//      //
//      // f 10 x y 10 = 20
//
//      val f =
//        Term.LetRec(
//          'f1.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b,  'f2.v('a.v - 1, 'x, 'y, 'b.v + 1)) },
//          'f2.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f3.v('a.v - 1, 'x, 'y, 'b.v + 1)) },
//          'f3.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f4.v('a.v - 1, 'x, 'y, 'b.v + 1)) },
//          'f4.n -> Term.Lam('a, 'x, 'y, 'b) { Term.If0('a, 'b, 'f1.v('a.v - 1, 'x, 'y, 'b.v + 1)) }
//        )('f1)
//
//      val tm = f(10, 0, 0, 10)
//      val resultTerm = compilation.normalize(builtins)(tm)
//
//      note(PrettyPrint.prettyTerm(resultTerm))
//      equal(resultTerm, Term.Num(20))
//    }

  )
}
