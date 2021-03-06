package org.unisonweb.util

import java.lang.{Character,StringBuilder}


/**
 * Text is represented with a `Sequence[Codepoint]`.
 *
 * Indices into a `Text` sequence refer to codepoints, not
 * Java `char` values. The Java `char` type is only 16 bits
 * and 21 bits are needed to express any codepoint in Unicode.
 *
 * NB: There is a huge amount of broken code out there that indexes directly
 * into an `Array[Char]` or `String` and assumes the result is something meaningful,
 * when in fact that code only works if all characters being used fit in the 16 bit
 * `char` type that Java provides!
 */
object Text {

  type Codepoint = Int

  type Text = Sequence[Codepoint]

  /** The empty `Text`, consists of no characters. */
  def empty: Text =
    Sequence.Flat(Deque.fromBlock(emptyBlock, 0))

  /** Converts from `String` to `Text`. */
  def fromString(s: String): Text =
    Sequence.Flat(Deque.viewArray(toCodepoints(s)))

  private def toCodepoints(s: String): Array[Codepoint] = {
    var i = 0
    var j = 0
    var out = new Array[Codepoint](s.codePointCount(0, s.length))
    while (i < s.length) {
      val ch = s.codePointAt(i)
      out(j) = ch
      if (Character.charCount(ch) == 2) i += 1
      j += 1
      i += 1
    }
    out
  }

  /** Convert a `Text` back to a `String`. */
  def toString(t: Text): String = {
    val buf = new StringBuilder()
    var i = 0L
    while (i < t.size) { buf.appendCodePoint(t(i)); i += 1L }
    buf.toString
  }

  def emptyBlock: Block[Codepoint] = Block.viewArray(new Array[Codepoint](128))
}
