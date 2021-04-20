package equate

package scalatest {
  import org.scalatest.matchers.{Matcher, MatchResult}

  /** Make the [[equate]] method available (especially in subclass).
    *
    * Usage example:
    *
    * {{{
    * import equate.scalatest.Equate
    * import org.scalatest.{Matchers, WordSpec}
    *
    * class MySpec extends WordSpec with Matchers with Equate {
    *   "diffs" should {
    *     "be computed" when {
    *       "value are unequal" in {
    *         true should equate(false)
    *       }
    *     }
    *   }
    * }
    * }}}
    */
  trait Equate {
    class EqualityOrDiffMatcher(expected: Any) extends Matcher[Any] {

      import _root_.equate.Algorithms.longestCommonSubsequenceEdits
      import _root_.equate.Presentation.renderReport

      def apply(observed: Any): MatchResult = {
        val success = observed == expected
        val (ob, ex) = (observed.toString, expected.toString)
        val errorReport = renderReport(longestCommonSubsequenceEdits(ob, ex))
        MatchResult(success, errorReport, s"${ob} equated ${ex}")
      }
    }

    /** @param expected the expected value, against which
      * @return A matcher which asserts values to be equal or else diffs
      * observed and expected.
      */
    def equate(expected: Any): EqualityOrDiffMatcher =
      new EqualityOrDiffMatcher(expected)
  }

  /** Make the [[equate]] method available for importing.
    *
    * Usage example:
    *
    * {{{
    * import equate.scalatest.Equate.equate
    * import org.scalatest.{Matchers, WordSpec}
    *
    * class MySpec extends WordSpec with Matchers {
    *   "diffs" should {
    *     "be computed" when {
    *       "value are unequal" in {
    *         true should equate(false)
    *       }
    *     }
    *   }
    * }
    * }}}
    */
  object Equate extends Equate
}

package scalacheck {
  import org.scalacheck.Prop
  import org.scalacheck.Prop._
  import org.scalacheck.util.Pretty
  import scala.language.implicitConversions

  object Equate {
    import _root_.equate.Algorithms.longestCommonSubsequenceEdits
    import _root_.equate.Presentation.renderReport

    /** A property which succeeds if `ob == ex` and otherwise fails with a
      * message including a diff of `ob.toString` and `ex.toString`.
      * @param ob The observed value
      * @param ex The expected value
      * @param pp A prettifier
      */
    def equate[T](ob: T, ex: T)(implicit pp: T => Pretty): Prop = {
      if (ob == ex) {
        Prop.proved
      } else {
        val observed = Pretty.pretty[T](ob, Pretty.Params(0))
        val expected = Pretty.pretty[T](ex, Pretty.Params(0))
        val edits = longestCommonSubsequenceEdits(observed, expected)
        val report = renderReport(edits)
        report |: (ob == ex)
      }
    }

    /** Provide `?==` and `==?` methods which forward to [[Equate.equate]]
      * @param obex The observed value (for [[?==]]) or expected value (for
      * [[==?]]).
      * @param pp A prettifier
      */
    class EquateOperators[T](obex: => T)(implicit pp: T => Pretty) {
      /** Fail with a diff if `obex != expected`, blaming `obex` */
      def ?==(expected: T) = Equate.equate(obex, expected)
      /** Fail with a diff if `obex != observed`, blaming `observed` */
      def ==?(observed: T) = Equate.equate(observed, obex)
    }

    /** Automatic conversion which makes the [[EquateOperators.?==]] and
      * [[EquateOperators.==?]] methods available, which assert equality or
      * output a diff.
      *
      * Usage example:
      *
      * {{{
      * import equate.scalacheck.Equate.?==
      * import org.scalacheck.Prop.forAll
      * import org.scalacheck.Properties
      *
      * object Contradiction extends Properties("Contradiction") {
      *   property("p == !p") = forAll {
      *     (p: Boolean) => p ?== (!p)
      *   }
      * }
      * }}}
      */
    implicit def ?==[T](x: T)(implicit pp: T => Pretty): EquateOperators[T] =
      new EquateOperators[T](x)
  }
}

sealed trait Diff[T]
case class Insert[T](value: T) extends Diff[T]
case class Delete[T](value: T) extends Diff[T]
case class Common[T](value: T) extends Diff[T]

object Presentation {

  /** Maximum quantity of diff characters per line */
  val PAYLOAD_PER_LINE = 60

  /** Turn a diff into a complete error message */
  def renderReport(edits: Seq[Diff[Char]]): String = {
    val baselines = Vector(
      "Diff legend: '^' in both; 'o' observed only; 'e' expected only.",
      "",
      "Observed (above) versus expected (below):",
      ""
    )

    val editLines = edits
      .grouped(PAYLOAD_PER_LINE)
      .flatMap(renderDiffLines(_, "  ") ++ Seq(""))

    (baselines ++ editLines ++ Vector("good luck :)")).mkString("\n")
  }

  /** Stringify and indent a single line of diff data */
  def renderDiffLines(
    edits: Seq[Diff[Char]],
    indentation: String
  ): Seq[String] = {
    renderDiff(edits).map(_.mkString(indentation, "", ""))
  }

  /** Render a diff as multiple lines of text */
  def renderDiff(edits: Seq[Diff[Char]]): Seq[Seq[Char]] = {
    Seq(
      edits.map(_ match {
        case Delete(c) => c
        case Common(c) => c
        case Insert(_) => ' '
      }),
      edits.map(_ match {
        case Delete(_) => ' '
        case Common(c) => c
        case Insert(c) => c
      }),
      edits.map(_ match {
        case Delete(c) => 'o'
        case Insert(c) => 'e'
        case Common(c) => '^'
      })
    )
  }
}

object Algorithms {
  /** See "An `O(ND)` Difference Algorithm and Its Variations" by Eugene
    * W. Myers at [[http://xmailserver.org/diff2.pdf]]; this implements the
    * linear space recursive bidirectional search.
    * @return a char-by-char diff describing how to turn `a` into `b`.
    * Specifically,
    * {{{
    * a == longestCommonSubsequenceEdits(a, b).collect {
    *     case Common(c) => c
    *     case Delete(c) => c
    * }.mkString
    * }}}
    * and
    * {{{
    * b == longestCommonSubsequenceEdits(a, b).collect {
    *     case Common(c) => c
    *     case Insert(c) => c
    * }.mkString
    * }}}
    * and
    * {{{
    * // this is the longest common subsequence of a and b:
    * longestCommonSubsequenceEdits(a, b).collect { case Common(c) => c }
    * }}}
    */
  def longestCommonSubsequenceEdits(a: String, b: String): Seq[Diff[Char]] = {
    val longestCommonSubsequenceIndices = lcsSubstringIndices(a, b)
    val nonEmptyCommonSubstringIndices =
      longestCommonSubsequenceIndices.filter {
        case (xlo, xhi, _) => xlo != xhi
      }
    commonSubstringIndicesToDiff(a, b, nonEmptyCommonSubstringIndices)
  }

  /** @return a list of substring indices `(xlo, xhi, ylo)` which together
    * make up a (the) longest common subsequence between `a` and `b`.
    *
    * The substring index triples are such that
    * {{{
    * lcsRuns(a, b).forall {
    *     case (xlo, xhi, ylo) =>
    *         (xlo until xhi).forall(i => a(i) == b(i - xlo + ylo))
    * }
    * }}}
    *
    * furthermore `lcsRuns(a, b).flatMap(t => Seq(t._1, t._2))` is sorted, and
    * so is `lcsRuns(a, b).map(_._3)`
    */
  def lcsSubstringIndices(a: String, b: String): List[(Int, Int, Int)] = {
    val (n, m) = (a.size, b.size)
    if (n == 0 || m == 0) return Nil

    val limit = n + m
    val vForward = new Array[Int](2*limit + 1)
    val vReverse = new Array[Int](2*limit + 1)
    recurseAroundTheMiddle(a, b, limit, vForward, vReverse, 0, n, 0, m)
  }

  /** Use [[findMiddle]] to find a common substring with (approximately) half
    * the differences before it and half the differences after it, such that
    * the common substring is part of the longest common subsequence.  This
    * substring may be empty.  Then recursively solve the two subproblems on
    * each side of the substring.
    *
    * The parameters are the same as [[findMiddle]] with one exception.
    *
    * @param commonSubstringIndices return value accumulator, initially Nil
    * @return a list of common substring indices making up the longest common
    * subsequence.
    */
  def recurseAroundTheMiddle(
    a: String,
    b: String,
    zero: Int,
    vForward: Array[Int],
    vReverse: Array[Int],
    i: Int,
    n: Int,
    j: Int,
    m: Int,
    commonSubstringIndices: List[(Int, Int, Int)] = Nil
  ): List[(Int, Int, Int)] = {
    if (i == n || j == m) {
      commonSubstringIndices
    } else {

      var (pi, pj) = (i, j)
      while (pi < n && pj < m && a(pi) == b(pj)) {
        pi += 1
        pj += 1
      }
      if (pi > i) return (i, pi, j) :: recurseAroundTheMiddle(
        a, b, zero, vForward, vReverse, pi, n, pj, m, commonSubstringIndices
      )

      var (si, sj) = (n, m)
      while (si > i && sj > j && a(si - 1) == b(sj - 1)) {
        si -= 1
        sj -= 1
      }
      if (si < n) return recurseAroundTheMiddle(
        a, b, zero, vForward, vReverse, i, si, j, sj,
        (si, n, sj) :: commonSubstringIndices
      )

      val (editDistance, middleRun) =
        findMiddle(a, b, zero, vForward, vReverse, i, n, j, m)

      if (2 <= editDistance) {
        val (xlo, xhi, ylo) = middleRun
        val yhi = ylo + (xhi - xlo)
        val tail = recurseAroundTheMiddle(
          a, b, zero, vForward, vReverse, xhi, n, yhi, m,
          commonSubstringIndices
        )
        recurseAroundTheMiddle(
          a, b, zero, vForward, vReverse, i, xlo, j, ylo,
          (xlo, xhi, ylo) :: tail
        )
      } else if (editDistance == 0) {
        (i, n, j) :: commonSubstringIndices
      } else if (n - i < m - j) {
        val uneq = Range(i, n).find(k => a(k) != b(k - i + j))
        uneq match {
          case None =>
            (i, n, j) :: commonSubstringIndices
          case Some(k) =>
            (i, k, j) :: (k, n, j + k - i + 1) :: commonSubstringIndices
        }
      } else {
        Range(j, m).find(k => a(i + k - j) != b(k)) match {
          case None =>
            (i, i + m - j, j) :: commonSubstringIndices
          case Some(k) =>
            (i, i + k - j, j) ::
            (i + k - j + 1, n, k) ::
            commonSubstringIndices
        }
      }
    }
  }

  /** Find a `(dist, (xlo, xhi, ylo))` such that `dist` is the LCS edit
    * distance between `a.substring(i, n)` and `b.substring(j, m)`, and such
    * that `a(i) == b(i - xlo + ylo)` for all `i` in `(xlo until xhi)`.
    *
    * If `0 <= i <= n <= a.size` and `0 <= j <= m <= b.size` don't both hold,
    * hilarity may ensue.
    *
    * @param a the observed/old sequence
    * @param b the expected/new sequence
    * @param zero an index into `vForward` and `vReverse` such that
    * `((zero - (a.size + b.size)) to (zero + (a.size + b.size)))` is a range
    * of valid (in-bounds) indices into `vForward` and `vReverse`
    * @param vForward an array, some entries of which will be overwritten
    * @param vReverse an array, some entries of which will be overwritten
    * @param i the start (inclusive) of the substring of `a` to search in
    * @param n the end (exclusive) of the substring of `a` to search in
    * @param j the start (inclusive) of the substring of `b` to search in
    * @param m the end (exclusive) of the substring of `b` to search in
    * @return see description
    */
  def findMiddle(
    a: String,
    b: String,
    zero: Int,
    vForward: Array[Int],
    vReverse: Array[Int],
    i: Int,
    n: Int,
    j: Int,
    m: Int
  ): (Int, (Int, Int, Int)) = {
    val delta = (n - i) - (m - j)

    vForward(zero + 1) = 0
    vReverse(zero + 1) = 0

    val maxdist = (m + n + 1) / 2
    var d = 0               // for (d <- 0 to (m + n + 1) / 2)
    while (d <= maxdist) {
      var k = -d            // for (k <- -d to d by 2)
      while (k <= d) {
        val mid = zero + k
        var x = if (k == -d || (k != d && vForward(mid-1) < vForward(mid+1)))
          vForward(mid + 1)
        else
          vForward(mid - 1) + 1
        var y = x - k
        val xorig = x
        while (i + x < n && j + y < m && a(i + x) == b(j + y)) {
          x += 1
          y += 1
        }
        vForward(mid) = x

        if ((delta % 2) != 0 && -(d-1) <= delta - k && delta - k <= d-1) {
          if (x + vReverse(zero + delta - k) >= n - i) {
            return (2*d - 1, (i + xorig, i + x, j + xorig - k))
          }
        }

        k += 2
      }

      k = -d                // for (k <- -d to d by 2)
      while (k <= d) {
        val mid = zero + k
        var x = if (k == -d || (k != d && vReverse(mid-1) < vReverse(mid+1)))
          vReverse(mid + 1)
        else
          vReverse(mid - 1) + 1
        var y = x - k
        val xorig = x
        while (i <= n-1-x && j <= m-1-y && a(n-1-x) == b(m-1-y)) {
          x += 1
          y += 1
        }
        vReverse(mid) = x

        if ((delta % 2 == 0) && (-d <= delta - k && delta - k <= d)) {
          if (x + vForward(zero + delta - k) >= n - i) {
            return (2*d, (n - x, n - xorig, m - y))
          }
        }

        k += 2
      }

      d += 1
    }

    ???
  }

  /** Convert a list of common substring indices into a diff.
    *
    * @return a diff of `a` and `b` based on `initialCommonSubstringIndices`.
    * If `initialCommonSubstringIndices` contains no empty runs, the result
    * will not contain an `Insert(_)` followed by a `Delete(_)`, i.e. in
    * difference regions it puts characters from `a` before those from `b`.
    *
    * If `initialCommonSubstringIndices` does not correspond to a common
    * subsequence of `a` and `b`, hilarity may ensue.
    */
  def commonSubstringIndicesToDiff(
    a: String,
    b: String,
    initialCommonSubstringIndices: List[(Int, Int, Int)]
  ): Seq[Diff[Char]] = {
    val (n, m) = (a.size, b.size)

    var (i, j) = (0, 0)
    var commonSubstringIndices = initialCommonSubstringIndices
    val buffer = scala.collection.mutable.ArrayBuffer[Diff[Char]]()

    while (i < n || j < m) {
      commonSubstringIndices match {
        case (xlo, xhi, ylo) :: tail =>
          commonSubstringIndices = tail
          for (x <- Range(i,   xlo)) buffer += Delete(a(x))
          for (y <- Range(j,   ylo)) buffer += Insert(b(y))
          for (x <- Range(xlo, xhi)) buffer += Common(a(x))
          i = xhi
          j = ylo + (xhi - xlo)
        case Nil =>
          while (i < n) { buffer += Delete(a(i)); i += 1 }
          while (j < m) { buffer += Insert(b(j)); j += 1 }
      }
    }

    buffer.toVector
  }
}
