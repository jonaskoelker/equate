package equate

import Algorithms.{
  commonSubstringIndicesToDiff,
  findMiddle,
  lcsSubstringIndices,
  longestCommonSubsequenceEdits
}
import Presentation.renderDiff
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Shrink}
import scalatest.Equate.equate
import scalacheck.Equate.?==

object Tests extends Properties("Equate") {

  val randomAlphabet: Gen[Gen[Char]] = Gen.frequency(
    (7, Gen.oneOf[Char]("ACGT")),
    (1, Arbitrary.arbChar.arbitrary)
  )

  implicit val arbitraryChar: Arbitrary[Char] =
    Arbitrary(randomAlphabet.flatMap(identity))

  implicit val arbitraryString: Arbitrary[String] =
    Arbitrary { randomAlphabet.flatMap(Gen.listOf(_).map(_.mkString)) }

  implicit val shrinkChar: Shrink[Char] = Shrink {
    c => implicitly[Shrink[Int]]
      .shrink(c.toInt - '0')
      .map(_ + '0')
      .filter(i => 0 <= i && i <= Char.MaxValue)
      .map(_.toChar)
  }

  implicit val shrinkString: Shrink[String] = Shrink {
    s => Shrink.shrinkContainer[List, Char].shrink(s.toList).map(_.mkString)
  }

  property("equate(a, b) doesn't explode on large inputs") = {
    val start = ' '.toInt
    val length = 159 * 37 // "one metric screenful" = my tmux resolution
    assert(length == 5883)
    val a = Range(start, start+length).map(_.toChar).mkString
    val b = a.reverse // a rather bad case for the diff algorithm
    Prop.within(5000) { !equate(a)(b).matches }
  }

  property("|myers(a, b)| = |wagnerFischer(a, b)|") = forAll {
    (a: String, b: String) =>
    val lhs = longestCommonSubsequenceEdits(a, b)
    val rhs = Oracles.wagnerFischer(a, b)
    s"lhs: ${lhs}" |: s"rhs: ${rhs}" |: (lhs.size ?= rhs.size)
  }

  // Conceptually we work with three functions, [dist, lcs, edits], each of
  // which can be defined in terms of any other, although dist will have to be
  // called multiple times to compute lcs or edits.
  //
  //  - dist(a, b) is the LCS edit distance between a and b.
  //  - lcs(a, b) is the longest common subsequence of a and b.
  //  - edits(a, b) is lcs(a, b) plus the a- and b-specific parts.
  //
  // Note that dist has only one correct value, whereas lcs and edits return
  // an arbitrary optimal solution.  There may be more than one lcs with a
  // given dist, and more than one edits for any given lcs.  In particular,
  // any region of differences (bounded by shared characters or string ends)
  // can be reordered freely subject to maintaining the relative order among
  // all the a-parts and the relative order among all the b-parts.  In other
  // words, we can swap any neighbouring Delete(ca) and Insert(cb).
  //
  // Note that |a| + |b| = 2*|lcs(a, b)| + dist(a, b), so dist and |lcs| can
  // be defined in terms of each other.  Properties of dist thus correspond to
  // properties of |lcs|.
  //
  // We also work (a tiny bit) with a fourth function, diff(a, b), which turns
  // edits(a, b) into a human-friendly textual representation.

  def dist[T](edits: Seq[Diff[T]]): Int = {
    edits.count {
      case Common(_) => false
      case _         => true
    }
  }

  def dist[T](a: String, b: String): Int = {
    dist(longestCommonSubsequenceEdits(a, b))
  }

  property("""dist("XMJYAUZ", "MZJAWXU") = 6""") = {
    dist("XMJYAUZ", "MZJAWXU") ?= 6
  }

  // begin: dist is a metric
  property("dist(a, a) = 0") = forAll {
    (a: String) => dist(a, a) ?= 0
  }

  property("dist(a, b) > 0 if a != b") = forAll {
    (a: String, b: String) => (a != b) ==> (dist(a, b) > 0)
  }

  property("dist(a, b) = dist(b, a)") = forAll {
    (a: String, b: String) => dist(a, b) ?= dist(b, a)
  }

  property("dist(a, c) <= dist(a, b) + dist(b, c)") = forAll {
    (a: String, b: String, c: String) =>
    val abEdits = longestCommonSubsequenceEdits(a, b)
    val bcEdits = longestCommonSubsequenceEdits(b, c)
    val acEdits = longestCommonSubsequenceEdits(a, c)

    s"abEdits: ${abEdits}" |:
    s"bcEdits: ${bcEdits}" |:
    s"acEdits: ${acEdits}" |:
    dist(acEdits) <= dist(abEdits) + dist(bcEdits)
  }
  // end: dist is a metric

  property("""dist(a, "") = dist("", a) = a.size""") = forAll {
    (a: String) =>
    val (aLeft, aRight) = (dist(a, ""), dist("", a))
    (aLeft ?= a.size) && (aRight ?= a.size)
  }

  // begin: optimal suffix substructure
  property("dist(xs + a, ys + a) = dist(xs, ys)") = forAll {
    (xs: String, ys: String, a: Char) =>
    dist(xs + a, ys + a) ?= dist(xs, ys)
  }

  property("dist(xs+x,ys+y)-1 in {dist(xs+x,ys),dist(xs,ys+y)} if x != y") =
    forAll {
      (xs: String, ys: String, x: Char, y: Char) => (x != y) ==> {
        val (xsx, ysy) = (xs + x, ys + y)
        val d = dist(xsx, ysy)
        (d ?= 1 + dist(xsx, ys)) || (d ?= 1 + dist(xs, ysy))
      }
    }
  // end: optimal suffix substructure

  // this plus optimal suffix substructure gives optimal prefix substructure
  property("dist(a, b) = dist(a.reverse, b.reverse)") = forAll {
    (a: String, b: String) => dist(a.reverse, b.reverse) ?= dist(a, b)
  }

  // Every pair of strings defines something which is almost a metric space
  // on [0..a.size] x [0..b.size].  The subspace [i1..i2) x [j1..j2) is
  // isomorphic to the space on (a.substring(i1, i2), b.substring(j1, j2)).
  // 
  // The distance in this almost-metric-space corresponds to the length of the
  // shortest path from (i1, j1) to (i2, j2) in the graph corresponding to the
  // dynamic programming table, with zero-cost edges where a(i) = b(j).
  //
  // Any zero-cost edges imply dist((i, j), (i+1, j+1)) = 0 even though
  // (i, j) != (i+1, j+1), which violates the definition of a metric space.
  //
  // But at least we satisfy the triangle inequality:
  property("dist(a ++ b, x ++ y) <= dist(a, x) + dist(b, y)") = forAll {
    // This implies dist(x, y) <= dist(x, "") + dist("", y).  Given an earlier
    // property, this implies dist(x, y) <= x.size + y.size.  Equality can be
    // achieved by deleting all of x, then inserting all of y, if |lcs| = 0.

    // Furthermore,
    //     dist(p + x + s, p + y + s)
    //  <= dist(p, p) + dist(x, y) + dist(s, s)
    //   = dist(x, y)

    // Every prefix of a diff corresponds to a subdivision of a+b and x+y such
    // that dist(a ++ b, x ++ y) = dist(a, x) + dist(b, y), see below.

    (a: String, b: String, x: String, y: String) =>
    val (s, t) = (a + b, x + y)

    val axEdits = longestCommonSubsequenceEdits(a, x)
    val byEdits = longestCommonSubsequenceEdits(b, y)
    val stEdits = longestCommonSubsequenceEdits(s, t)

    s"axEdits: ${axEdits}" |:
    s"byEdits: ${byEdits}" |:
    s"stEdits: ${stEdits}" |:
    dist(stEdits) <= dist(axEdits) + dist(byEdits)
  }

  property("|a.size - b.size| <= dist(a, b)") = forAll {
    (a: String, b: String) => Math.abs(a.size - b.size) <= dist(a, b)
    // No overflow since a.size >= 0 and b.size >= 0,
    // and thus Integer.MIN_VALUE < a.size - b.size,
    // and thus (a.size - b.size).abs >= 0.
  }

  def lcs[T](edits: Seq[Diff[T]]): Seq[T] = {
    edits.collect { case Common(element) => element }
  }

  def lcs(a: String, b: String): Seq[Char] = {
    lcs(longestCommonSubsequenceEdits(a, b))
  }

  property("|a| + |b| = 2*|lcs(a, b)| + dist(a, b)") = forAll {
    (a: String, b: String) =>
    val edits = longestCommonSubsequenceEdits(a, b)
    val theLcs = lcs(edits)
    val distance = dist(edits)
    s"a.size: ${a.size}" |:
    s"b.size: ${b.size}" |:
    s"theLcs: ${theLcs}" |:
    s"distance: ${distance}" |:
    { (2*theLcs.size + distance) ?= (a.size + b.size) }
  }

  property("""lcs("XMJYAUZ", "MZJAWXU") = "MJAU"""") = {
    lcs("XMJYAUZ", "MZJAWXU").mkString ?= "MJAU"
  }

  property("lcs(a, a) = a") = forAll {
    (a: String) =>
    val theLcs = lcs(longestCommonSubsequenceEdits(a, a))
    theLcs.mkString ?= a
  }

  def isSubsequence[T](needle: List[T], haystack: List[T]): Boolean = {
    (needle, haystack) match {
      case (Nil,   _    )           => true
      case (_,     Nil  )           => false
      case (x::xs, y::ys) if x == y => isSubsequence(xs,     ys)
      case (_,     _::ys)           => isSubsequence(needle, ys)
    }
  }

  property("lcs(a, b) is a subsequence of both a and b") = forAll {
    (a: String, b: String) =>
    val theLcs = lcs(longestCommonSubsequenceEdits(a, b)).toList

    s"lcs(a, b): ${theLcs}" |:
    (isSubsequence(theLcs, a.toList) && isSubsequence(theLcs, b.toList))
  }

  val stringWithSubsequence = for {
    a    <- Arbitrary.arbitrary[String]
    bits <- Gen.listOfN(a.size, Arbitrary.arbitrary[Boolean])
    b     = a.zip(bits).collect { case (c, true) => c }.mkString
  } yield (a, b)

  property("lcs(x, y) = y if y is a subsequence of x") =
    forAllNoShrink(stringWithSubsequence) { case (x, y) => lcs(x, y) ?= y }

  property("lcs(x, y) = x if x is a subsequence of y") =
    forAllNoShrink(stringWithSubsequence) { case (y, x) => lcs(x, y) ?= x }

  // begin: not a feature of the LCS but of the particular implementation
  property("lcs(xs + a, ys + a) = lcs(xs, ys) + a") = forAll {
    (xs: String, ys: String, a: Char) =>
    lcs(xs + a, ys + a) ?= lcs(xs, ys) :+ a
  }

  property("lcs(a + xs, a + ys) = a + lcs(xs, ys)") = forAll {
    (xs: String, ys: String, a: Char) =>
    lcs(a + xs, a + ys) ?= a +: lcs(xs, ys)
  }
  // end: not a feature of the LCS but of the particular implementation

  property("a != lcs(a, b) != b if a ! = b") = forAll {
    (a: String, b: String) => (a != b) ==> {
      val theLcs = lcs(longestCommonSubsequenceEdits(a, b))
      s"lcs: ${theLcs}" |: { (a != theLcs) && (theLcs != b) }
    }
  }

  // begin: these are not requirements, just counterexamples worth knowing
  {
    val (a, b) = ("01", "10")

    property(s"lcs(a, b) != lcs(b, a) for (a, b) = (${a}, ${b})") = {
      lcs(a, b) != lcs(b, a)
    }

    property(s"lcs(${a}, ${b}) != lcs(${a}.reverse, ${b}.reverse)") = {
      lcs(a, b) != lcs(a.reverse, b.reverse)
    }
  }
  // end: these are not requirements, just counterexamples worth knowing

  def reconstructObserved[T](edits: Seq[Diff[T]]): Seq[T] = edits.collect {
    case Common(c) => c
    case Delete(c) => c
  }

  def reconstructExpected[T](edits: Seq[Diff[T]]): Seq[T] = edits.collect {
    case Common(c) => c
    case Insert(c) => c
  }

  property("""edits("XMJYAUZ", "MZJAWXU") == -X M +Z J -Y A +W +X U -Z""") = {
    val observed = longestCommonSubsequenceEdits("XMJYAUZ", "MZJAWXU")
    val expected = Seq(
      Delete('X'),
      Common('M'),
      Insert('Z'),
      Common('J'),
      Delete('Y'),
      Common('A'),
      Insert('W'),
      Insert('X'),
      Common('U'),
      Delete('Z')
    )
    observed ?= expected
  }

  property("edits(a, b) is injective") = forAll {
    (a: String, b: String) =>
    val edits = longestCommonSubsequenceEdits(a, b)
    val a2 = reconstructObserved(edits)
    val b2 = reconstructExpected(edits)

    s"edits: ${edits}" |:
    s"a2: ${a2}" |:
    s"b2: ${b2}" |:
    { ((a2.mkString, b2.mkString)) ?= ((a, b)) }
  }

  def noInsertThenDelete(edits: Seq[Diff[Char]]): Boolean = {
    edits.zip(edits.drop(1)).forall {
      case (Insert(_), Delete(_)) => false
      case _                      => true
    }
  }

  // This makes it easier to render the diffs I want to render
  property("edits(a, b) deletes from a before inserting from b") = forAll {
    (a: String, b: String) =>
    noInsertThenDelete(longestCommonSubsequenceEdits(a, b))
  }

  property("edits() prefixes/suffixes optimally solve subproblems") = forAll {
    (ab: String, xy: String) =>
    val edits = longestCommonSubsequenceEdits(ab, xy)
    val splits = (1 until edits.size).map(edits.splitAt)
    all(splits.map({
      case (prefix, suffix) =>
        val a = reconstructObserved(prefix).mkString
        val b = reconstructObserved(suffix).mkString
        val x = reconstructExpected(prefix).mkString
        val y = reconstructExpected(suffix).mkString

        s"a: ${a}" |:
        s"b: ${b}" |:
        s"x: ${x}" |:
        s"y: ${y}" |:
        all(
          a + b ?= ab,
          x + y ?= xy,
          dist(a, x) + dist(b, y) ?= dist(edits)
        )
    }): _*)
  }

  implicit val arbitraryEdits: Arbitrary[Diff[Char]] = Arbitrary {
    Arbitrary.arbitrary[Char]
      .flatMap(c => Gen.oneOf(Common(c), Insert(c), Delete(c)))
  }

  // Important: the two inputs can be read off of the printed output!
  property("diff(a, b) is injective") = forAll {
    (edits: Seq[Diff[Char]]) =>
    val Seq(observed, expected, diff) = renderDiff(edits)

    val a1 = reconstructObserved(edits)
    val b1 = reconstructExpected(edits)
    val a2 = observed.zip(diff).collect { case (c, '^' | 'o') => c }
    val b2 = expected.zip(diff).collect { case (c, '^' | 'e') => c }

    s"diff: ${diff}" |:
    s"a2: ${a2}" |:
    s"b2: ${b2}" |:
    { (a1 ?= a2) && (b1 ?= b2) }
  }

  // There's more than one optimal solution.  I judge that skewing towards
  // showing observed before expected gives the best usability.
  property("diff(a, b) deletes from a before inserting from b") = forAll {
    (a: String, b: String) =>
    val edits = longestCommonSubsequenceEdits(a, b)
    val diff = renderDiff(edits)(2).mkString
    s"diff: ${diff}" |: !diff.contains("eo")
  }

  // just for the lols of demonstrating that stuff works on custom types
  case class Person(name: String, age: Int)
  implicit val arbitraryPerson = Arbitrary { Gen.resultOf(Person) }

  property("a ?== a") = forAll { (p: Person) => p ?== p }

  property("equate(x)(x).matches") = forAll {
    (x: Either[(Int, Int), String]) => equate(x)(x).matches
  }

  property("!equate(x)(y).matches if x != y") = forAll {
    (x: Option[Char], y: Option[Char]) => (x != y) ==> !equate(y)(x).matches
  }

  property("isSubsequence({a,b}.toString, equate(a, b).failMsg)") = forAll {
    (a: List[Person], b: List[Person]) =>
    val matchResult = equate(a)(b)
    (!matchResult.matches) ==> {
      val failureMessage = matchResult.rawFailureMessage.toList
      val aOk = isSubsequence(a.toString.toList, failureMessage)
      val bOk = isSubsequence(b.toString.toList, failureMessage)
      ((aOk, bOk)) ?= ((true, true))
    }
  }

  //////////////////////////////////////////////////////////////////////

  // Here we test the internals of Myers' algorithm.  If there's any error it
  // should manifest and be detectable in the diff ultimately computed; these
  // tests are here primarily to narrow the search for the error cause.

  property("lcsSubstringIndices(a, b).forall(common substring)") = forAll {
    (a: String, b: String) =>
    all(lcsSubstringIndices(a, b).map {
      case (xlo, xhi, ylo) =>
        a.substring(xlo, xhi) ?= b.substring(ylo, ylo + xhi - xlo)
    }: _*)
  }

  property("lcsSubstringIndices(a, b) => monotonically increasing") = forAll {
    (a: String, b: String) =>
    val indices = lcsSubstringIndices(a, b)
    val aIndices = indices.flatMap { case (lo, hi, _) => Seq(lo, hi) }
    val bIndices = indices.map(_._3)
    (aIndices ?= aIndices.sorted) && (bIndices ?= bIndices.sorted)
  }

  property("findMiddle(a, b) = (dist(a, b), any common substring)") = forAll {
    // arrange
    (a: String, b: String) => (a != "" && b != "") ==> {
      val (n, m) = (a.size, b.size)
      val limit = n + m
      val vForward = new Array[Int](2*limit + 1)
      val vReverse = new Array[Int](2*limit + 1)

      // do stuff
      val (dist, commonSubstring) =
        findMiddle(a, b, limit, vForward, vReverse, 0, n, 0, m)
      val (xlo, xhi, ylo) = commonSubstring

      // trust but verify
      val diff = Oracles.wagnerFischer(a, b)
      val aSubstring = a.substring(xlo, xhi)
      val bSubstring = b.substring(ylo, ylo + xhi - xlo)

      s"commonSubstring: ${commonSubstring}" |:
      s"aSubstring: ${aSubstring}" |:
      s"bSubstring: ${bSubstring}" |:
      s"dist: ${dist}" |:
      s"diff = ${diff}" |:
      { ((n + m + dist) ?= (2 * diff.size)) && (aSubstring ?= bSubstring) }
      // diff.size = lcs.size + dist = (#Common) + (#Insert and #Delete)
      // |a| + |b| = 2*|lcs| + dist(a, b)
      // so |a| + |b| = 2*diff.size - dist
      // and thus |a| + |b| + dist = 2*diff.size
    }
  }

  val genFindMiddleTranslationInvariance = for {
    a <- Arbitrary.arbitrary[String].filter(_.nonEmpty)
    b <- Arbitrary.arbitrary[String].filter(_.nonEmpty)
    i <- Gen.choose(0, a.size)
    n <- Gen.choose(i, a.size)
    j <- Gen.choose(0, b.size)
    m <- Gen.choose(j, b.size)
  } yield (a, b, i, n, j, m)

  // Stateless means independent of array contents, i.e. they can be reused.
  // The reason we pass them in is to perform two rather than k allocations.
  property("findMiddle is translation invariant (and de facto stateless)") =
    forAll(genFindMiddleTranslationInvariance) {
      case (a, b, i, n, j, m) =>
        val limit = a.size + b.size
        val vForward = new Array[Int](2*limit + 1)
        val vReverse = new Array[Int](2*limit + 1)

        val result1 =
          findMiddle(a, b, limit, vForward, vReverse, i, n, j, m)

        val result2 =
          findMiddle(
            a.substring(i, n),
            b.substring(j, m),
            limit, vForward, vReverse,
            0,
            n - i,
            0,
            m - j
          )

        val (dist1, (xlo1, xhi1, ylo1)) = result1
        val translated = (dist1, (xlo1 - i, xhi1 - i, ylo1 - j))

        s"result1: ${result1}" |:
        s"translated: ${translated}" |:
        s"result2: ${result2}" |:
        { translated ?= result2 }
    }


  property("commonSubstringIndicesToDiff is (approximately) invertible") = {
    forAllShrink(deleteThenInsert, PropHelper.filtered(noInsertThenDelete)) {
      (edits: Seq[Diff[Char]]) =>
      require(noInsertThenDelete(edits))

      val a = reconstructObserved(edits).mkString
      val b = reconstructExpected(edits).mkString
      val runs = substringsFromEdits(edits)

      s"a: ${a}" |: s"b: ${b}" |: s"runs: ${runs}" |:
      { commonSubstringIndicesToDiff(a, b, runs) ?= edits }
    }
  }

  /** `(D*I*CC*)*(D*I*)` where D=Delete, I=Insert, C=Common */
  val deleteThenInsert: Gen[Seq[Diff[Char]]] = {

    import PropHelper.{concat, star}

    val delete = Arbitrary.arbitrary[Char].map(Delete(_)).map(List(_))
    val insert = Arbitrary.arbitrary[Char].map(Insert(_)).map(List(_))
    val common = Arbitrary.arbitrary[Char].map(Common(_)).map(List(_))

    val deleteThenInsert = concat(star(delete), star(insert))
    val atLeastOneCommon = concat(common, star(common))

    concat(
      deleteThenInsert,
      star(concat(atLeastOneCommon, deleteThenInsert))
    )
  }

  def substringsFromEdits(edits: Seq[Diff[Char]]): List[(Int, Int, Int)] = {

    def recur(
      xylo: Option[(Int, Int)], // Some(start) iff inside a common substring
      xhi: Int,                 // index into a, esp. at common substring end
      yhi: Int,                 // index into b, esp. at common substring end
      deltas: List[Diff[Char]]  // edits.drop(k) for some k <= xhi + yhi
    ): List[(Int, Int, Int)] = (xylo, deltas) match {
      case (None, Nil)               => Nil
      case (None, Delete(_) :: tail) => recur(xylo, xhi + 1, yhi, tail)
      case (None, Insert(_) :: tail) => recur(xylo, xhi, yhi + 1, tail)

      case (None,             Common(_) :: tail) =>
        recur(Some((xhi, yhi)), xhi + 1, yhi + 1, tail)
      case (Some(_),          Common(_) :: tail) =>
        recur(xylo, xhi + 1, yhi + 1, tail)
      case (Some((xlo, ylo)), _)                 =>
        (xlo, xhi, ylo) :: recur(None, xhi, yhi, deltas)
    }

    recur(None, 0, 0, edits.toList)
  }
}

object Oracles {
  // This is too slow on large inputs to unleash upon the world, but it's a
  // perfectly fine testing oracle on moderately sized inputs.
  // See http://www.inrg.csie.ntu.edu.tw/algorithm2014/homework/Wagner-74.pdf
  def wagnerFischer(a: String, b: String): List[Diff[Char]] = {

    val (n, m) = (a.size, b.size)
    val table = Array.fill(n+1)(Array.fill(m+1)(0))

    for (i <- 1 to n; j <- 1 to m) {
      if (a(i-1) == b(j-1)) {
        table(i)(j) = table(i-1)(j-1) + 1             // compute LCS length,
      } else {
        table(i)(j) = table(i-1)(j) max table(i)(j-1) // not edit distance.
      }
    }

    var path: List[Diff[Char]] = Nil
    var (i, j) = (n, m)
    while (i != 0 || j != 0) {
      if (i == 0) {
        path = Insert(b(j-1)) :: path
        j -= 1
      } else if (j == 0) {
        path = Delete(a(i-1)) :: path
        i -= 1
      } else if (a(i-1) == b(j-1)) {
        path = Common(a(i-1)) :: path
        i -= 1
        j -= 1
      } else if (table(i)(j-1) < table(i-1)(j)) {
        path = Delete(a(i-1)) :: path
        i -= 1
      } else {
        path = Insert(b(j-1)) :: path
        j -= 1
      }
    }

    path
  }
}

object PropHelper {
  /** sample `gl` and `gr`, concatenating the results */
  def concat[T](gl: Gen[Seq[T]], gr: Gen[Seq[T]]): Gen[Seq[T]] =
    Gen.zip(gl, gr).map { case (l, r) => l ++ r }

  /** sample `g` any number of times, concatenating the results */
  def star[T](g: Gen[Seq[T]]): Gen[Seq[T]] =
    Gen.listOf(g).map(_.flatten)

  def filtered[T](p: T => Boolean)(implicit s: Shrink[T]): T => Stream[T] =
    x => s.shrink(x).filter(p)
}
