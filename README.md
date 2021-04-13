# Equate

Equality assertion for scalatest and scalacheck which outputs a diff
of two unequal inputs.

# The purpose

When your scalatest suite fails you might see output like this:

```
[info]   TcpHeader(12345,54321,123456789,234567890,0,false,false,false,false,false,false,false,false,false,12,6354,List()) was not equal to TcpHeader(12345,5421,123456789,234567890,0,false,false,false,false,false,false,true,false,false,12,6354,List()) (TcpSpec.scala:8080)
```

If you want to find out where the two values differ, you have a choice:

 * You can spell your way through all the fields by hand.
 * You can pipe the two strings through e.g. `wdiff` and squint at the
   output of _that_.
 * You can string together `diff <(tr ',' '\n' ob) <(tr ',' '\n' ex)`
   or something like that, after copy-pasting the two values to two
   files named `ob` and `ex`, which takes a while.

Why though?  The computer should just tell you.  So I made it tell
you.  With `Equate` you get output like this:

```
[info] - some test case *** FAILED ***
[info]   Diff legend: '^' in both; 'o' observed only; 'e' expected only.
[info]
[info]   Observed (above) versus expected (below):
[info]
[info]     TcpHeader(12345,54321,123456789,234567890,0,false,false,fals
[info]     TcpHeader(12345,54321,123456789,234567890,0,false,false,fals
[info]     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[info]
[info]     e,false,false,false,fals   e,false,false,12,6354,List())
[info]     e,false,false,false,    true,false,false,12,6354,List())
[info]     ^^^^^^^^^^^^^^^^^^^^ooooeee^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[info]
[info]   good luck :) (TcpSpec.scala:8080)
```

This way, you can quickly spot that the difference is in the third
boolean field from the right.

# How to use `equate`

```
import equate.scalatest.Equate.equate
import myapp.yakshaving.{Yak, shave}
import org.scalatest.{Matchers, WordSpec}

class YakShavingSpec extends WordSpec with Matchers {
  "yak razors" should {
    "shave yaks" in {
      val observed = shave(Yak(hairlength = 5))
      val expected = Yak(hairlength = 1)
      observed should equate (expected)
    }
  }
}
```

Use `equate` the same way you would use `equal`.  If you're like me in
that you use `shouldBe expected` and not `should equal (expected)`,
use `should equate(_)` the same way you would use `shouldBe _`.

Instead of importing `equate` you can mix in `Equate`, like so:

```
import equate.scalatest.Equate
import myapp.yakshaving.{Yak, shave}
import org.scalatest.{Matchers, WordSpec}

class YakShavingSpec extends WordSpec with Matchers with Equate {
  "yak razors" should {
    "shave yaks" in {
      shave(Yak(hairlength=5)) should equate (Yak(hairlength=1))
    }
  }
}
```

There's also support for scalacheck:

```
import equate.scalacheck.Equate.?==
import myapp.yakshaving.{Yak, shave}
import myapp.testhelpers.arbitraryYak
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class YakProperties extends Properties("Yak") {
  property("yak shaving is idempotent (but that never stopped anyone)") =
    forAll { (yak: Yak) => shave(shave(yak)) ?== shave(yak) }
}
```

Use `?==` and `==?` the same way you would use `?=` and `=?`. You only
need to import `?==`: this is an implicit conversion to a wrapper that
supplies both `?==` and `==?` (similar to `Prop.AnyOperators`).

# What exactly does `equate` do?

`equate` asserts that that `observed == expected`, i.e. that its two
inputs are equal.  Otherwise, a diff between `observed.toString` and
`expected.toString` is computed and emitted.

It uses the Myers' linear space bidirectional search algorithm, the
same algorithm as `git` uses (by default), modulo perhaps a few tweaks
of my own.  Fewer differences means faster execution.  The diffs are
calculated character by character, which gets slow if you're comparing
several kilobytes of random or otherwise highly mismatching data.

This algorithm seems like a good choice: I've had Wagner-Fischer take
too long on mere kilobytes of data, and the simpler quadratic space
Myers algorithm exhaust my JVM's memory.  The fancy Myers algorithm
takes ~25 microseconds on the `TcpHeader` example, a fairly typical
workload, and ~200ms comparing two screenfuls (`159*37` characters) of
maximally different data.

# Definition of "equate"

Transitive verb: to treat, represent, or regard as equal, equivalent,
or comparable.

Intransitive verb: to correspond as equal.

# Copyright

Equate is copyright 2021 Jonas Kölker, released under the Apache 2.0
license, see LICENSE.
