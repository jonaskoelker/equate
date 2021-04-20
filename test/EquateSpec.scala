package equate

import scalatest.Equate
import org.scalatest.{Matchers, WordSpec}

class EquateSpec extends WordSpec with Matchers with Equate {
  "equate" should {
    "succeed" when {
      "inputs are equal" in {
        2 + 2 should equate (4)
      }
    }
    "fail" when {
      "inputs are unequal" in {
        2 + 2 should not (equate (5))
      }
    }
  }

  // I want to write `observed should equate (expected)` in my tests, even
  // when the inferred types of `observed` and `expected` are different but
  // sensibly comparable, e.g. `Some[Int]` vs. `Option[Int]`.
  //
  // The point of these tests is to verify that this Just Works(TM).
  "equate" should {
    "compile and succeed on identical types" when {
      "comparing None (inferred) to itself" in {
        val observed               = None
        val expected               = None
        observed should equate (expected)
      }

      "comparing None (manifest) to itself" in {
        val observed: None.type    = None
        val expected: None.type    = None
        observed should equate (expected)
      }

      "comparing Some (inferred) to itself" in {
        val observed               = Some(())
        val expected               = Some(())
        observed should equate (expected)
      }

      "comparing Some (manifest) to itself" in {
        val observed: Some[Unit]   = Some(())
        val expected: Some[Unit]   = Some(())
        observed should equate (expected)
      }

      "comparing None declared as Option[Unit] to iself" in {
        val observed: Option[Unit] = None
        val expected: Option[Unit] = None
        observed should equate (expected)
      }

      "comparing Some declared as Option[Unit] to iself" in {
        val observed: Option[Unit] = Some(())
        val expected: Option[Unit] = Some(())
        observed should equate (expected)
      }

      "comparing None: Option[Unit] to iself" in {
        val observed               = None: Option[Unit]
        val expected               = None: Option[Unit]
        observed should equate (expected)
      }

      "comparing Some: Option[Unit] to iself" in {
        val observed               = Some(()): Option[Unit]
        val expected               = Some(()): Option[Unit]
        observed should equate (expected)
      }

    }

    "compile and succeed on sub-/supertypes" when {
      "comparing None (inferred) to Option" in {
        val observed               = None
        val expected: Option[Unit] = None
        observed should equate (expected)
      }

      "comparing None (manifest) to Option" in {
        val observed: None.type    = None
        val expected: Option[Unit] = None
        observed should equate (expected)
      }

      "comparing Some (inferred) to Option" in {
        val observed               = Some(())
        val expected: Option[Unit] = Some(())
        observed should equate (expected)
      }

      "comparing Some (manifest) to Option" in {
        val observed: Some[Unit]   = Some(())
        val expected: Option[Unit] = Some(())
        observed should equate (expected)
      }

      "comparing Option to None (inferred)" in {
        val observed: Option[Unit] = None
        val expected               = None
        observed should equate (expected)
      }

      "comparing Option to None (manifest)" in {
        val observed: Option[Unit] = None
        val expected: None.type    = None
        observed should equate (expected)
      }

      "comparing Option to Some (inferred)" in {
        val expected: Option[Unit] = Some(())
        val observed               = Some(())
        observed should equate (expected)
      }

      "comparing Option to Some (manifest)" in {
        val expected: Option[Unit] = Some(())
        val observed: Some[Unit]   = Some(())
        observed should equate (expected)
      }

    }
  }
}
