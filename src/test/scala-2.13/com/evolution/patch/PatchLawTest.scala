package com.evolution.patch

import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.MonadTests
import cats.{Eq, Id}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class PatchLawTest extends AnyFunSuite with FunSuiteDiscipline with Configuration {

  type Patch[A] = com.evolution.patch.Patch[Id, Unit, Unit, Unit, A]

  implicit def eqPatch[A: Eq]: Eq[Patch[A]] = (x: Patch[A], y: Patch[A]) => {

    def run(patch: Patch[A]) = patch.run((), SeqNr.Min) { (_, _, _) => () }

    run(x) === run(y)
  }

  implicit def arbitraryPatch[A: Arbitrary]: Arbitrary[Patch[A]] = {
    Arbitrary {
      Arbitrary
        .arbitrary[A]
        .map { a => Patch.pure(a) }
    }
  }

  checkAll("Patch", MonadTests[Patch].monad[Int, Int, String])

  checkAll("Patch", MonoidTests[Patch[Int]].monoid)
}
