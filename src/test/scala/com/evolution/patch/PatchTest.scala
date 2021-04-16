package com.evolution.patch

import cats.Id
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class PatchTest extends AnyFunSuite with Matchers {

  test("Pure.toString") {
    Patch
      .pure("pure")
      .toString shouldEqual "Patch(pure)"
  }

  test("Effect.toString") {
    Patch
      .effect("effect")
      .toString shouldEqual "Effect(effect)"
  }

  test("Lift.toString") {
    Patch
      .lift("lift".pure[Id])
      .toString shouldEqual "Lift(lift)"
  }

  test("State.toString") {
    Patch
      .state[Int]
      .toString shouldEqual "State"
  }

  test("SeqNr.toString") {
    Patch
      .seqNr
      .toString shouldEqual "SeqNr"
  }

  test("FlatMap.toString") {
    Patch
      .pure("pure")
      .flatMap {
        new Function1[String, Patch[Nothing, Nothing, Nothing, Unit, Unit]] {
          def apply(a: String) = Patch.unit
          override def toString() = "f"
        }
      }
      .toString shouldEqual "FlatMap(Patch(pure),f,Derive.right)"
  }

  test("Map.toString") {
    Patch
      .pure("pure")
      .map {
        new Function1[String, Unit] {
          def apply(a: String) = ()
          override def toString() = "f"
        }
      }
      .toString shouldEqual "Map(Patch(pure),f)"
  }

  test("all") {
    val patch = for {
      _ <- Patch.pure("")
      a <- Patch.lift("a".pure[Id])
      _ <- Patch.effect("f".pure[Id])
      _ <- Patch.event("e0")
      _ <- Patch.state[Int]
      _ <- Patch.seqNr
      _ <- Patch.change[Int] { (s, _) => (s + 1, "e1", (), ()).pure[Id] }
    } yield a

    val expected = Patch.Result(2, List("e0", "e1"), "f", "a")
    patch
      .narrow[Id, Int, String]
      .run(0, SeqNr.Min) { case (s, _) => (s + 1).pure[Id] } shouldEqual expected.pure[Id]
  }

  test("map/flatMap") {

    val result = for {
      logs   <- Ref[IO].of(List.empty[String])
      result <- {
        val inc = Patch[Int] { (state, seqNr) =>
          val effect = logs.update { "+1" :: _ }
          ((state + 1, "inc").some, effect, seqNr).pure[IO]
        }

        val dec = Patch[Int] { (state, seqNr) =>
          val effect = logs.update { "-1" :: _ }
          ((state - 1, "dec").some, effect, seqNr).pure[IO]
        }

        val empty = Patch[Int] { (_, seqNr) =>
          val effect = logs.update { "empty" :: _ }
          (none[(Int, String)], effect, seqNr).pure[IO]
        }

        val patch = for {
          a <- inc
          b <- dec
          c <- inc
          d <- empty
          e <- inc
        } yield {
          List(a, b, c, d, e)
        }
        patch.run(0, 1L) {
          case (s, "inc") => (s + 1).pure[IO]
          case (s, "dec") => (s - 1).pure[IO]
          case (s, _)     => s.pure[IO]
        }
      }

      _  = result.value shouldEqual List(1L, 2L, 3L, 4L, 4L)
      _  = result.state shouldEqual 2
      _  = result.events shouldEqual List("inc", "dec", "inc", "inc")
      a <- logs.get
      _  = a shouldEqual List.empty
      _ <- result.effect
      a <- logs.get
      _  = a.reverse shouldEqual List("+1", "-1", "+1", "empty", "+1")
    } yield {}

    result.unsafeRunSync()
  }

  test("pure") {
    val expected = Patch.Result((), List.empty, (), "a")
    Patch
      .pure("a")
      .narrow[Id, Unit, Unit]
      .run((), SeqNr.Min) { (_, _) => ().pure[Id] } shouldEqual expected.pure[Id]
  }

  test("lift") {
    val expected = Patch.Result((), List.empty, (), "a")
    Patch
      .lift("a".pure[Id])
      .narrow[Id, Unit, Unit]
      .run((), SeqNr.Min) { (_, _) => ().pure[Id] } shouldEqual expected.pure[Id]
  }

  test("map") {
    val expected = Patch.Result((), List.empty, (), "0")
    Patch
      .pure(0)
      .map { _.toString }
      .narrow[Id, Unit, Unit]
      .run((), SeqNr.Min) { (_, _) => ().pure[Id] } shouldEqual expected.pure[Id]
  }

  test("seqNr") {
    val patch = for {
      a <- Patch.seqNr
      _ <- Patch.event(())
      b <- Patch.seqNr
      _ <- Patch.event(())
      _ <- Patch.event(())
      c <- Patch.seqNr
    } yield {
      (a, b, c)
    }

    val expected = (10, 11, 13)
    patch
      .narrow[Id, Unit, Unit]
      .run((), 10) { (_, _) => ().pure[Id] }
      .value shouldEqual expected
  }

  test("state") {
    val expected = Patch.Result("state", List.empty, (), "state")
    Patch
      .state[String]
      .narrow[Id, String, Unit]
      .run("state", SeqNr.Min) { (s, _) => s.pure[Id] } shouldEqual expected.pure[Id]
  }

  test("event") {

    abstract sealed class E extends Product
    object E {
      case object A extends E
      case object B extends E
    }

    val patch = for {
      a <- Patch.state[Int]
      _ <- Patch.event(E.A)
      b <- Patch.state[Int]
      _ <- Patch.event(E.B)
      c <- Patch.state[Int]
    } yield (a, b, c)
    val expected = Patch.Result(2, List(E.A, E.B), ().pure[Id], (0, 1, 2))
    patch
      .monadNarrow[Try]
      .run(0, SeqNr.Min) { (s, _) => (s + 1).pure[Try] } shouldEqual expected.pure[Try]
  }

  test("effect") {
    Patch
      .effect(0)
      .narrow[Id, Int, Int]
      .run(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }
      .effect shouldEqual 0
  }

  test("effect/flatMap") {
    Patch
      .effect(0)
      .flatMap { _ => Patch.unit }
      .narrow[Id, Int, Int]
      .run(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }
      .effect shouldEqual 0
  }

  test("combine effect") {
    val patch = for {
      _ <- Patch.effect(0)
      _ <- Patch.effect(())
      _ <- Patch.effect("a")
      _ <- Patch.effect("b")
      _ <- Patch.effect(())
    } yield {}

    patch
      .narrow[Id, Int, Int]
      .run(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }
      .effect shouldEqual (0 -> ("a" -> "b"))
  }

  test("combine monadic effect") {
    val patch = for {
      _ <- Patch.effect(0.some)
      _ <- Patch.effect("a".some)
    } yield {}

    patch
      .narrow[Id, Int, Int]
      .run(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }
      .effect shouldEqual (0, "a").some
  }

  test("combine complex effect") {
    val patch = for {
      _ <- Patch.effect(0.some)
      _ <- Patch.effect(())
      _ <- Patch.effect("a".some)
      _ <- Patch.effect(().some)
      _ <- Patch.effect("b".some)
      _ <- Patch.effect(().some)
      _ <- Patch.effect(())
    } yield {}

    patch
      .narrow[Id, Int, Int]
      .run(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }
      .effect shouldEqual (0, ("a", "b")).some
  }

  test("effect with custom derivation") {
    val `+`: Derive[Int, Int, Int] = _ + _
    val `-`: Derive[Int, Int, Int] = _ - _
    val patch = Patch
      .effect(0)
      .flatMap { _ =>
        Patch
          .effect(1)
          .flatMap { _ =>
            Patch
              .effect(2)
              .flatMap { _ =>
                Patch.effect(3)
              }(`-`)
          }(`+`)
      }(`-`)

    patch
      .run(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }
      .effect shouldEqual 0
  }

  test("effectMap") {
    Patch
      .effect(0.pure[Try])
      .flatMap { _ => Patch.effect("1".pure[Try]) }
      .effectMap { _.toOption }
      .narrow[Id, Unit, Unit]
      .run((), SeqNr.Min) { (_, _) => ().pure[Id] }
      .effect shouldEqual (0, "1").some
  }

  test("change") {
    val expected = Patch.Result(1, List("e"), "f".pure[Id], "a")
    Patch
      .change[Int] { case (s, _) => (s + 1, "e", "f".pure[Id], "a").pure[Id] }
      .run(0, SeqNr.Min) { (s, _) => (s + 1).pure[Id] } shouldEqual expected
  }

  test("default") {
    val expected = Patch.Result(1, List("e"), "f".pure[Id], "a")
    Patch
      .apply[Int] { case (s, _) => ((s + 1, "e").some, "f".pure[Id], "a").pure[Id] }
      .run(0, SeqNr.Min) { (s, _) => (s + 1).pure[Id] } shouldEqual expected
  }

  test("stack safe") {
    val patch = Patch.unit.narrow[Id, Int, Int]
    List
      .fill(10000) { patch }
      .foldA
      .run(0, SeqNr.Min) { (s, _) => (s + 1).pure[Id] }
      .state shouldEqual 0
  }

  test("stack safe effect") {
    val patch = Patch.effect(1).narrow[Id, Int, Int]
    val derive: Derive[Int, Int, Int] = (a, b) => a + b
    List
      .fill(10000) { patch }
      .reduceLeft { (a, b) => a.flatMap { _ => b }(derive) }
      .run(0, SeqNr.Min) { (s, _) => s.pure[Id] }
      .effect shouldEqual 10000
  }
}