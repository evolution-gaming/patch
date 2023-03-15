package com.evolution.patch

import cats.Id
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import com.evolution.patch.Patch.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class PatchTest extends AnyFunSuite with Matchers {

  test("all") {
    implicit val P      = Patch.Maker[Id, Int, String]
    implicit val change = Patch.Change[Int, String] { (state, _, _) => (state + 1).pure[Id] }
    val patch = for {
      _ <- ().patch[Id, Int, String]
      a <- P.lift { "a".pure[Id] }
      _ <- P.effect { "f".pure[Id] }
      _ <- P.event("e0")
      _ <- P.state
      _ <- P.seqNr
      _ <- P.change { (s, _) => (s + 1, "e1", (), ()).pure[Id] }
    } yield a
    patch
      .run(0, SeqNr.Min) shouldEqual Patch
      .Result
      .apply(2, 2L, List("e0", "e1"), "f", "a")
      .pure[Id]
  }

  test("all complex") {
    implicit val P      = Patch.Maker[Id, SeqNr, String]
    implicit val change = Patch.Change[SeqNr, String] { (s, _, _) => (s + 1).pure[Id] }

    def lift[A](a: A) = {
      for {
        _ <- ().patchEffect
        a <- a.patch
        _ <- ().patchEffect
        a <- a.pure[Id].patchLift
        _ <- ().patchEffect
      } yield a
    }
    val patch0 = for {
      a <- P.seqNr
      a <- lift(a)
      _ <- P.event(a.toString)
      b <- P.state
      b <- lift(b)
      c <- P.seqNr
      _ <- c.toString.patchEffect
      d <- P.change { (s, seqNr) => (s + 1, seqNr.toString, (), (a, b, seqNr.toString)).pure[Id] }
      d <- lift(d)
    } yield d

    val patch1 = for {
      a <- patch0
      a <- lift(a)

      x <- P.seqNr
      _ <- P.event(s"x$x")

      b <- patch0
      b <- lift(b)

      x <- P.seqNr
      _ <- P.event(s"x$x")

      c <- patch0
      c <- lift(c)
    } yield {
      (a, b, c)
    }

    patch1
      .run(0L, SeqNr.Min) shouldEqual Patch
      .Result
      .apply(
        state  = 8L,
        seqNr  = 8L,
        events = List("0", "1", "x2", "3", "4", "x5", "6", "7"),
        effect = ("1", ("4", "7")),
        value  = ((0L, 1L, "1"), (3L, 4L, "4"), (6L, 7L, "7"))
      )
      .pure[Id]
  }

  test("map/flatMap") {
    implicit val P = Patch.Maker[IO, Int, String]

    val result = for {
      logs <- Ref[IO].of(List.empty[String])
      result <- {
        val inc = P.apply { (state, seqNr) =>
          val effect = logs.update { "+1" :: _ }
          ((state + 1, "inc").some, effect, seqNr).pure[IO]
        }

        val dec = P.apply { (state, seqNr) =>
          val effect = logs.update { "-1" :: _ }
          ((state - 1, "dec").some, effect, seqNr).pure[IO]
        }

        val empty = P.apply { (_, seqNr) =>
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
        patch.run(0, 1L)
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
    implicit val P = Patch.Maker[Id, Unit, Unit]
    ()
      .patch
      .run((), SeqNr.Min) shouldEqual Patch
      .Result
      .apply((), SeqNr.Min, List.empty, (), ())
      .pure[Id]
  }

  test("lift") {
    implicit val P = Patch.Maker[Id, Unit, Int]
    "a"
      .pure[Id]
      .patchLift
      .run((), SeqNr.Min) shouldEqual Patch
      .Result
      .apply((), SeqNr.Min, List.empty, (), "a")
      .pure[Id]
  }

  test("map") {
    implicit val P = Patch.Maker[Id, Unit, Unit]
    ()
      .patch
      .map { _.toString }
      .run((), SeqNr.Min) shouldEqual Patch
      .Result
      .apply((), SeqNr.Min, List.empty, (), "()")
      .pure[Id]
  }

  test("seqNr") {
    implicit val P      = Patch.Maker[Id, Unit, Unit]
    implicit val change = Patch.Change.const[Unit](().pure[Id])
    val patch = for {
      a <- P.seqNr
      _ <- ().patchEvent
      b <- P.seqNr
      _ <- ().patchEvent
      c <- P.seqNr
    } yield {
      (a, b, c)
    }
    val expected = (10, 11, 12)
    patch
      .run((), 10)
      .value shouldEqual expected
  }

  test("state") {
    implicit val P = Patch.Maker[Id, String, Unit]
    P
      .state
      .run("state", SeqNr.Min) shouldEqual Patch.Result("state", SeqNr.Min, List.empty, (), "state").pure[Id]
  }

  test("event") {
    implicit val P      = Patch.Maker[Id, Int, Int]
    implicit val change = Patch.Change[Int, Int] { (state, _, event) => (state + event).pure[Id] }
    val patch = for {
      a <- P.state
      _ <- 1.patchEvent
      b <- P.state
      _ <- 2.patchEvent
      c <- P.state
    } yield {
      (a, b, c)
    }
    patch.run(0, SeqNr.Min) shouldEqual Patch.Result(3, 2L, List(1, 2), ().pure[Id], (0, 1, 3))
  }

  test("effect") {
    implicit val P = Patch.Maker[Id, Int, Int]
    0
      .patchEffect
      .run(0, SeqNr.Min)
      .effect shouldEqual 0
  }

  test("effect/flatMap") {
    implicit val P = Patch.Maker[Id, Int, Int]
    0
      .patchEffect
      .flatMap { _ => ().patch }
      .run(0, SeqNr.Min)
      .effect shouldEqual 0
  }

  test("combine effect") {
    implicit val P = Patch.Maker[Id, Unit, Unit]
    val patch = for {
      _ <- 0.patchEffect
      _ <- ().patchEffect
      _ <- "a".patchEffect
      _ <- "b".patchEffect
      a <- ().patchEffect
    } yield a
    patch
      .run((), SeqNr.Min)
      .effect shouldEqual (0 -> ("a" -> "b"))
  }

  test("combine monadic effect") {
    implicit val P = Patch.Maker[Id, Unit, Unit]
    val patch = for {
      _ <- 0.some.patchEffect
      _ <- "a".some.patchEffect
    } yield {}
    patch
      .run((), SeqNr.Min)
      .effect shouldEqual (0, "a").some
  }

  test("combine complex effect") {
    implicit val P = Patch.Maker[Id, Unit, Unit]
    val patch = for {
      _ <- 0.some.patchEffect
      _ <- ().patchEffect
      _ <- "a".some.patchEffect
      _ <- ().some.patchEffect
      _ <- "b".some.patchEffect
      _ <- ().some.patchEffect
      _ <- ().patchEffect
    } yield {}
    patch
      .run((), SeqNr.Min)
      .effect shouldEqual (0, ("a", "b")).some
  }

  test("combine unit effect") {
    implicit val P = Patch.Maker[Id, Unit, Unit]
    val patch = for {
      _ <- ().patchEffect
      _ <- ().patchEffect
    } yield {}
    val expected = ()
    patch
      .run((), SeqNr.Min)
      .effect shouldEqual expected
  }

  test("effect with custom derivation") {
    implicit val P                  = Patch.Maker[Id, Unit, Unit]
    val `+` : Derive[Int, Int, Int] = _ + _
    val `-` : Derive[Int, Int, Int] = _ - _
    val patch = 0
      .patchEffect
      .flatMap { _ =>
        1
          .patchEffect
          .flatMap { _ =>
            2
              .patchEffect
              .flatMap { _ =>
                3.patchEffect
              }(implicitly, `-`)
          }(implicitly, `+`)
      }(implicitly, `-`)

    patch
      .run((), SeqNr.Min)
      .effect shouldEqual 0
  }

  test("effect.map") {
    implicit val P = Patch.Maker[Id, Unit, Unit]
    0
      .pure[Try]
      .patchEffect
      .flatMap { _ =>
        "1"
          .pure[Try]
          .patchEffect
      }
      .effect
      .map { _.toOption }
      .run((), SeqNr.Min)
      .effect shouldEqual (0, "1").some
  }

  test("monad.map") {
    implicit val P = Patch.Maker[Id, Unit, Unit]
    0
      .patch
      .monad
      .map {
        new FunctionK[Id, Option] {
          def apply[A](a: Id[A]) = a.some
        }
      }
      .run((), SeqNr.Min)
      .map { _.state } shouldEqual ().some
  }

  test("orElse") {
    implicit val P = Patch.Maker[Either[Unit, *], Int, Int]
    implicit val change = Patch.Change[Int, Int] { (s, _, e) =>
      val s1 = s + e
      if (s1 >= 0) s1.asRight[Unit]
      else ().asLeft[Int]
    }
    (-1)
      .patchEvent
      .orElse { 1.patchEvent }
      .run(0, SeqNr.Min) shouldEqual Patch.Result(1, 1, List(1), (), ()).asRight
  }

  test("change") {
    implicit val P = Patch.Maker[Id, Long, String]
    P
      .change { (s, _) => (s + 1, "e", "f", "a").pure[Id] }
      .run(0, SeqNr.Min) shouldEqual Patch.Result(1, 1L, List("e"), "f", "a")
  }

  test("change/flatMap") {
    implicit val P = Patch.Maker[Id, List[(String, SeqNr)], SeqNr]

    def change(a: String) = {
      P.change { (s, seqNr) =>
        ((a, seqNr) :: s, seqNr, seqNr.pure[Id], seqNr).pure[Id]
      }
    }

    val patch = for {
      _ <- for {
        _ <- change("a")
        _ <- change("b")
      } yield {}
      _ <- change("c")
    } yield {}

    patch
      .run(List.empty, SeqNr.Min) shouldEqual Patch
      .Result
      .apply(
        state  = List(("c", 2), ("b", 1), ("a", 0)),
        seqNr  = 3L,
        events = List(0, 1, 2),
        effect = ((0, 1), 2),
        value  = ()
      )
  }

  test("apply") {
    implicit val P = Patch.Maker[Id, Long, String]
    P
      .apply { (s, _) => ((s + 1, "e").some, "f".pure[Id], "a").pure[Id] }
      .run(0, SeqNr.Min) shouldEqual Patch.Result(1, 1L, List("e"), "f".pure[Id], "a")
  }

  test("flatMap stacksafe") {
    implicit val P      = Patch.Maker[Id, Unit, Unit]
    implicit val change = Patch.Change[Unit, Unit] { (_, _, _) => ().pure[Id] }
    val patch = for {
      _ <- P.state
      _ <- P.event(())
    } yield 1
    val count = 10000
    List
      .fill(count) { patch }
      .foldLeft(0.patch) {
        case (a, b) =>
          for {
            a <- a
            b <- b
          } yield {
            a + b
          }
      }
      .run((), SeqNr.Min) shouldEqual Patch.Result((), count.toLong, List.fill(count) { () }, (), count)
  }

  test("tailRecM stacksafe") {
    implicit val P      = Patch.Maker[IO, Unit, Unit]
    implicit val change = Patch.Change[Unit, Unit] { (_, _, _) => ().pure[IO] }
    val patch = for {
      _ <- P.state
      _ <- P.event(())
    } yield 1
    val count = 10000
    List
      .fill(count) { patch }
      .foldA
      .run((), SeqNr.Min)
      .unsafeRunSync() shouldEqual Patch.Result((), count.toLong, List.fill(count) { () }, (), count)
  }
}