package com.evolution.patch

import cats.Id
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class PatchTest extends AnyFunSuite with Matchers {

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
        patch(0, 1L) {
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
    val patch = for {
      _ <- Patch[Int] { (s, _) => ((s + 1, "e").some, ().pure[Id], ()).pure[Try] }
      a <- Patch.pure("a")
    } yield a
    val expected = Patch.Result(1, List("e"), (), "a")
    patch(0, SeqNr.Min) { case (s, _) => (s + 1).pure[Try] } shouldEqual expected.pure[Try]
  }

  test("lift") {
    val patch = for {
      _ <- Patch[Int] { (s, _) => ((s + 1, "e").some, ().pure[Id], ()).pure[Try] }
      a <- Patch.lift("a".pure[Try])
    } yield a
    val expected = Patch.Result(1, List("e"), (), "a")
    patch(0, SeqNr.Min) { case (s, _) => (s + 1).pure[Try] } shouldEqual expected.pure[Try]
  }

  test("map") {
    val expected = Patch.Result(1, List("e"), (), "a")
    Patch[Int] { (s, _) => ((s + 1, "e").some, ().pure[Id], ()).pure[Try] }
      .map { _ => "a"}
      .apply(0, SeqNr.Min) { case (s, _) => (s + 1).pure[Try] } shouldEqual expected.pure[Try]
  }

  test("seqNr") {
    val patch = for {
      _ <- Patch[Int] { (s, _) => ((s + 1, "e").some, ().pure[Id], ()).pure[Try] }
      _ <- Patch[Int] { (s, _) => ((s + 1, "e").some, ().pure[Id], ()).pure[Try] }
      a <- Patch.seqNr
    } yield a
    val expected = Patch.Result(2, List("e", "e"), (), 3)
    patch(0, SeqNr.Min + 1) { case (s, _) => (s + 1).pure[Try] } shouldEqual expected.pure[Try]
  }

  test("state") {
    val patch = for {
      _ <- Patch[Int] { (s, _) => ((s + 1, "e").some, ().pure[Id], ()).pure[Try] }
      a <- Patch.state[Int]
    } yield a
    val expected = Patch.Result(1, List("e"), ().pure[Id], 1)
    patch(0, SeqNr.Min) { case (s, _) => (s + 1).pure[Try] } shouldEqual expected.pure[Try]
  }

  test("event") {

    abstract sealed class E extends Product
    object E {
      case object A extends E
      case object B extends E
    }

    val patch = for {
      _ <- Patch[Int] { (_, _) => (none[(Int, E)], ().pure[Id], ()).pure[Try] }
      a <- Patch.state[Int]
      _ <- Patch.event(E.A)
      b <- Patch.state[Int]
      _ <- Patch.event(E.B)
      c <- Patch.state[Int]
    } yield (a, b, c)
    val expected = Patch.Result(2, List(E.A, E.B), ().pure[Id], (0, 1, 2))
    patch(0, SeqNr.Min) { (s, _) => (s + 1).pure[Try] } shouldEqual expected.pure[Try]
  }

  test("combine effect") {
    val patch: Patch[Id, Int, Int, (Int, (String, String)), Unit] = for {
      _ <- Patch.effect(0)
      _ <- Patch.effect(())
      _ <- Patch.effect("a")
      _ <- Patch.effect("b")
      _ <- Patch.effect(())
    } yield {}

    patch(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }.effect shouldEqual (0 -> ("a" -> "b"))
  }

  test("combine monadic effect") {
    val patch: Patch[Id, Int, Int, Option[(Int, String)], Unit] = for {
      _ <- Patch.effect(0.some)
      _ <- Patch.effect("a".some)
    } yield {}

    patch(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }.effect shouldEqual (0, "a").some
  }

  test("combine complex monadic effect") {
    val patch: Patch[Id, Int, Int, Option[(Int, (String, String))], Unit] = for {
      _ <- Patch.effect(0.some)
      _ <- Patch.effect(())
      _ <- Patch.effect("a".some)
      _ <- Patch.effect(().some)
      _ <- Patch.effect("b".some)
      _ <- Patch.effect(().some)
      _ <- Patch.effect(())
    } yield {}

    patch(0, SeqNr.Min) { (s, _: Int) => (s + 1).pure[Id] }.effect shouldEqual (0, ("a", "b")).some
  }
}