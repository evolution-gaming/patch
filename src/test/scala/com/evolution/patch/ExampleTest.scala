package com.evolution.patch

import cats.effect.IO
import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExampleTest extends AnyFunSuite with Matchers {

  test("example") {

    final case class Event(value: Int)

    final case class State(value: Int)

    def enabled: IO[Boolean] = IO.pure(true)

    def log(msg: String): IO[Unit] = IO.unit

    val patch: Patch[IO, State, Event, IO[Unit], Either[String, State]] = for {
      enabled <- Patch.lift { enabled }
      result  <- if (enabled) {
        for {
          before <- Patch.state[State]
          _      <- Patch.event(Event(+1))
          after  <- Patch.state[State]
          seqNr  <- Patch.seqNr
          _      <- Patch.effect { log(s"state changed from $before to $after($seqNr)") }
        } yield {
          after.asRight[String]
        }
      } else {
        Patch
          .effect { log("state remains the same") }
          .as("disabled".asLeft[State])
      }
    } yield result


    val result = patch.run(State(0), SeqNr.Min) { (state, event) =>
      state
        .copy(value = state.value + event.value)
        .pure[IO]
    }

    result.unsafeRunSync() shouldEqual Patch.Result(State(1), List(Event(1)), IO.unit, State(1).asRight)
  }
}