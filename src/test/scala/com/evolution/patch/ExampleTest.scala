package com.evolution.patch

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExampleTest extends AnyFunSuite with Matchers {

  test("example") {

    final case class Event(value: Int)

    final case class State(value: Int)

    // we need this to make compiler happy
    implicit val maker = Patch.Maker[IO, State, Event]

    // how to apply newly issued event to state
    implicit val change = Patch.Change[State, Event] { (state, seqNr, event) =>
      state
        .copy(value = state.value + event.value)
        .pure[IO]
    }

    import com.evolution.patch.Patch.implicits._ // adds nice syntax

    def enabled: IO[Boolean] = IO.pure(true)

    def log(msg: String): IO[Unit] = IO.unit

    val patch: Patch[IO, State, Event, IO[Unit], Either[String, State]] = for {
      enabled <- enabled.patchLift // you might need to execute effect in order to decide on how to proceed
      result  <- if (enabled) {
        for {
          before <- Patch.state
          _      <- Event(+1).patchEvent // event to be appended
          after  <- Patch.state // state after event is applied
          seqNr  <- Patch.seqNr // seqNr at this point
          _      <- log(s"state changed from $before to $after($seqNr)").patchEffect
        } yield {
          after.asRight[String]
        }
      } else {
        // you might not produce any events and just have side effect
        log("state remains the same")
          .patchEffect
          .as("disabled".asLeft[State])
      }
    } yield result

    // now we can run our `Patch` by passing initial `state` and `seqNr`
    patch
      .run(State(0), SeqNr.Min)
      .unsafeRunSync() shouldEqual Patch.Result(State(1), 1L, List(Event(1)), IO.unit, State(1).asRight)
  }
}