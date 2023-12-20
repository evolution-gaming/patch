# Patch
[![Build Status](https://github.com/evolution-gaming/patch/workflows/CI/badge.svg)](https://github.com/evolution-gaming/patch/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/evolution-gaming/patch/badge.svg?branch=master)](https://coveralls.io/github/evolution-gaming/patch?branch=master)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/f9d2e05d108c4c259680b4b5f7753001)](https://www.codacy.com/gh/evolution-gaming/patch/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=evolution-gaming/patch&amp;utm_campaign=Badge_Grade)
[![Version](https://img.shields.io/badge/version-click-blue)](https://evolution.jfrog.io/artifactory/api/search/latestVersion?g=com.evolution&a=patch_2.13&repos=public)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellowgreen.svg)](https://opensource.org/licenses/MIT)

`Patch` is a monadic data structure - a building block for [event-sourcing](https://martinfowler.com/eaaDev/EventSourcing.html) application

# Motivation

Event Sourcing is one of the latest advances in software architecture design. It is meant to replace CRUD approach in the areas
where speed of persisting data, scalability and near real-time reactions are essential. The approach does not come without its own
flaws though. It is a complicated task to build a well working Event Sourcing based application and in a lot of cases it is
considered to be an unnecessary overkill.

The modern frameworks such as [Akka Persistence](https://doc.akka.io/docs/akka/current/typed/persistence.html) provide
the user-friendly API and storage implementations, and, therefore, make the task of building Event Sourced software, easier.

The problem is that, however, as you are building the larger application, you realize quickly that these frameworks have
a single important flaw: neither [EventSourcedBehavior](https://doc.akka.io/api/akka/2.8/akka/persistence/typed/scaladsl/EventSourcedBehavior.html),
nor [PersistentActor](https://doc.akka.io/api/akka/2.8/akka/persistence/PersistentActor.html) compose, which is a real shame,
given that Event Sourcing could be really useful for the large and complicated business domains, where high performance and
availability is also essential.

This small (~500 LOC) library is meant to make an improvement in this area. The developer will, finally, be able to split
the events sourcing code into smaller pieces, write unit tests for each interesting piece, and then compose the parts into
a larger application, all in a type safe and consistent way.

# Maturity

The code is proven to be stable and, currently, powering the whole set of mission critical applications. Saying that, the
library itself is provided on AS IS basis, without any promises or guarantees.

# Implementation

It is a specialized version of [IndexedReaderWriterStateT](https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/data/IndexedReaderWriterStateT.scala#L34)

In case you come here and have no clue of what the weird word above means, you can start your learning journey in the following order:
1. [cats exercises](https://www.scala-exercises.org/cats)
2. [Reader](https://eed3si9n.com/learning-scalaz/Reader.html)
3. [Writer](https://typelevel.org/cats/datatypes/writer.html)
4. [State](https://typelevel.org/cats/datatypes/state.html)
5. [Monad Transformer](https://eed3si9n.com/learning-scalaz/Monad+transformers.html#:~:text=A%20monad%20transformer%20is%20similar,behaviour%20of%20an%20underlying%20monad.)
6. [WriterT](https://typelevel.org/cats/datatypes/writert.html)
7. [StateT](https://typelevel.org/cats/datatypes/statet.html)
8. If you have gone so far by now and do understand basic principles of event sourcing, you should have no questions about `Patch` :)

# Example

Here is a short example of how this works

```scala
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
    val result = patch.run(State(0), SeqNr.Min)

    // here we have resulting state, list of all events, composition of side effects to be executed in case events are successfully persisted
    result // IO(Patch.Result(State(1), List(Event(1)), IO.unit, State(1).asRight))
```

## Setup

in [`build.sbt`](https://www.scala-sbt.org/1.x/docs/Basic-Def.html#What+is+a+build+definition%3F)
```scala
addSbtPlugin("com.evolution" % "sbt-artifactory-plugin" % "0.0.2")

libraryDependencies += "com.evolution" %% "patch" % "0.1.0"
```
