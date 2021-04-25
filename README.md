# Patch
[![Build Status](https://github.com/evolution-gaming/patch/workflows/CI/badge.svg)](https://github.com/evolution-gaming/patch/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/evolution-gaming/patch/badge.svg?branch=master)](https://coveralls.io/github/evolution-gaming/patch?branch=master)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/f9d2e05d108c4c259680b4b5f7753001)](https://www.codacy.com/gh/evolution-gaming/patch/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=evolution-gaming/patch&amp;utm_campaign=Badge_Grade)
[![Version](https://img.shields.io/badge/version-click-blue)](https://evolution.jfrog.io/artifactory/api/search/latestVersion?g=com.evolution&a=patch_2.13&repos=public)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellowgreen.svg)](https://opensource.org/licenses/MIT)

`Patch` is a monadic data structure - a building block for [event-sourcing](https://martinfowler.com/eaaDev/EventSourcing.html) application
Here is a short example of how this works

```scala
  final case class Event(value: Int)
  
  final case class State(value: Int)
  
  def enabled: IO[Boolean] = ???
  
  def log(msg: String): IO[Unit] = ???
  
  val patch: Patch[IO, State, Event, IO[Unit], Either[String, State]] = for {
    enabled <- Patch.lift { enabled } // you might need to execute effect in order to decide on how to proceed
    result  <- if (enabled) {
      for {
        before <- Patch.state[State]
        _      <- Patch.event(Event(+1)) // event to be appended
        after  <- Patch.state[State] // state after event is applied
        seqNr  <- Patch.seqNr // seqNr at this point
        _      <- Patch.effect { log(s"state changed from $before to $after($seqNr)") } // side effect to be executed 
      } yield {
        after.asRight[String]
      }
    } else {
      // you might not produce any events and just have side effect
      Patch
        .effect { log("state remains the same") }
        .as("disabled".asLeft[State])
    }
  } yield result
  
  // now we can run our `Patch` by passing initial state, seqNr and `replay` function `(state, event) => state`   
  val result = patch.run(State(0), SeqNr.Min) { (state, event, seqNr) =>
    state
      .copy(value = state.value + event.value)
      .pure[IO]
  }

  // here we have resulting state, list of all events, composition of side effects to be executed in case events are successfully persisted
  result // Patch.Result(State(1), List(Event(1)), IO.unit, State(1).asRight)
```

## Setup

in [`build.sbt`](https://www.scala-sbt.org/1.x/docs/Basic-Def.html#What+is+a+build+definition%3F)
```scala
addSbtPlugin("com.evolution" % "sbt-artifactory-plugin" % "0.0.2")

libraryDependencies += "com.evolution" %% "patch" % "0.0.5"
```