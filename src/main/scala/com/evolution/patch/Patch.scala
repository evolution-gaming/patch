package com.evolution.patch

import alleycats.Empty
import cats.syntax.all._
import cats.{Monad, Monoid, StackSafeMonad}

/**
  * This monadic data structure helps to describe changes to state as events
  * and combine them together as well as relevant effects
  *
  * @tparam S state
  * @tparam E event
  * @tparam F cumulative effect
  * @tparam A abstract return type
  */
sealed abstract class Patch[+M[_], +S, +E, F, +A] { self =>

  private[patch] def tag: Byte

  def map[M1[x] >: M[x], S1 >: S, E1 >: E, B](f: A => B): Patch[M1, S1, E1, F, B] = Patch.Map(self, f)

  def flatMap[M1[x] >: M[x], S1 >: S, E1 >: E, F1, F2, B](
    f: A => Patch[M1, S1, E1, F1, B])(implicit
    derive: Derive[F, F1, F2]
  ): Patch[M1, S1, E1, F2, B] = Patch.FlatMap(self, f, derive)

  def narrow[M1[x] >: M[x], S1 >: S, E1 >: E]: Patch[M1, S1, E1, F, A] = self

  def monadNarrow[M1[x] >: M[x]]: Patch[M1, S, E, F, A] = self

  def stateNarrow[S1 >: S]: Patch[M, S1, E, F, A] = self

  def eventNarrow[E1 >: E]: Patch[M, S, E1, F, A] = self

  def as[A1](a: A1): Patch[M, S, E, F, A1] = self.map { _ => a }

  def unit: Patch[M, S, E, F, Unit] = as(())


  def effectNarrow[F1](implicit derive: Derive[F, Unit, F1]): Patch[M, S, E, F1, A] = {
    self.effectMap { f => derive(f, ()) }
  }

  def effectMap[F1](f: F => F1): Patch[M, S, E, F1, A] = Patch.EffectMap(self, f)

  def effect[F1, F2](f: F1)(implicit derive: Derive[F, F1, F2]): Patch[M, S, E, F2, A] = {
    for {
      a <- self
      _ <- Patch.effect(f)
    } yield a
  }
}

object Patch extends PatchImplicits0 {

  def apply[S]: Apply[S] = new Apply[S]

  private[Patch] class Apply[S](val b: Boolean = false) extends AnyVal {

    def apply[M[_], E, F, A](f: (S, SeqNr) => M[(Option[(S, E)], F, A)]): Patch[M, S, E, F, A] = Default(f)
  }


  def change[S]: ChangeApply[S] = new ChangeApply[S]

  private[Patch] class ChangeApply[S](val b: Boolean = false) extends AnyVal {

    def apply[M[_], E, F, A](f: (S, SeqNr) => M[(S, E, F, A)]): Patch[M, S, E, F, A] = Change(f)
  }


  def event[E](value: E): Patch[Nothing, Nothing, E, Unit, Unit] = Event(value)

  def state[S]: Patch[Nothing, S, Nothing, Unit, S] = GetState

  def seqNr: Patch[Nothing, Nothing, Nothing, Unit, SeqNr] = GetSeqNr

  val unit: Patch[Nothing, Nothing, Nothing, Unit, Unit] = pure(())

  def pure[A](value: A): Patch[Nothing, Nothing, Nothing, Unit, A] = Pure(value)

  def lift[M[_], A](value: M[A]): Patch[M, Nothing, Nothing, Unit, A] = Lift(value)

  def effect[F](value: F): Patch[Nothing, Nothing, Nothing, F, Unit] = Effect(value)

  def when[M[_], S, E, F: Empty, A: Empty](
    cond: Boolean)(
    patch: => Patch[M, S, E, F, A]
  ): Patch[M, S, E, F, A] = {
    if (cond) patch else effect(Empty[F].empty).as(Empty[A].empty)
  }


  private[patch] final case class Pure[A](value: A) extends Patch[Nothing, Nothing, Nothing, Unit, A] {

    def tag = 0

    override def toString = s"Patch($value)"
  }


  private[patch] final case class FlatMap[M[_], S, E, F, F1, F2, A, A1](
    patch: Patch[M, S, E, F, A],
    f: A => Patch[M, S, E, F1, A1],
    derive: Derive[F, F1, F2]
  ) extends Patch[M, S, E, F2, A1] {

    def tag = 1
  }


  private[patch] final case class Map[M[_], S, E, F, A, A1](
    patch: Patch[M, S, E, F, A],
    f: A => A1
  ) extends Patch[M, S, E, F, A1] {

    def tag = 2
  }


  private[patch] final case class Lift[M[_], A](value: M[A]) extends Patch[M, Nothing, Nothing, Unit, A] {

    def tag = 3
  }


  private[patch] final case class Event[E](value: E) extends Patch[Nothing, Nothing, E, Unit, Unit] {

    def tag = 4
  }


  private[patch] final case class Effect[F](value: F) extends Patch[Nothing, Nothing, Nothing, F, Unit] {

    def tag = 5
  }


  private[patch] final case object GetState extends Patch[Nothing, Nothing, Nothing, Unit, Nothing] {

    def tag = 6

    override def toString = "State"
  }


  private[patch] final case object GetSeqNr extends Patch[Nothing, Nothing, Nothing, Unit, SeqNr] {

    def tag = 7

    override def toString = "SeqNr"
  }


  private[patch] final case class Default[M[_], S, E, F, A](
    f: (S, SeqNr) => M[(Option[(S, E)], F, A)]
  ) extends Patch[M, S, E, F, A] {

    def tag = 8
  }


  private[patch] final case class Change[M[_], S, E, F, A](
    f: (S, SeqNr) => M[(S, E, F, A)]
  ) extends Patch[M, S, E, F, A] {

    def tag = 9
  }


  private[patch] final case class EffectMap[M[_], S, E, F, F1, A](
    patch: Patch[M, S, E, F, A],
    f: F => F1
  ) extends Patch[M, S, E, F1, A] {

    def tag = 10
  }


  final case class Result[S, E, F, A](state: S, seqNr: SeqNr, events: List[E], effect: F, value: A)


  implicit class PatchOps[M[_], S, E, F, A](val self: Patch[M, S, E, F, A]) extends AnyVal {

    def run(
      state: S,
      seqNr: SeqNr)(
      replay: (S, E, SeqNr) => M[S])(implicit
      M: Monad[M]
    ): M[Result[S, E, F, A]] = {
      RunPatch(self, state, seqNr, replay)
    }
  }
}

sealed abstract class PatchImplicits0 {

  implicit def monoidPatch[M[_], S, E, F: Empty, A: Monoid](implicit
    derive: Derive[F, F, F]
  ): Monoid[Patch[M, S, E, F, A]] = {
    new Monoid[Patch[M, S, E, F, A]] {

      def empty = {
        Patch
          .effect(Empty[F].empty)
          .as(Monoid[A].empty)
      }

      def combine(x: Patch[M, S, E, F, A], y: Patch[M, S, E, F, A]) = {
        for {
          x <- x
          y <- y
        } yield {
          x.combine(y)
        }
      }
    }
  }


  implicit def monadPatch[M[_], S, E, F: Empty](implicit
    derive: Derive[F, F, F],
  ): Monad[Patch[M, S, E, F, *]] = {
    new StackSafeMonad[Patch[M, S, E, F, *]] {

      def pure[A](a: A) = {
        Patch
          .effect(Empty[F].empty)
          .as(a)
      }

      def flatMap[A, B](fa: Patch[M, S, E, F, A])(f: A => Patch[M, S, E, F, B]) = {
        fa.flatMap(f)
      }

      override def map[A, B](fa: Patch[M, S, E, F, A])(f: A => B) = {
        fa.map(f)
      }
    }
  }
}