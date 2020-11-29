package com.evolution.patch

import alleycats.Empty
import cats.implicits._
import cats.{Applicative, Monad, Monoid}

/**
  * This monadic data structure helps to describe changes to state as events
  * and combine them together as well as relevant effects
  *
  * @tparam S state
  * @tparam E event
  * @tparam M cumulative effect
  * @tparam A abstract return type
  */
sealed abstract class Patch[+F[_], +S, +E, M, A]

object Patch {

  def apply[S]: Apply[S] = new Apply[S]

  private[Patch] class Apply[S](val b: Boolean = false) extends AnyVal {

    def apply[F[_], E, M, A](f: (S, SeqNr) => F[(Option[(S, E)], M, A)]): Patch[F, S, E, M, A] = Default(f)
  }


  def change[S]: ChangeApply[S] = new ChangeApply[S]

  private[Patch] class ChangeApply[S](val b: Boolean = false) extends AnyVal {

    def apply[F[_], E, M, A](f: (S, SeqNr) => F[(S, E, M, A)]): Patch[F, S, E, M, A] = Change(f)
  }


  def event[E](value: E): Patch[Nothing, Nothing, E, Unit, Unit] = Event(value)

  def state[S]: Patch[Nothing, S, Nothing, Unit, S] = GetState()

  def seqNr: Patch[Nothing, Nothing, Nothing, Unit, SeqNr] = GetSeqNr

  val unit: Patch[Nothing, Nothing, Nothing, Unit, Unit] = pure(())

  def pure[A](value: A): Patch[Nothing, Nothing, Nothing, Unit, A] = Pure(value)

  def lift[F[_], A](value: F[A]): Patch[F, Nothing, Nothing, Unit, A] = Lift(value)

  def effect[M](value: M): Patch[Nothing, Nothing, Nothing, M, Unit] = Effect(value)

  def when[F[_], S, E, M: Empty, A: Empty](
    cond: Boolean)(
    patch: => Patch[F, S, E, M, A]
  ): Patch[F, S, E, M, A] = {
    if (cond) patch else pure(Empty[A].empty).effect(Empty[M].empty)
  }


  implicit def monoidPatch[F[_]: Applicative, S, E, A: Empty, M: Empty](implicit
    derive: Derive[M, M, M]
  ): Monoid[Patch[F, S, E, M, A]] = {
    new Monoid[Patch[F, S, E, M, A]] {

      def empty = {
        Patch
          .pure(Empty[A].empty)
          .effect(Empty[M].empty)
      }

      def combine(x: Patch[F, S, E, M, A], y: Patch[F, S, E, M, A]) = {
        x.flatMap { _ => y }
      }
    }
  }

  /*implicit def monadPatch[F[_]: Applicative, S, E, M: Empty](implicit
    combine: Combine[M, M, M]
  ): Monad[Patch[F, S, E, M, *]] = {
    new StackSafeMonad[Patch[F, S, E, M, *]] {

      def flatMap[A, B](fa: Patch[F, S, E, M, A])(f: A => Patch[F, S, E, M, B]) = fa.flatMap(f)

      def pure[A](a: A) = Patch.pure(a).effect(Empty[M].empty)
    }
  }*/


  implicit class PatchOps[F[_], S, E, M, A](val self: Patch[F, S, E, M, A]) extends AnyVal {

    def map[A1, S1 >: S, E1 >: E](f: A => A1): Patch[F, S1, E1, M, A1] = Map(self, f)

    def flatMap[A1, S1 >: S, E1 >: E, M1, M2](
      f: A => Patch[F, S1, E1, M1, A1])(implicit
      derive: Derive[M, M1, M2]
    ): Patch[F, S1, E1, M2, A1] = FlatMap(self, f, derive)

    def as[A1](a: A1): Patch[F, S, E, M, A1] = self.map { _ => a }

    def unit: Patch[F, S, E, M, Unit] = as(())

    def mapEffect[M1](f: M => M1): Patch[F, S, E, M1, A] = MapEffect(self, f)

    def effect[M1, M2](m: M1)(implicit derive: Derive[M, M1, M2]): Patch[F, S, E, M2, A] = {
      for {
        a <- self
        _ <- Patch.effect(m)
      } yield a
    }

    def apply(
      state: S,
      seqNr: SeqNr)(
      replay: (S, E) => F[S])(implicit
      F: Monad[F]
    ): F[Result[S, E, M, A]] = {

      def loop[A1, M1](s: S, seqNr: SeqNr, p: Patch[F, S, E, M1, A1]): F[(S, List[E], M1, A1)] = {
        p match {
          case Default(f) =>
            f(s, seqNr).map {
              case (Some((s, e)), a, b) => (s, List(e), a, b)
              case (None, a, b)         => (s, List.empty[E], a, b)
            }

          case Change(f) =>
            f(s, seqNr).map { case (s, e, a, b) => (s, List(e), a, b) }

          case p: Event[E] =>
            replay(s, p.e).map { s => (s, List(p.e), (), ()) }

          case Map(p, f) =>
            loop(s, seqNr, p).map { case (s, es, m, a) => (s, es, m, f(a)) }

          case FlatMap(p, f, derive) =>
            loop(s, seqNr, p).flatMap { case (s, es, m, a) =>
              loop(s, seqNr + es.size, f(a)).map {
                case (s, Nil, m1, a)      => (s, es, derive(m, m1), a)
                case (s, e :: Nil, m1, a) => (s, e :: es, derive(m, m1), a)
                case (s, e, m1, a)        => (s, e ::: es, derive(m, m1), a)
              }
            }

          case _: GetState[S] =>
            (s, List.empty[E], (), s).pure[F]

          case GetSeqNr =>
            (s, List.empty[E], (), seqNr).pure[F]

          case Pure(a) =>
            (s, List.empty[E], (), a).pure[F]

          case p: Lift[F, A1] =>
            p.fa.map { a => (s, List.empty[E], (), a) }

          case Effect(m) =>
            (s, List.empty[E], m, ()).pure[F]

          case MapEffect(p, f) =>
            loop(s, seqNr, p).map { case (s, es, m, a) => (s, es, f(m), a) }
        }
      }

      loop(state, seqNr, self).map { case (s, es, a, b) => Result(s, es.reverse, a, b) }
    }
  }

  private final case class Default[F[_], S, E, M, A](
    f: (S, SeqNr) => F[(Option[(S, E)], M, A)]
  ) extends Patch[F, S, E, M, A]


  private final case class Change[F[_], S, E, M, A](
    f: (S, SeqNr) => F[(S, E, M, A)]
  ) extends Patch[F, S, E, M, A]


  private final case class Event[E](
    e: E
  ) extends Patch[Nothing, Nothing, E, Unit, Unit]


  private final case class Map[F[_], S, E, M, A, A1](
    p: Patch[F, S, E, M, A],
    f: A => A1
  ) extends Patch[F, S, E, M, A1]


  private final case class FlatMap[F[_], S, E, M, M1, M2, A, A1](
    p: Patch[F, S, E, M, A],
    f: A => Patch[F, S, E, M1, A1],
    derive: Derive[M, M1, M2]
  ) extends Patch[F, S, E, M2, A1]


  private final case class Pure[A](
    a: A
  ) extends Patch[Nothing, Nothing, Nothing, Unit, A]


  private final case class Lift[F[_], A](
    fa: F[A]
  ) extends Patch[F, Nothing, Nothing, Unit, A]


  private final case class GetState[S]() extends Patch[Nothing, S, Nothing, Unit, S]


  private final case object GetSeqNr extends Patch[Nothing, Nothing, Nothing, Unit, SeqNr]


  private final case class Effect[M](
    m: M
  ) extends Patch[Nothing, Nothing, Nothing, M, Unit]


  private final case class MapEffect[F[_], S, E, M, A, M1](
    p: Patch[F, S, E, M, A],
    f: M => M1
  ) extends Patch[F, S, E, M1, A]


  final case class Result[S, E, M, A](state: S, events: List[E], effect: M, value: A)
}