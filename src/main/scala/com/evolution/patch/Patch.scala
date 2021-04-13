package com.evolution.patch

import alleycats.Empty
import cats.implicits._
import cats.{Applicative, Monad, Monoid, StackSafeMonad}

/**
  * This monadic data structure helps to describe changes to state as events
  * and combine them together as well as relevant effects
  *
  * @tparam S state
  * @tparam E event
  * @tparam F cumulative effect
  * @tparam A abstract return type
  */
sealed abstract class Patch[+M[_], +S, +E, F, A]

object Patch {

  def apply[S]: Apply[S] = new Apply[S]

  private[Patch] class Apply[S](val b: Boolean = false) extends AnyVal {

    def apply[M[_], E, F, A](f: (S, SeqNr) => M[(Option[(S, E)], F, A)]): Patch[M, S, E, F, A] = Default(f)
  }


  def change[S]: ChangeApply[S] = new ChangeApply[S]

  private[Patch] class ChangeApply[S](val b: Boolean = false) extends AnyVal {

    def apply[M[_], E, F, A](f: (S, SeqNr) => M[(S, E, F, A)]): Patch[M, S, E, F, A] = Change(f)
  }


  def event[E](value: E): Patch[Nothing, Nothing, E, Unit, Unit] = Event(value)

  def state[S]: Patch[Nothing, S, Nothing, Unit, S] = GetState()

  def seqNr: Patch[Nothing, Nothing, Nothing, Unit, SeqNr] = GetSeqNr

  val unit: Patch[Nothing, Nothing, Nothing, Unit, Unit] = pure(())

  def pure[A](value: A): Patch[Nothing, Nothing, Nothing, Unit, A] = Pure(value)

  def lift[M[_], A](value: M[A]): Patch[M, Nothing, Nothing, Unit, A] = Lift(value)

  def effect[F](value: F): Patch[Nothing, Nothing, Nothing, F, Unit] = Effect(value)

  def when[M[_], S, E, F: Empty, A: Empty](
    cond: Boolean)(
    patch: => Patch[M, S, E, F, A]
  ): Patch[M, S, E, F, A] = {
    if (cond) patch else pure(Empty[A].empty).effect(Empty[F].empty)
  }


  implicit def monoidPatch[M[_]: Applicative, S, E, F: Empty, A: Empty](implicit
    derive: Derive[F, F, F]
  ): Monoid[Patch[M, S, E, F, A]] = {
    new Monoid[Patch[M, S, E, F, A]] {

      def empty = {
        Patch
          .pure(Empty[A].empty)
          .effect(Empty[F].empty)
      }

      def combine(x: Patch[M, S, E, F, A], y: Patch[M, S, E, F, A]) = {
        x.flatMap { _ => y }
      }
    }
  }


  implicit def monadPatch[F[_]: Applicative, S, E, M: Empty](implicit
    derive: Derive[M, M, M],
  ): Monad[Patch[F, S, E, M, *]] = {
    new StackSafeMonad[Patch[F, S, E, M, *]] {

      def flatMap[A, B](fa: Patch[F, S, E, M, A])(f: A => Patch[F, S, E, M, B]) = {
        PatchOps(fa).flatMap(f)
      }

      def pure[A](a: A) = {
        Patch.pure(a).effect(Empty[M].empty)
      }
    }
  }


  implicit class PatchOps[M[_], S, E, F, A](val self: Patch[M, S, E, F, A]) extends AnyVal {

    def map[A1, S1 >: S, E1 >: E](f: A => A1): Patch[M, S1, E1, F, A1] = Map(self, f)

    def flatMap[A1, S1 >: S, E1 >: E, F1, F2](
      f: A => Patch[M, S1, E1, F1, A1])(implicit
      derive: Derive[F, F1, F2]
    ): Patch[M, S1, E1, F2, A1] = FlatMap(self, f, derive)

    def as[A1](a: A1): Patch[M, S, E, F, A1] = self.map { _ => a }

    def unit: Patch[M, S, E, F, Unit] = as(())

    def mapEffect[M1](f: F => M1): Patch[M, S, E, M1, A] = MapEffect(self, f)

    def effect[M1, M2](m: M1)(implicit derive: Derive[F, M1, M2]): Patch[M, S, E, M2, A] = {
      for {
        a <- self
        _ <- Patch.effect(m)
      } yield a
    }

    def apply(
      state: S,
      seqNr: SeqNr)(
      replay: (S, E) => M[S])(implicit
      F: Monad[M]
    ): M[Result[S, E, F, A]] = {

      def loop[A1, F1](s: S, seqNr: SeqNr, p: Patch[M, S, E, F1, A1]): M[(S, List[E], F1, A1)] = {
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
            (s, List.empty[E], (), s).pure[M]

          case GetSeqNr =>
            (s, List.empty[E], (), seqNr).pure[M]

          case Pure(a) =>
            (s, List.empty[E], (), a).pure[M]

          case p: Lift[M, A1] =>
            p.ma.map { a => (s, List.empty[E], (), a) }

          case Effect(m) =>
            (s, List.empty[E], m, ()).pure[M]

          case MapEffect(p, f) =>
            loop(s, seqNr, p).map { case (s, es, m, a) => (s, es, f(m), a) }
        }
      }

      loop(state, seqNr, self).map { case (s, es, a, b) => Result(s, es.reverse, a, b) }
    }
  }

  private final case class Default[M[_], S, E, F, A](
    f: (S, SeqNr) => M[(Option[(S, E)], F, A)]
  ) extends Patch[M, S, E, F, A]


  private final case class Change[M[_], S, E, F, A](
    f: (S, SeqNr) => M[(S, E, F, A)]
  ) extends Patch[M, S, E, F, A]


  private final case class Event[E](
    e: E
  ) extends Patch[Nothing, Nothing, E, Unit, Unit]


  private final case class Map[M[_], S, E, F, A, A1](
    p: Patch[M, S, E, F, A],
    f: A => A1
  ) extends Patch[M, S, E, F, A1]


  private final case class FlatMap[M[_], S, E, F, F1, F2, A, A1](
    p: Patch[M, S, E, F, A],
    f: A => Patch[M, S, E, F1, A1],
    derive: Derive[F, F1, F2]
  ) extends Patch[M, S, E, F2, A1]


  private final case class Pure[A](
    a: A
  ) extends Patch[Nothing, Nothing, Nothing, Unit, A]


  private final case class Lift[M[_], A](
    ma: M[A]
  ) extends Patch[M, Nothing, Nothing, Unit, A]


  private final case class GetState[S]() extends Patch[Nothing, S, Nothing, Unit, S]


  private final case object GetSeqNr extends Patch[Nothing, Nothing, Nothing, Unit, SeqNr]


  private final case class Effect[F](
    f: F
  ) extends Patch[Nothing, Nothing, Nothing, F, Unit]


  private final case class MapEffect[M[_], S, E, F, A, F1](
    p: Patch[M, S, E, F, A],
    f: F => F1
  ) extends Patch[M, S, E, F1, A]


  final case class Result[S, E, F, A](state: S, events: List[E], effect: F, value: A)
}