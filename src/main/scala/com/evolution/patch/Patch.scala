package com.evolution.patch

import cats.data.{AndThen, EitherT}
import cats.syntax.all._
import cats.{Applicative, FlatMap, Functor, Monad, MonadError, Monoid, StackSafeMonad, ~>}

/** This monadic data structure helps to describe changes to state as events and
  * combine them together as well as relevant effects
  *
  * @tparam M
  *   A *monad* to be used to capture the effectful computation when doing a
  *   patch such as [[cats.effect.IO]]. The common use case is to use it to
  *   raise an error found if validation or other effectful process failed.
  * @tparam S
  *   A *state* to update when calling [[Patch#event]], or retain when calling
  *   other methods.
  * @tparam E
  *   A type of *events* to accumulate when calling [[Patch#event]] method) or
  *   retain when calling othouter methods.
  * @tparam F
  *   A type of *effects* to be executed after the produced events are confirmed
  *   to be persisted. When doing [[Patch#flatMap]] the effects will be
  *   accumulate into a single value using provided `Monoid[F]` instance. I.e.
  *   if `F = IO[Unit]` then `fa`, `fb` and `fc` will accumulate to `fa *> fb *>
  *   fc`. Note that `F` does not have to be anyhow related to `M[_]`.
  * @tparam A
  *   A *result* of the function. Also used as an argument in [[Patch#map]] and
  *   [[Patch#flatMap]].
  */
sealed abstract case class Patch[M[_], S, E, F, A] private (
  io: Patch.In[S, E] => M[Patch.Out[S, E, F, A]]
) {
  self =>

  import Patch._

  def map[B](f: A => B)(implicit M: Functor[M]): Patch[M, S, E, F, B] = {
    of {
      AndThen
        .apply(io)
        .andThen { out =>
          out.map { out => out.copy(a = f(out.a)) }
        }
    }
  }

  def flatMap[F1, F2, B](
    f: A => Patch[M, S, E, F1, B]
  )(implicit M: FlatMap[M], derive: Derive[F, F1, F2]): Patch[M, S, E, F2, B] = {
    of {
      AndThen
        .apply(io)
        .andThen { out =>
          out.flatMap { out =>
            f(out.a)
              .io(out.in)
              .map { out1 =>
                out1.copy(effect = derive(out.effect, out1.effect))
              }
          }
        }
    }
  }

  def as[B](value: B)(implicit M: Functor[M]): Patch[M, S, E, F, B] = map { _ => value }

  def unit(implicit M: Functor[M]): Patch[M, S, E, F, Unit] = as(())

  def monad: MonadProjection[M, S, E, F, A] = new MonadProjection(self)

  def state: StateProjection[M, S, E, F, A] = new StateProjection(self)

  def event: EventProjection[M, S, E, F, A] = new EventProjection(self)

  def effect: EffectProjection[M, S, E, F, A] = new EffectProjection(self)

  def run(state: S, seqNr: SeqNr)(implicit M: Monad[M]): M[Result[S, E, F, A]] = {
    val in = In(state, seqNr, List.empty[E])
    self
      .io(in)
      .map { out =>
        Result(out.state, out.seqNr, out.events.reverse, out.effect, out.a)
      }
  }
}

object Patch extends PatchInstances2 {

  def apply[S]: PartiallyApplied[S] = new PartiallyApplied

  private[Patch] class PartiallyApplied[S](val b: Boolean = false) extends AnyVal {

    def apply[M[_], E, F, A](
      f: (S, SeqNr) => M[(Option[(S, E)], F, A)]
    )(implicit maker: Maker[M, S, E], M: Functor[M]): Patch[M, S, E, F, A] = {
      maker.apply(f)
    }
  }

  def change[S]: ChangePartiallyApplied[S] = new ChangePartiallyApplied

  private[Patch] class ChangePartiallyApplied[S](val b: Boolean = false) extends AnyVal {

    def apply[M[_], E, F, A](
      f: (S, SeqNr) => M[(S, E, F, A)]
    )(implicit maker: Maker[M, S, E], M: Functor[M]): Patch[M, S, E, F, A] = {
      maker.change(f)
    }
  }

  def state[M[_], S, E](implicit maker: Maker[M, S, E], M: Applicative[M]): Patch[M, S, E, Unit, S] = {
    maker.state
  }

  def seqNr[M[_], S, E](implicit maker: Maker[M, S, E], M: Applicative[M]): Patch[M, S, E, Unit, SeqNr] = {
    maker.seqNr
  }

  def events[M[_], S, E](implicit maker: Maker[M, S, E], M: Applicative[M]): Patch[M, S, E, Unit, List[E]] = {
    maker.events
  }

  def pure[M[_], S, E, A](value: A)(implicit maker: Maker[M, S, E], M: Applicative[M]): Patch[M, S, E, Unit, A] = {
    maker.pure(value)
  }

  def lift[M[_], S, E, A](value: M[A])(implicit maker: Maker[M, S, E], M: Functor[M]): Patch[M, S, E, Unit, A] = {
    maker.lift(value)
  }

  def event[M[_], S, E](
    event: E
  )(implicit maker: Maker[M, S, E], M: Functor[M], change: Change[M, S, E]): Patch[M, S, E, Unit, Unit] = {
    maker.event(event)
  }

  def effect[M[_], S, E, F](effect: F)(implicit maker: Maker[M, S, E], M: Applicative[M]): Patch[M, S, E, F, Unit] = {
    maker.effect(effect)
  }

  private[patch] def of[S, E]: OfPartiallyApplied[S, E] = new OfPartiallyApplied

  private[Patch] class OfPartiallyApplied[S, E](val b: Boolean = false) extends AnyVal {

    def apply[M[_], F, A](f: In[S, E] => M[Out[S, E, F, A]]): Patch[M, S, E, F, A] = {
      new Patch[M, S, E, F, A](f) {}
    }
  }

  final case class Result[S, E, F, A](state: S, seqNr: SeqNr, events: List[E], effect: F, value: A)

  trait Change[F[_], S, E] {
    def apply(state: S, seqNr: SeqNr, event: E): F[S]
  }

  object Change {
    def apply[S, E]: PartiallyApplied[S, E] = new PartiallyApplied

    private[Change] class PartiallyApplied[S, E](val b: Boolean = false) extends AnyVal {

      def apply[F[_]](f: (S, SeqNr, E) => F[S]): Change[F, S, E] = (s, seqNr, e) => f(s, seqNr, e)
    }

    def const[E]: PartiallyAppliedConst[E] = new PartiallyAppliedConst

    private[Change] class PartiallyAppliedConst[E](val b: Boolean = false) extends AnyVal {

      def apply[F[_], S](state: F[S]): Change[F, S, E] = (_, _, _) => state
    }
  }

  implicit class PatchOps[M[_], S, E, F, A](val self: Patch[M, S, E, F, A]) extends AnyVal {

    def optional[Er](implicit M: MonadError[M, Er], monoid: Monoid[F]): Patch[M, S, E, F, Option[A]] = {
      of { in =>
        self
          .io(in)
          .map { out => out.copy(a = out.a.some) }
          .handleError { _ => in.out(Monoid[F].empty, none[A]) }
      }
    }
  }

  implicit class PatchPatchOps[M[_], S, E, F, F1, A](val self: Patch[M, S, E, F, Patch[M, S, E, F1, A]]) extends AnyVal {

    def flatten1[F2](implicit M: FlatMap[M], derive: Derive[F, F1, F2]): Patch[M, S, E, F2, A] = {
      self.flatMap(identity)
    }
  }

  private[patch] class EffectProjection[M[_], S, E, F, A](val self: Patch[M, S, E, F, A]) extends AnyVal {

    def map[F1](f: F => F1)(implicit M: Functor[M]): Patch[M, S, E, F1, A] = {
      of {
        AndThen
          .apply(self.io)
          .andThen { out =>
            out.map { out =>
              out.copy(effect = f(out.effect))
            }
          }
      }
    }

    def add[F1, F2](effect: F1)(implicit M: Monad[M], derive: Derive[F, F1, F2]): Patch[M, S, E, F2, A] = {
      of {
        AndThen
          .apply(self.io)
          .andThen { out =>
            out.map { out =>
              out.copy(effect = derive(out.effect, effect))
            }
          }
      }
    }
  }

  private[patch] class MonadProjection[M[_], S, E, F, A](val self: Patch[M, S, E, F, A]) extends AnyVal {

    def map[M1[_]](f: M ~> M1): Patch[M1, S, E, F, A] = {
      of { in =>
        f {
          self.io(in)
        }
      }
    }
  }

  private[patch] class StateProjection[M[_], S, E, F, A](val self: Patch[M, S, E, F, A]) extends AnyVal {

    def map[S1](toS: S => S1, toS1: S1 => S)(implicit M: Functor[M]): Patch[M, S1, E, F, A] = {
      of { in =>
        self
          .io
          .apply {
            in.copy(state = toS1(in.state))
          }
          .map { out =>
            out.copy(state = toS(out.state))
          }
      }
    }

    def flatMap[S1](toS: S => M[S1])(toS1: S1 => M[S])(implicit M: FlatMap[M]): Patch[M, S1, E, F, A] = {
      of { in =>
        for {
          state <- toS1(in.state)
          out   <- self.io { in.copy(state = state) }
          state <- toS(out.state)
        } yield {
          out.copy(state = state)
        }
      }
    }
  }

  private[patch] class EventProjection[M[_], S, E, F, A](val self: Patch[M, S, E, F, A]) extends AnyVal {

    def map[E1](toE1: E => E1, toE: E1 => E)(implicit M: Functor[M]): Patch[M, S, E1, F, A] = {
      of { in =>
        self
          .io
          .apply {
            in.copy(
              events = in
                .events
                .map(toE)
            )
          }
          .map { out =>
            out.copy(
              events = out
                .events
                .map(toE1)
            )
          }
      }
    }
  }

  private[patch] final case class In[S, E](state: S, seqNr: SeqNr, events: List[E]) {
    def out[F, A](effect: F, a: A): Out[S, E, F, A] = Out(state, seqNr, events, effect, a)
  }

  private[patch] final case class Out[S, E, F, A](state: S, seqNr: SeqNr, events: List[E], effect: F, a: A) {
    def in: In[S, E] = In(state, seqNr, events)
  }

  object implicits {

    implicit class EitherOpsPatch[F[_], L, R](val self: Either[L, R]) extends AnyVal {

      def patchEitherTLift[S, E](
        implicit maker: Maker[EitherT[F, L, *], S, E],
        F: Monad[F],
      ): Patch[EitherT[F, L, *], S, E, Unit, R] = {
        self
          .toEitherT[F]
          .patchLift
      }
    }

    implicit class OpsPatch[M[_], A](val self: M[A]) extends AnyVal {
      def patchLift[S, E](implicit maker: Maker[M, S, E], M: Functor[M]): Patch[M, S, E, Unit, A] = maker.lift(self)

      // TODO TECH-756 how could we get rid of this?
      def patchEitherTLift[Er, S, E](
        implicit maker: Maker[EitherT[M, Er, *], S, E],
        M: Monad[M]
      ): Patch[EitherT[M, Er, *], S, E, Unit, A] = {
        maker.lift { EitherT.right[Er] { self } }
      }
    }

    implicit class IdOpsPatch[A](val self: A) extends AnyVal {
      def patchEvent[M[_], S](
        implicit maker: Maker[M, S, A],
        change: Change[M, S, A],
        M: Functor[M]
      ): Patch[M, S, A, Unit, Unit] = maker.event(self)

      def patch[M[_], S, E](implicit maker: Maker[M, S, E], M: Applicative[M]): Patch[M, S, E, Unit, A] =
        maker.pure(self)

      def patchEffect[F[_], S, E](implicit maker: Maker[F, S, E], F: Applicative[F]): Patch[F, S, E, A, Unit] =
        maker.effect(self)
    }
  }

  trait Maker[M[_], S, E] {

    def apply[F, A](f: (S, SeqNr) => M[(Option[(S, E)], F, A)])(implicit M: Functor[M]): Patch[M, S, E, F, A]

    def change[F, A](f: (S, SeqNr) => M[(S, E, F, A)])(implicit M: Functor[M]): Patch[M, S, E, F, A]

    def state(implicit M: Applicative[M]): Patch[M, S, E, Unit, S]

    def seqNr(implicit M: Applicative[M]): Patch[M, S, E, Unit, SeqNr]

    def events(implicit M: Applicative[M]): Patch[M, S, E, Unit, List[E]]

    def pure[A](value: A)(implicit M: Applicative[M]): Patch[M, S, E, Unit, A]

    def lift[A](value: M[A])(implicit M: Functor[M]): Patch[M, S, E, Unit, A]

    def event(event: E)(implicit M: Functor[M], change: Change[M, S, E]): Patch[M, S, E, Unit, Unit]

    def effect[F](effect: F)(implicit M: Applicative[M]): Patch[M, S, E, F, Unit]
  }

  object Maker {

    def apply[M[_], S, E]: Maker[M, S, E] = {
      new Maker[M, S, E] {

        def apply[F, A](f: (S, SeqNr) => M[(Option[(S, E)], F, A)])(implicit M: Functor[M]) = {
          of { in =>
            f(in.state, in.seqNr).map {
              case (Some((s, e)), f, a) => Out(s, in.seqNr.inc, e :: in.events, f, a)
              case (None, f, a)         => in.out(f, a)
            }
          }
        }

        def change[F, A](f: (S, SeqNr) => M[(S, E, F, A)])(implicit functor: Functor[M]) = {
          of { in =>
            f(in.state, in.seqNr).map {
              case (s, e, f, a) => Out(s, in.seqNr.inc, e :: in.events, f, a)
            }
          }
        }

        def state(implicit M: Applicative[M]) = {
          of { in =>
            in
              .out((), in.state)
              .pure[M]
          }
        }

        def seqNr(implicit M: Applicative[M]) = {
          of { in =>
            in
              .out((), in.seqNr)
              .pure[M]
          }
        }

        def events(implicit M: Applicative[M]) = {
          of { in =>
            in
              .out((), in.events.reverse)
              .pure[M]
          }
        }

        def pure[A](value: A)(implicit M: Applicative[M]) = {
          of { in =>
            in
              .out((), value)
              .pure[M]
          }
        }

        def lift[A](value: M[A])(implicit M: Functor[M]) = {
          of { in =>
            value.map { a =>
              in.out((), a)
            }
          }
        }

        def event(event: E)(implicit M: Functor[M], change: Change[M, S, E]) = {
          of { in =>
            change(in.state, in.seqNr, event).map { state =>
              Out(state, in.seqNr.inc, event :: in.events, (), ())
            }
          }
        }

        def effect[F](effect: F)(implicit M: Applicative[M]) = {
          of { in =>
            in
              .out(effect, ())
              .pure[M]
          }
        }
      }
    }
  }

  private implicit class SeqNrOps(val self: SeqNr) extends AnyVal {
    def inc: SeqNr = self + 1
  }
}

sealed abstract private[patch] class PatchInstances1 {
  import Patch._

  implicit def monadPatch[M[_]: Monad, S, E, F: Monoid]: Monad[Patch[M, S, E, F, *]] = {
    implicit val derive = Derive.fromMonoid[F]
    new Monad[Patch[M, S, E, F, *]] {

      def pure[A](a: A) = {
        of[S, E] { in =>
          in
            .out(Monoid[F].empty, a)
            .pure[M]
        }
      }

      def flatMap[A, B](fa: Patch[M, S, E, F, A])(f: A => Patch[M, S, E, F, B]) = {
        fa.flatMap(f)
      }

      override def map[A, B](fa: Patch[M, S, E, F, A])(f: A => B) = {
        fa.map(f)
      }

      def tailRecM[A, B](a: A)(f: A => Patch[M, S, E, F, Either[A, B]]) = {
        of[S, E] { in =>
          (a, in, Monoid[F].empty).tailRecM {
            case (a, in, effect) =>
              f(a)
                .io(in)
                .map { out =>
                  val effect1 = derive(effect, out.effect)
                  out.a match {
                    case Left(a) =>
                      (a, out.in, effect1).asLeft
                    case Right(a) =>
                      out
                        .copy(a = a, effect = effect1)
                        .asRight
                  }
                }
          }
        }
      }
    }
  }
}

sealed abstract private[patch] class PatchInstances2 extends PatchInstances1 {
  import Patch._

  implicit def monoidPatch[M[_], S, E, F, A](implicit
    maker: Maker[M, S, E],
    M: Monad[M],
    F: Monoid[F],
    A: Monoid[A],
    derive: Derive[F, F, F],
  ): Monoid[Patch[M, S, E, F, A]] = {
    new Monoid[Patch[M, S, E, F, A]] {

      def empty = {
        Patch
          .effect(Monoid[F].empty)
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

  implicit def monadErrorPatch[M[_], Er, S, E, F: Monoid](
    implicit M: MonadError[M, Er]
  ): MonadError[Patch[M, S, E, F, *], Er] = {
    implicit val derive = Derive.fromMonoid[F]
    new MonadError[Patch[M, S, E, F, *], Er] with StackSafeMonad[Patch[M, S, E, F, *]] {

      def pure[A](a: A) = {
        of[S, E] { in =>
          in
            .out(Monoid[F].empty, a)
            .pure[M]
        }
      }

      def flatMap[A, B](fa: Patch[M, S, E, F, A])(f: A => Patch[M, S, E, F, B]) = {
        fa.flatMap(f)
      }

      override def map[A, B](fa: Patch[M, S, E, F, A])(f: A => B) = {
        fa.map(f)
      }

      def raiseError[A](e: Er) = {
        Patch.of[S, E] { _ => e.raiseError[M, Patch.Out[S, E, F, A]] }

      }

      def handleErrorWith[A](fa: Patch[M, S, E, F, A])(f: Er => Patch[M, S, E, F, A]) = {
        of[S, E] { in =>
          fa
            .io(in)
            .handleErrorWith { a => f(a).io(in) }
        }
      }

      override def handleError[A](fa: Patch[M, S, E, F, A])(f: Er => A) = {
        of[S, E] { in =>
          fa
            .io(in)
            .handleError { a =>
              in.out(Monoid[F].empty, f(a))
            }
        }
      }

      override def attempt[A](fa: Patch[M, S, E, F, A]): Patch[M, S, E, F, Either[Er, A]] = {
        of[S, E] { in =>
          fa
            .io(in)
            .map { out => out.copy(a = out.a.asRight[Er]) }
            .handleError { a => in.out(Monoid[F].empty, a.asLeft[A]) }
        }
      }
    }
  }
}
