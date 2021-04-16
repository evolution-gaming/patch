package com.evolution.patch

import cats.Monad
import cats.syntax.all._
import com.evolution.patch.Patch._

import scala.annotation.switch

private[patch] object RunPatch {

  def apply[M[_], S, E, F, A](
    patch: Patch[M, S, E, F, A],
    state: S,
    seqNr: SeqNr,
    replay: (S, E) => M[S])(implicit
    M: Monad[M]
  ): M[Result[S, E, F, A]] = {

    type P = Patch[M, S, E, Any, Any]
    type D = Any => Any
    type X = (Any, Any) => M[(D, P)]
    type L = (S, SeqNr, List[E], List[D], P, List[X])
    type R = Result[S, E, Any, Any]

    (state, seqNr, List.empty[E], List.empty[D], patch.asInstanceOf[P], List.empty[X])
      .tailRecM[M, R] { case (s, seqNr, es, ds, p, xs) =>

        def result(s: S, seqNr: SeqNr, es: List[E], ds: List[D], f: Any, a: Any): M[Either[L, R]] = {
          xs match {
            case Nil =>
              Result(
                state = s,
                events = es,
                effect = ds.foldLeft(f) { (f, d) => d(f) },
                value = a)
                .asRight[L]
                .pure[M]

            case x :: xs =>
              x(f, a).map { case (d, p) => (s, seqNr, es, d :: ds, p, xs).asLeft[R] }
          }
        }

        (p.tag: @switch) match {
          case 0 =>
            val a = p.asInstanceOf[Pure[Any]].value
            result(s, seqNr, es, ds, (), a)

          case 1 =>
            val p1 = p.asInstanceOf[FlatMap[M, S, E, Any, Any, Any, Any, Any]]
            val x: X = (f, a) => {
              val p2 = p1.f(a)
              val d: D = f1 => p1.derive(f, f1)
              (d, p2).pure[M]
            }
            (s, seqNr, es, ds, p1.patch, x :: xs)
              .asLeft[R]
              .pure[M]

          case 2 =>
            val p1 = p.asInstanceOf[Map[M, S, E, Any, Any, Any]]
            val x: X = (f, a) => {
              val p2 = Pure(p1.f(a)).asInstanceOf[P]
              val d: D = _ => f
              (d, p2).pure[M]
            }
            (s, seqNr, es, ds, p1.patch, x :: xs)
              .asLeft[R]
              .pure[M]

          case 3 =>
            p
              .asInstanceOf[Lift[M, Any]]
              .value
              .flatMap { a => result(s, seqNr, es, ds, (), a) }

          case 4 =>
            val e = p.asInstanceOf[Event[E]].value
            replay(s, e).flatMap { s => result(s, seqNr + 1, e :: es, ds, (), ()) }

          case 5 =>
            val f = p.asInstanceOf[Effect[Any]].value
            result(s, seqNr, es, ds, f, ())

          case 6 =>
            result(s, seqNr, es, ds, (), s)

          case 7 =>
            result(s, seqNr, es, ds, (), seqNr)

          case 8 =>
            p
              .asInstanceOf[Default[M, S, E, Any, Any]]
              .f(s, seqNr)
              .flatMap {
                case (None, f, a)         => result(s, seqNr, es, ds, f, a)
                case (Some((s, e)), f, a) => result(s, seqNr + 1, e :: es, ds, f, a)
              }

          case 9 =>
            p
              .asInstanceOf[Change[M, S, E, Any, Any]]
              .f(s, seqNr)
              .flatMap { case (s, e, f, a) => result(s, seqNr + 1, e :: es, ds, f, a) }

          case 10 =>
            val p1 = p.asInstanceOf[EffectMap[M, S, E, Any, Any, Any]]
            (s, seqNr, es, p1.f :: ds, p1.patch, xs)
              .asLeft[R]
              .pure[M]
        }
      }
      .map { result =>
        result.copy(
          events = result.events.reverse,
          value = result.value.asInstanceOf[A],
          effect = result.effect.asInstanceOf[F])
      }
  }
}
