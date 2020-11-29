package com.evolution.patch

import cats.Apply
import cats.syntax.all._

trait Derive[A, B, C] {

  def apply(a: A, b: B): C
}

object Derive extends DeriveImplicits0 {

  def apply[A, B, C](implicit derive: Derive[A, B, C]): Derive[A, B, C] = derive

  implicit def productRightDerive[F[_]: Apply, A]: Derive[F[Unit], F[A], F[A]] = (a, b) => a.productR(b)

  implicit def rightDerive[A]: Derive[Unit, A, A] = (_, a) => a


  object implicits {

    implicit class IdOpsDerive[A](val self: A) extends AnyVal {
      def derive[B, C](b: B)(implicit derive: Derive[A, B, C]): C = derive(self, b)
    }
  }
}

sealed abstract class DeriveImplicits0 extends DeriveImplicits1 {

  implicit def leftDerive[A]: Derive[A, Unit, A] = (a, _) => a

  implicit def productLeftDerive[F[_]: Apply, A]: Derive[F[A], F[Unit], F[A]] = (a, b) => a.productL(b)
}

sealed abstract class DeriveImplicits1 extends DeriveImplicits2 {

  implicit def productDerive[F[_]: Apply, A, B]: Derive[F[A], F[B], F[(A, B)]] = (a, b) => a.product(b)
}

sealed abstract class DeriveImplicits2 {

  implicit def bothDerive[A, B]: Derive[A, B, (A, B)] = (a, b) => (a, b)
}