package com.evolution.patch

import cats.Apply
import cats.kernel.Monoid
import cats.syntax.all._

trait Derive[A, B, C] {

  def apply(a: A, b: B): C
}

object Derive extends DeriveImplicits0 {

  def apply[A, B, C](implicit derive: Derive[A, B, C]): Derive[A, B, C] = derive

  def fromMonoid[A: Monoid]: Derive[A, A, A] = new Derive[A, A, A] {
    def apply(a: A, b: A) = Monoid[A].combine(a, b)
  }

  implicit def productRightDerive[F[_]: Apply, A]: Derive[F[Unit], F[A], F[A]] = new Derive[F[Unit], F[A], F[A]] {

    def apply(a: F[Unit], b: F[A]) = a.productR(b)

    override def toString = "Derive.productRight"
  }


  implicit def rightDerive[A]: Derive[Unit, A, A] = new Derive[Unit, A, A] {

    def apply(a: Unit, b: A) = b

    override def toString = "Derive.right"
  }


  object implicits {

    implicit class IdOpsDerive[A](val self: A) extends AnyVal {
      def derive[B, C](b: B)(implicit derive: Derive[A, B, C]): C = derive(self, b)
    }
  }
}

sealed abstract class DeriveImplicits0 extends DeriveImplicits1 {

  implicit def leftDerive[A]: Derive[A, Unit, A] = new Derive[A, Unit, A] {

    def apply(a: A, b: Unit) = a

    override def toString = "Derive.left"
  }


  implicit def productLeftDerive[F[_]: Apply, A]: Derive[F[A], F[Unit], F[A]] = new Derive[F[A], F[Unit], F[A]] {

    def apply(a: F[A], b: F[Unit]) = a.productL(b)

    override def toString = "Derive.productLeft"
  }
}

sealed abstract class DeriveImplicits1 extends DeriveImplicits2 {

  implicit def productDerive[F[_]: Apply, A, B]: Derive[F[A], F[B], F[(A, B)]] = new Derive[F[A], F[B], F[(A, B)]] {

    def apply(a: F[A], b: F[B]) = a.product(b)

    override def toString = "Derive.product"
  }
}

sealed abstract class DeriveImplicits2 {

  implicit def bothDerive[A, B]: Derive[A, B, (A, B)] = new Derive[A, B, (A, B)] {

    def apply(a: A, b: B) = (a, b)

    override def toString = "Derive.both"
  }
}