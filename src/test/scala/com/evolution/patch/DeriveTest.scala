package com.evolution.patch

import cats.syntax.all._
import com.evolution.patch.Derive.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class DeriveTest extends AnyFunSuite with Matchers {
  import DeriveTest._

  test("derive both") {
    val expected = ("a", "b")
    val actual: (String, String) = derive("a", "b")()
    actual shouldEqual expected
  }

  test("derive left") {
    val actual: String = derive("", ())()
    actual shouldEqual ""
  }

  test("derive right") {
    val actual: String = derive((), "")()
    actual shouldEqual ""
  }

  test("derive unit") {
    val expected = ()
    def actual(): Unit = derive((), ())()
    actual() shouldEqual expected
  }

  test("derive product") {
    val actual: Option[(String, String)] = derive("a".some, "b".some)()
    actual shouldEqual ("a", "b").some
  }

  test("derive product left") {
    val actual: Option[String] = derive("".some, ().some)()
    actual shouldEqual "".some
  }

  test("derive product right") {
    val actual: Option[String] = derive(().some, "".some)()
    actual shouldEqual "".some
  }
}

object DeriveTest {

  def derive[A, B](a: A, b: B): Apply[A, B] = new Apply[A, B] {
    def apply[C]()(implicit derive: Derive[A, B, C]): C = a.derive(b)
  }

  trait Apply[A, B] {
    def apply[C]()(implicit derive: Derive[A, B, C]): C
  }
}