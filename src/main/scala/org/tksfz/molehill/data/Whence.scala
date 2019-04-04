package org.tksfz.molehill.data

import cats.Monad
import cats.effect.concurrent.Deferred
import org.tksfz.molehill.plan.Plan.FreePlan

sealed trait Whence[A] {
  def map[B](f: A => B): Whence[B] = this match {
    case Local(a) => Local(f(a))
    case External(deferred) => ExternalDerived(deferred.get.map(f))
    case ExternalDerived(derived) => ExternalDerived(derived.map(f))
  }

  def flatMap[B](f: A => Whence[B]): Whence[B]

  def lift: FreePlan[A] = ???
}

case class Local[A](a: A) extends Whence[A] {
  override def flatMap[B](f: A => Whence[B]) = {
    f(a)
  }
}
case class External[F[_] : Monad, A](deferred: Deferred[F, A]) extends Whence[A] {
  def flatMap[B](f: A => Whence[B]): Whence[B] = {
    ExternalDerived(deferred.get.flatMap { _ match {
      case Local(a) => Monad[F].pure(a)
      case External(b) => b.get
      case ExternalDerived(b) => b
    }
  })
  }
}
case class ExternalDerived[F[_] : Monad, A](derived: F[A]) extends Whence[A] {
  def flatMap[B](f: A => Whence[B]): Whence[B] = {
    ExternalDerived(derived.flatMap { _ match {
      case Local(a) => Monad[F].pure(a)
      case External(b) => b.get
      case ExternalDerived(b) => b
    }
    })
  }
}