package org.tksfz.molehill.data

import cats.Monad
import cats.effect.concurrent.Deferred
import org.tksfz.molehill.plan.Plan.FreePlan

// PlannedSource?
sealed trait Whence[A] {
  def map[B](f: A => B): Whence[B] = this match {
    case Local(a) => Local(f(a))
    case External(deferred) => ExternalDerived(deferred.get.map(f))
    case ExternalDerived(derived) => ExternalDerived(derived.map(f))
  }

  def flatMap[B](f: A => Whence[B]): Whence[B]

  def toFreePlan: FreePlan[A] = this match {
    case Local(a) => FreePlan.pure(a)
    case External(deferred) => deferred.get
    case ExternalDerived(derived) => derived
  }
}

case class Local[A](a: A) extends Whence[A] {
  override def flatMap[B](f: A => Whence[B]) = {
    f(a)
  }
}
case class External[A](deferred: Deferred[FreePlan, A]) extends Whence[A] {
  def flatMap[B](f: A => Whence[B]): Whence[B] = {
    ExternalDerived(deferred.get.flatMap {
      case Local(a) => Monad[FreePlan].pure(a)
      case External(b: Deferred[FreePlan, B]) => b.get
      case ExternalDerived(b: FreePlan[B]) => b
    })
  }
}
case class ExternalDerived[A](derived: FreePlan[A]) extends Whence[A] {
  def flatMap[B](f: A => Whence[B]): Whence[B] = {
    ExternalDerived(derived.flatMap {
      case Local(a) => Monad[FreePlan].pure(a)
      case External(b: Deferred[FreePlan, B]) => b.get
      case ExternalDerived(b: FreePlan[B]) => b
    })
  }
}