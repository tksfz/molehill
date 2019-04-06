package org.tksfz.molehill.data

import cats.Monad
import cats.effect.concurrent.Deferred
import org.tksfz.molehill.plan.PlanIO

// PlannedSource? Predicted?
sealed trait Whence[A] {
  def map[B](f: A => B): Whence[B] = this match {
    case Local(a) => Local(f(a))
    case External(deferred) => ExternalDerived(deferred.get.map(f))
    case ExternalDerived(derived) => ExternalDerived(derived.map(f))
  }

  def flatMap[B](f: A => Whence[B]): Whence[B]

  def toPlanIO: PlanIO[A] = this match {
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

/**
  * Indicates that this value comes from the Provider and is predicted to *change* from its previous state.
  * Hence, if an External value appears in a Spec, then it is assumed to be inconsistent.
  */
case class External[A](deferred: Deferred[PlanIO, A]) extends Whence[A] {
  def flatMap[B](f: A => Whence[B]): Whence[B] = {
    ExternalDerived(deferred.get.flatMap {
      case Local(a) => Monad[PlanIO].pure(a)
      case External(b: Deferred[PlanIO, B]) => b.get
      case ExternalDerived(b: PlanIO[B]) => b
    })
  }
}
case class ExternalDerived[A](derived: PlanIO[A]) extends Whence[A] {
  def flatMap[B](f: A => Whence[B]): Whence[B] = {
    ExternalDerived(derived.flatMap {
      case Local(a) => Monad[PlanIO].pure(a)
      case External(b: Deferred[PlanIO, B]) => b.get
      case ExternalDerived(b: PlanIO[B]) => b
    })
  }
}