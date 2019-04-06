package org.tksfz.molehill.data

import cats.Monad
import cats.effect.concurrent.Deferred
import org.tksfz.molehill.plan.PlanIO

/**
  * Holds a predicted value, which may be known completely (in the Local case), or not known entirely but assumed to
  * come fresh in the External case (and therefore assumed to have changed from its previous state).
  *
  * This predicted value is used to determine whether a value may have changed from its previous state during
  * the plan command.
  */
sealed trait Predicted[A] {
  def map[B](f: A => B): Predicted[B] = this match {
    case Local(a) => Local(f(a))
    case External(deferred) => ExternalDerived(deferred.get.map(f))
    case ExternalDerived(derived) => ExternalDerived(derived.map(f))
  }

  def flatMap[B](f: A => Predicted[B]): Predicted[B]

  def toPlanIO: PlanIO[A] = this match {
    case Local(a) => FreePlan.pure(a)
    case External(deferred) => deferred.get
    case ExternalDerived(derived) => derived
  }
}

case class Local[A](a: A) extends Predicted[A] {
  override def flatMap[B](f: A => Predicted[B]) = {
    f(a)
  }
}

/**
  * Indicates that this value comes from the Provider and is predicted to *change* from its previous state.
  * Hence, if an External value appears in a Spec, then it is assumed to be inconsistent.
  */
case class External[A](deferred: Deferred[PlanIO, A]) extends Predicted[A] {
  def flatMap[B](f: A => Predicted[B]): Predicted[B] = {
    ExternalDerived(deferred.get.flatMap {
      case Local(a) => Monad[PlanIO].pure(a)
      case External(b: Deferred[PlanIO, B]) => b.get
      case ExternalDerived(b: PlanIO[B]) => b
    })
  }
}
case class ExternalDerived[A](derived: PlanIO[A]) extends Predicted[A] {
  def flatMap[B](f: A => Predicted[B]): Predicted[B] = {
    ExternalDerived(derived.flatMap {
      case Local(a) => Monad[PlanIO].pure(a)
      case External(b: Deferred[PlanIO, B]) => b.get
      case ExternalDerived(b: PlanIO[B]) => b
    })
  }
}