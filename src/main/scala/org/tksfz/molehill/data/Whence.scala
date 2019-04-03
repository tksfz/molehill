package org.tksfz.molehill.data

import cats.effect.concurrent.Deferred

sealed trait Whence[A]

case class Local[A](a: A) extends Whence[A]
case class External[F[_], A](deferred: Deferred[F, A]) extends Whence[A]