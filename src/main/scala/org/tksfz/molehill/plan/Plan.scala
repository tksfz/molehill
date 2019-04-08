package org.tksfz.molehill.plan

import java.util.concurrent.CompletableFuture

import cats.Id
import cats.data.Kleisli
import cats.effect.Async
import cats.free.Free
import monix.catnap.FutureLift
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.Predicted
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

sealed trait Plan[A]

case class CreateAnew[Spec[_[_]], Exports[_[_]]](postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                                 execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]

case class Modify[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                             execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]

object PlanIO {
  def fromCompletableFuture[T](cf: CompletableFuture[T]) = {
    FutureLift.javaCompletableToAsync(Async[PlanIO].pure(cf))
  }
}