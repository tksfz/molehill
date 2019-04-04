package org.tksfz.molehill.plan

import cats.Id
import cats.data.Kleisli
import cats.effect.Async
import cats.free.Free
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.Whence
import org.tksfz.molehill.plan.Plan.PlanIO
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

sealed trait Plan[A]

case class CreateAnew[Spec[_[_]], Exports[_[_]]](postSpec: Spec[Whence], postExports: Exports[Whence],
                                                 execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Whence]]

case class Modify[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], postSpec: Spec[Whence], postExports: Exports[Whence],
                                             execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Whence]]

object Plan {
  type PlanIO[A] = Free[Plan, A]

  implicit val asyncFreePlan: Async[PlanIO] = ???
}