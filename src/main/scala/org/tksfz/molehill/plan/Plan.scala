package org.tksfz.molehill.plan

import cats.Id
import cats.data.Kleisli
import cats.effect.Async
import cats.free.Free
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.Predicted
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

sealed trait Plan[A]

case class CreateAnew[Spec[_[_]], Exports[_[_]]](postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                                 execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]

case class Modify[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                             execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]
