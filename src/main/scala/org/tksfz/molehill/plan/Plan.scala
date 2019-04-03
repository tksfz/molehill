package org.tksfz.molehill.plan

import cats.Id
import cats.free.Free
import org.tksfz.molehill.aws.ec2.EC2IO
import org.tksfz.molehill.data.Whence

sealed trait Plan[A]

case class CreateAnew[Spec[_[_]], Exports[_[_]]](postSpec: Spec[Whence], postExports: Exports[Whence], execute: EC2IO[Exports[Whence]]) extends Plan[Exports[Whence]]

object Plan {
  type FreePlan[A] = Free[Plan, A]
}