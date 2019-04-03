package org.tksfz.molehill.aws

import cats.data.Kleisli
import cats.effect.IO
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

object ec2 {
  type EC2IO[A] = Kleisli[IO, Ec2AsyncClient, A]
}