package org.tksfz.molehill.aws

import cats.data.Kleisli
import cats.effect.IO
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

object ec2 {
  type EC2Kleisli[F[_], A] = Kleisli[F, Ec2AsyncClient, A]
  type EC2IO[A] = EC2Kleisli[IO, A]

  object EC2Kleisli {
    def apply[F[_], A](f: Ec2AsyncClient => F[A]): EC2Kleisli[F, A] = Kleisli(f)
  }
}