package org.tksfz.molehill.plan

import java.util.concurrent.CompletableFuture

import cats.Id
import cats.data.Kleisli
import cats.effect.Async
import monix.catnap.FutureLift
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.{Bifocals, Predicted, Quadfocals, SequencePredicted}
import shapeless.Lens
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

import scala.language.higherKinds

sealed trait Plan[A]

case class CreateAnew[Spec[_[_]], Exports[_[_]]](postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                                 execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]

case class ModifyBuilder[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], predictedExports: Exports[Predicted], targetSpec: Spec[Predicted],
                                                    execute: Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]])
                                                   (implicit sequencer: SequencePredicted[Spec]) extends Plan[Exports[Predicted]] {
  lazy val preSpecLocal: Spec[Predicted] = ???

  // TODO: handle deferreds. For example we can check that all new Deferred's are completed here.
  def updated(updatePredictedExports: Exports[Predicted] => Exports[Predicted])(next: Exports[Id] => Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]]) = {
    ModifyBuilder(preSpec, updatePredictedExports(predictedExports), targetSpec, execute.flatMap(next))
  }

  def toEC2PlanKleisli: EC2Kleisli[PlanIO, Exports[Id]] = {
    Kleisli[PlanIO, Ec2AsyncClient, Exports[Id]] { ec2 =>
      for {
        spec <- sequencer(targetSpec)
        r <- execute.run(ec2, spec)
      } yield r
    }
  }
}

object ModifyBuilder {
  def apply[Spec[_[_]] : SequencePredicted, Exports[_[_]]](preSpec: Spec[Id], preExports: Exports[Id],
                                                           targetSpec: Spec[Predicted]): ModifyBuilder[Spec, Exports] = {
    val preExportsLocal: Exports[Predicted] = ???
    ModifyBuilder(preSpec, preExportsLocal, targetSpec, Kleisli.pure[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]](preExports))
  }
}

case class Modify[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                             execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]

object PlanIO {
  def fromCompletableFuture[T](cf: CompletableFuture[T]) = {
    FutureLift.javaCompletableToAsync(Async[PlanIO].pure(cf))
  }
}