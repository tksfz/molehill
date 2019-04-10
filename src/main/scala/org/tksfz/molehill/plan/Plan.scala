package org.tksfz.molehill.plan

import java.util.concurrent.CompletableFuture

import cats.Id
import cats.data.Kleisli
import cats.effect.Async
import monix.catnap.FutureLift
import org.tksfz.molehill.aws.ec2.{EC2Kleisli}
import org.tksfz.molehill.data.{Predicted, Quadfocals, SequencePredicted}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

import scala.language.higherKinds

sealed trait Plan[A]

case class CreateAnew[Spec[_[_]], Exports[_[_]]](postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                                 execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]

case class ModifyBuilder[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], predictedExports: Exports[Predicted], targetSpec: Spec[Predicted],
                                                    execute: Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]])
                                                   (implicit sequencer: SequencePredicted[Spec]) {
  lazy val preSpecLocal: Spec[Predicted] = ???

  // TODO: handle deferreds
  def updated(updatePredictedExports: Exports[Predicted] => Exports[Predicted])(next: Exports[Id] => Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]]) = {
    ModifyBuilder(preSpec, updatePredictedExports(predictedExports), targetSpec, execute.flatMap(next))
  }

  def withFieldSolver[A](field: Quadfocals[Spec[Predicted], Exports[Predicted], Predicted[A], Spec[Id], Exports[Id], Id[A]],
                         f: Exports[Id] => Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), A]): ModifyBuilder[Spec, Exports] = {
    if (isInconsistent(field.lens1.get, preSpecLocal, targetSpec)) {
      updated(field.copyFromTo(targetSpec, _)) {
        exports =>
          f(exports).map { a =>
            field.lens4.set(exports)(a)
          }
      }
    } else {
      this
    }
  }

  /** Determines whether the attribute specified by f is inconsistent between the targetSpec and preSpec.
    *
    * Should this require f: Spec[D] => D[A]?
    *
    * @param f selects an attribute from the specification type Spec
    * @return true when the value of the attribute in the preSpec is inconsistent with the targetSpec
    */
  private def isInconsistent[Spec[_[_]], A, Exports[_[_]]](f: Spec[Predicted] => A, preSpec: Spec[Predicted], targetSpec: Spec[Predicted]): Boolean = {
    val a = f(preSpec)
    val b = f(targetSpec)
    a != b
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