package org.tksfz.molehill.plan

import java.util.concurrent.CompletableFuture

import cats.Id
import cats.data.Kleisli
import cats.effect.{Async, ExitCase}
import cats.free.Free
import monix.catnap.FutureLift
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.{Bifocals, Predicted, Quadfocals, SequencePredicted}
import shapeless.Lens
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

sealed trait Plan[A]

case class CreateAnew[Spec[_[_]], Exports[_[_]]](postSpec: Spec[Predicted], postExports: Exports[Predicted],
                                                 execute: EC2Kleisli[PlanIO, Exports[Id]]) extends Plan[Exports[Predicted]]

/**
  * @param preSpec the spec before any modifications have been performed
  * @param targetSpec the ultimate spec we're aiming for
  * @param predictedExports the predicted exports after this modification
  * @param execute applies this modification
  */
case class Modify[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], targetSpec: Spec[Predicted], predictedExports: Exports[Predicted],
                                             execute: Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]])
                                            (implicit sequencer: SequencePredicted[Spec]) extends Plan[Exports[Predicted]] {
  lazy val preSpecLocal: Spec[Predicted] = ???

  // TODO: handle deferreds. For example we can check that all new Deferred's are completed here.
  def updated(updatePredictedExports: Exports[Predicted] => Exports[Predicted])(next: Exports[Id] => Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]]) = {
    // TODO: consider adding postSpec and updating it
    Modify(preSpec, targetSpec, updatePredictedExports(predictedExports), execute.flatMap(next))
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

object Modify {
  def apply[Spec[_[_]] : SequencePredicted, Exports[_[_]]](preSpec: Spec[Id], preExports: Exports[Id],
                                                           targetSpec: Spec[Predicted]): Modify[Spec, Exports] = {
    val predictedExports: Exports[Predicted] = ???
    Modify(preSpec, targetSpec, predictedExports, Kleisli.pure[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]](preExports))
  }
}

// Common operations for all algebras.
final case class Raw[A](f: Ec2AsyncClient => A) extends Plan[A]
//final case class Embed[A](e: Embedded[A]) extends Plan[A]
final case class Delay[A](a: () => A) extends Plan[A]
final case class HandleErrorWith[A](fa: PlanIO[A], f: Throwable => PlanIO[A]) extends Plan[A]
final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends Plan[A]
final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => PlanIO[Unit]) extends Plan[A]
final case class BracketCase[A, B](acquire: PlanIO[A], use: A => PlanIO[B], release: (A, ExitCase[Throwable]) => PlanIO[Unit]) extends Plan[B]
final case object Shift extends Plan[Unit]
final case class EvalOn[A](ec: ExecutionContext, fa: PlanIO[A]) extends Plan[A]

object PlanIO {
  def fromCompletableFuture[T](cf: CompletableFuture[T]) = {
    FutureLift.javaCompletableToAsync(asyncPlanIO.pure(cf))(asyncPlanIO)
  }
  val asyncM = Free.catsFreeMonadForFree[Plan]

  implicit val asyncPlanIO: Async[PlanIO] = new Async[PlanIO] {
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit) = Free.liftF(Async1(k))
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => PlanIO[Unit]) = Free.liftF(AsyncF(k))
    override def suspend[A](thunk: => PlanIO[A]) = asyncM.flatten(Free.liftF(Delay(() => thunk)))
    override def bracketCase[A, B](acquire: PlanIO[A])(use: A => PlanIO[B])(release: (A, ExitCase[Throwable]) => PlanIO[Unit]) = Free.liftF(BracketCase(acquire, use, release))
    override def pure[A](x: A) = Free.pure(x)
    override def flatMap[A, B](fa: PlanIO[A])(f: A => PlanIO[B]) = asyncM.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => PlanIO[Either[A, B]]) = asyncM.tailRecM(a)(f)
    override def raiseError[A](e: Throwable) = Free.liftF(Delay(throw e))
    override def handleErrorWith[A](fa: PlanIO[A])(f: Throwable => PlanIO[A]) = Free.liftF(HandleErrorWith(fa, f))
  }
}

