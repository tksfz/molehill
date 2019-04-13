package org.tksfz.molehill

import cats.arrow.FunctionK
import cats.free.Free
import cats.{Applicative, Functor, Id, Monad}
import org.tksfz.molehill.algebra.{EC2Alg, EC2Exports, EC2Spec}
import org.tksfz.molehill.plan.{EC2PlanInterpreter, Modify, Plan, PlanIO}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

object Main {

  import cats.syntax.all._
  def program[F[_] : Monad, D[_]](interp: EC2Alg[F, D])(implicit D: Applicative[D]): F[Unit] = {
    for {
      inst <- interp.ec2("i1", EC2Spec(D.pure("t2.large"), D.pure("ami-12345"), D.pure(false)))
    } yield ()
  }

  import cats.instances.list._

  def main(args: Array[String]): Unit = {
    val state = Map("i1" -> (EC2Spec[Id]("t2.large", "ami-12345", true),
      EC2Exports[Id]("i-12345", "t2.large", "ami-12345", true)))
    val interp = new EC2PlanInterpreter(state)
    implicit val m: Monad[PlanIO] = Free.catsFreeMonadForFree
    program(interp).foldMap[List](new FunctionK[Plan, List] {
      override def apply[A](fa: Plan[A]) = {
        println(fa)
        fa match {
          case m@Modify(_, _, _, k) =>
            m.toEC2PlanKleisli.run(null).run(null)
        }
        Nil
      }
    })
  }
}
