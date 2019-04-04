package org.tksfz.molehill.plan

import java.util.function.BiConsumer

import cats.Id
import cats.data.Kleisli
import cats.effect.{Async, IO}
import cats.effect.concurrent.Deferred
import cats.free.Free
import org.tksfz.molehill.algebra.{EC2Alg, EC2Exports, EC2Spec}
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.{External, ExternalDerived, Whence}
import org.tksfz.molehill.plan.Plan.FreePlan
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{RunInstancesRequest, RunInstancesResponse}

class EC2PlanInterpreter(preStore: Map[String, Any]) extends EC2Alg[FreePlan, Whence] {
  import Plan.asyncFreePlan

  override def ec2(key: String, spec: EC2Spec[Whence]): FreePlan[EC2Exports[Whence]] = {
    val preSpec: Option[EC2Spec[Id]] = preStore.get(key).map(_.asInstanceOf[EC2Spec[Id]])
    preSpec.fold {
      for {
        instanceId <- Deferred.uncancelable[FreePlan, String]
        // An example optimization would be to invoke runInstances
        exports = EC2Exports(External(instanceId), spec.instanceType, spec.amiId)
        x <- Free.liftF(CreateAnew(spec, exports, EC2Kleisli { ec2 =>
          for {
            amiId <- spec.amiId.lift
            instanceType <- spec.instanceType.lift
            request = RunInstancesRequest.builder().instanceType(instanceType).imageId(amiId).build()
            response <- Async[FreePlan].async[RunInstancesResponse] { cb =>
              ec2.runInstances(request).whenComplete(new BiConsumer[RunInstancesResponse, Throwable] {
                override def accept(t: RunInstancesResponse, u: Throwable) = {
                  cb(if (u != null) Left(u) else Right(t))
                }
              })
            }
            _ <- instanceId.complete(response.instances().get(0).instanceId)
          } yield {
            val responseHead = response.instances().get(0)
            EC2Exports[Id](responseHead.instanceId, responseHead.instanceTypeAsString, responseHead.imageId)
          }
        }))
      } yield {
        x
      }
    } { ???
    }
  }
}
