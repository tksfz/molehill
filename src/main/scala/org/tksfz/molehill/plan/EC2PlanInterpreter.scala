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
import org.tksfz.molehill.plan.Plan.PlanIO
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{AttributeValue, ModifyInstanceAttributeRequest, RunInstancesRequest, RunInstancesResponse}

class EC2PlanInterpreter(preStore: Map[String, Any]) extends EC2Alg[PlanIO, Whence] {
  import Plan.asyncFreePlan

  override def ec2(key: String, spec: EC2Spec[Whence]): PlanIO[EC2Exports[Whence]] = {
    val preSpec: Option[(EC2Spec[Id], EC2Exports[Id])] = preStore.get(key).map(_.asInstanceOf[(EC2Spec[Id], EC2Exports[Id]))
    preSpec.fold {
      for {
        instanceId <- Deferred.uncancelable[PlanIO, String]
        // An example optimization would be to invoke runInstances
        exports = EC2Exports(External(instanceId), spec.instanceType, spec.amiId)
        x <- Free.liftF(CreateAnew(spec, exports, EC2Kleisli { ec2 =>
          for {
            amiId <- spec.amiId.toPlanIO
            instanceType <- spec.instanceType.toPlanIO
            request = RunInstancesRequest.builder().instanceType(instanceType).imageId(amiId).build()
            response <- Async[PlanIO].async[RunInstancesResponse] { cb =>
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
    } { case (preSpec, preExports) =>
      val preExportsWhence: EC2Exports[Whence] = ???
      val inconsistent = preExportsWhence.instanceType != spec.instanceType
      if (isInconsistent(_.instanceType)) {
        Modify(preSpec, spec, EC2Exports(preExportsWhence.instanceId, spec.instanceType, preExportsWhence.amiId),
          EC2Kleisli { ec2 =>
            for {
              instanceType <- spec.instanceType.toPlanIO
            } yield {
              ec2.modifyInstanceAttribute(
                ModifyInstanceAttributeRequest.builder()
                  .instanceId(preExports.instanceId)
                  .instanceType(AttributeValue.builder().value(instanceType).build())
                  .build())
              EC2Exports[Id](preExports.instanceId, instanceType, preExports.amiId)
            }
          })
      }
    }
  }

  private def isInconsistent[R[_[_]], B](f: R => B): Boolean = ???
}
