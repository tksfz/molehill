package org.tksfz.molehill.plan

import java.util.function.BiConsumer

import cats.Id
import cats.data.Kleisli
import cats.effect.{Async, IO}
import cats.effect.concurrent.Deferred
import cats.free.Free
import org.tksfz.molehill.algebra.{EC2Alg, EC2Exports, EC2Spec}
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.{Bifocals, External, ExternalDerived, Predicted, Quadfocals, SequencePredicted}
import shapeless._
import shapeless.ops.record.Selector
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{AttributeBooleanValue, AttributeValue, ModifyInstanceAttributeRequest, RunInstancesRequest, RunInstancesResponse}

class EC2PlanInterpreter(preStore: Map[String, Any]) extends EC2Alg[PlanIO, Predicted] {

  override def ec2(key: String, targetSpec: EC2Spec[Predicted]): PlanIO[EC2Exports[Predicted]] = {
    val pre: Option[(EC2Spec[Id], EC2Exports[Id])] = preStore.get(key).map(_.asInstanceOf[(EC2Spec[Id], EC2Exports[Id])])
    pre.fold {
      for {
        instanceId <- Deferred.uncancelable[PlanIO, String]
        // An example optimization would be to invoke runInstances
        predictedExports = EC2Exports(External(instanceId), targetSpec.instanceType, targetSpec.amiId, targetSpec.disableApiTermination)
        x <- Free.liftF(CreateAnew(targetSpec, predictedExports, EC2Kleisli[PlanIO, EC2Exports[Id]] { ec2 =>
          for {
            spec <- Predicted.sequence(targetSpec)
            request = RunInstancesRequest.builder().instanceType(spec.instanceType).imageId(spec.amiId).build()
            response <- PlanIO.fromCompletableFuture(ec2.runInstances(request))
            // TODO: automatically push kleisli Exports results into deferred
            _ <- instanceId.complete(response.instances().get(0).instanceId)
          } yield {
            val responseHead = response.instances().get(0)
            EC2Exports[Id](responseHead.instanceId, responseHead.instanceTypeAsString, responseHead.imageId, ???)
          }
        }))
      } yield {
        x
      }
    } { case (preSpec: EC2Spec[Id], preExports: EC2Exports[Id]) =>
      Free.liftF(
        ModifyBuilder(preSpec, preExports, targetSpec)
          .withFieldSolver[String](field[EC2Spec, EC2Exports].apply.apply[Predicted[String], String]('instanceType)) { exports =>
            Kleisli[PlanIO, (Ec2AsyncClient, EC2Spec[Id]), String] { case (ec2, targetSpec) =>
              PlanIO.fromCompletableFuture(ec2.modifyInstanceAttribute(
                ModifyInstanceAttributeRequest.builder()
                  .instanceType(AttributeValue.builder().value(targetSpec.instanceType).build())
                  .build()))
                .map { _ =>
                  targetSpec.instanceType
                }
          }
        }
      )
    }
  }

  /*
  // Below doesn't work due to https://github.com/milessabin/shapeless/issues/889
  def field2[Spec[_[_]], Exports[_[_]], A, B](k: Witness)
                                         (implicit context: Context[Spec, Exports],
                                          mkLens1: MkFieldLens.Aux[Spec[Predicted], k.T, A],
                                          mkLens2: MkFieldLens.Aux[Exports[Predicted], k.T, A],
                                          mkLens3: MkFieldLens.Aux[Spec[Id], k.T, B],
                                          mkLens4: MkFieldLens.Aux[Exports[Id], k.T, B],
                                         )
  : (Bifocals[Spec[Predicted], Exports[Predicted], A], Bifocals[Spec[Id], Exports[Id], B]) = {
    (Bifocals[Spec[Predicted], Exports[Predicted], A](k), Bifocals[Spec[Id], Exports[Id], B](k))
  } */

  /** Simultaneously creates lenses for (Spec, Exports) X (Predicted, Id) for fields that they hold in common.
    * Uses an implicit Context to infer the Spec and Exports types.
    * This implementation works around https://github.com/milessabin/shapeless/issues/889 where type inference
    * seems to fail for ops.record.Selector[Spec[Id]].
    */
  class QuadfocalsBuilder[Spec[_[_]], Exports[_[_]]] {
    def apply[R1 <: HList, R2 <: HList, R3 <: HList, R4 <: HList]
    (implicit g1: MkLabelledGenericLens.Aux[Spec[Predicted], R1],
     g2: MkLabelledGenericLens.Aux[Exports[Predicted], R2],
     g3: MkLabelledGenericLens.Aux[Spec[Id], R3],
     g4: MkLabelledGenericLens.Aux[Exports[Id], R4]) = new {
      def apply[A, B](k: Witness)
                (implicit l1: MkRecordSelectLens.Aux[R1, k.T, A],
                 l2: MkRecordSelectLens.Aux[R2, k.T, A],
                 l3: MkRecordSelectLens.Aux[R3, k.T, B],
                 l4: MkRecordSelectLens.Aux[R4, k.T, B]
                ): Quadfocals[Spec[Predicted], Exports[Predicted], A, Spec[Id], Exports[Id], B] = {
        Quadfocals(l1() compose g1(), l2() compose g2(), l3() compose g3(), l4() compose g4())
      }
    }
  }

  def field[Spec[_[_]], Exports[_[_]]]
  (implicit g1: MkLabelledGenericLens[Spec[Predicted]],
   g2: MkLabelledGenericLens[Exports[Predicted]],
   g3: MkLabelledGenericLens[Spec[Id]],
   g4: MkLabelledGenericLens[Exports[Id]]) = new QuadfocalsBuilder[Spec, Exports]

}