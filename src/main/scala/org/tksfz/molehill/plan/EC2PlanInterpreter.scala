package org.tksfz.molehill.plan

import cats.Id
import cats.data.Kleisli
import cats.effect.concurrent.Deferred
import cats.free.Free
import org.tksfz.molehill.algebra.{EC2Alg, EC2Exports, EC2Spec}
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.{Bifocals, External, ExternalDerived, Predicted, SequencePredicted}
import shapeless._
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
      import shapeless.syntax.singleton._
      implicit val ctx = Context(preSpec, preExports)
      Free.liftF(
        Modify(preSpec, preExports, targetSpec)
          .withFieldTypes[String :: Boolean :: HNil](select.hlist('instanceType, 'disableApiTermination)) {
            case instanceType :: disableApiTermination :: HNil =>
              println(instanceType + disableApiTermination)
              Kleisli[PlanIO, Ec2AsyncClient, String :: Boolean :: HNil] { ec2 =>
                PlanIO.fromCompletableFuture(ec2.modifyInstanceAttribute(
                  ModifyInstanceAttributeRequest.builder()
                    .instanceId(preExports.instanceId)
                    .instanceType(AttributeValue.builder().value(instanceType).build())
                    .disableApiTermination(AttributeBooleanValue.builder().value(disableApiTermination).build())
                    .build()))
                  .map { _ =>
                    instanceType :: disableApiTermination :: HNil
                  }
              }
          }
      )
    }
  }

  case class Context[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], preExports: Exports[Id])

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

  /** Uses an implicit Context to infer the Spec and Exports types.
    */
  private def select[Spec[_[_]], Exports[_[_]], R1 <: HList, R2 <: HList, R3 <: HList, R4 <: HList]
  (implicit ctx: Context[Spec, Exports],
   g1: MkLabelledGenericLens.Aux[Spec[Predicted], R1],
   g2: MkLabelledGenericLens.Aux[Exports[Predicted], R2],
   g3: MkLabelledGenericLens.Aux[Spec[Id], R3],
   g4: MkLabelledGenericLens.Aux[Exports[Id], R4]) = new QuadfocalsBuilder[Spec, Exports, R1, R2, R3, R4]

}