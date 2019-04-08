package org.tksfz.molehill.plan

import java.util.function.BiConsumer

import cats.Id
import cats.effect.{Async, IO}
import cats.effect.concurrent.Deferred
import cats.free.Free
import org.tksfz.molehill.algebra.{EC2Alg, EC2Exports, EC2Spec}
import org.tksfz.molehill.aws.ec2.EC2Kleisli
import org.tksfz.molehill.data.{Bifocals, External, ExternalDerived, Predicted, SequencePredicted}
import shapeless._
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model.{AttributeValue, ModifyInstanceAttributeRequest, RunInstancesRequest, RunInstancesResponse}

class EC2PlanInterpreter(preStore: Map[String, Any]) extends EC2Alg[PlanIO, Predicted] {

  override def ec2(key: String, targetSpec: EC2Spec[Predicted]): PlanIO[EC2Exports[Predicted]] = {
    val pre: Option[(EC2Spec[Id], EC2Exports[Id])] = preStore.get(key).map(_.asInstanceOf[(EC2Spec[Id], EC2Exports[Id])])
    pre.fold {
      for {
        instanceId <- Deferred.uncancelable[PlanIO, String]
        // An example optimization would be to invoke runInstances
        exports = EC2Exports(External(instanceId), targetSpec.instanceType, targetSpec.amiId)
        x <- Free.liftF(CreateAnew(targetSpec, exports, EC2Kleisli[PlanIO, EC2Exports[Id]] { ec2 =>
          val targetSpecFinal: PlanIO[EC2Spec[Id]] = Predicted.sequence(targetSpec)
          for {
            spec <- targetSpecFinal
            request = RunInstancesRequest.builder().instanceType(spec.instanceType).imageId(spec.amiId).build()
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
    } { case (preSpec: EC2Spec[Id], preExports: EC2Exports[Id]) =>
      implicit val ctx: Context[EC2Spec, EC2Exports] = Context(preSpec, preExports, targetSpec)
      syncModify(field('instanceType)) { targetSpec => ec2 =>
        val instanceType = targetSpec.instanceType
        ec2.modifyInstanceAttribute(
          ModifyInstanceAttributeRequest.builder()
            .instanceId(preExports.instanceId)
            .instanceType(AttributeValue.builder().value(instanceType).build())
            .build())
        // temporary until we add deferFuture
        Async[PlanIO].pure(EC2Exports[Id](preExports.instanceId, instanceType, preExports.amiId))
      }
    }
  }

  case class Context[Spec[_[_]], Exports[_[_]]](preSpec: Spec[Id], preExports: Exports[Id], targetSpec: Spec[Predicted])

  /** Helper function to apply the common pattern of detecting whether a particular attribute in the
    * specification is inconsistent and, if so, runs a particular operation to make it consistent.
    *
    * The exported value for field f is then predicted to be the targetSpec value of f.
    *
    * A lens over the spec and exports is used to retrieve the value from the spec (to determine consistency)
    * and update the value in the exports.
    *
    * @param bifocals a pair of lenses over Spec and Exports that selects an attribute they hold in common
    */
  private def syncModify[Spec[_[_]], Exports[_[_]], A](bifocals: Bifocals[Spec[Predicted], Exports[Predicted], A])
                                                      (mkConsistent: Spec[Id] => Ec2AsyncClient => PlanIO[Exports[Id]])
                                                      (implicit context: Context[Spec, Exports],
                                                       sequencer: SequencePredicted[Spec]): PlanIO[Exports[Predicted]] = {
    val preSpecWhence: Spec[Predicted] = ???
    val preExportsWhence: Exports[Predicted] = ???
    if (isInconsistent(bifocals.lens1.get)) {
      val a = bifocals.lens1.get(context.targetSpec)
      val predictedExports = bifocals.lens2.set(preExportsWhence)(a)
      for {
        targetSpec <- sequencer(context.targetSpec)
        x <- Free.liftF(Modify(context.preSpec, context.targetSpec, predictedExports, EC2Kleisli(mkConsistent(targetSpec))))
      } yield {
        x
      }
    } else {
      ???
    }
  }

  /** Determines whether the attribute specified by f is inconsistent between the targetSpec and preSpec.
    *
    * Should this require f: Spec[D] => D[A]?
    *
    * @param f selects an attribute from the specification type Spec
    * @return true when the value of the attribute in the preSpec is inconsistent with the targetSpec
    */
  private def isInconsistent[Spec[_[_]], A, Exports[_[_]]](f: Spec[Predicted] => A)(implicit context: Context[Spec, Exports]): Boolean = {
    val a = f(context.preSpec.asInstanceOf[Spec[Predicted]])
    val b = f(context.targetSpec)
    a != b
  }

  /** Simultaneously defines lenses for both Spec and Exports for fields that they hold in common.
    * Uses an implicit Context to infer the Spec and Exports types.
    */
  def field[Spec[_[_]], Exports[_[_]], A](k: Witness)
                                         (implicit context: Context[Spec, Exports],
                                          mkLens1: MkFieldLens.Aux[Spec[Predicted], k.T, A],
                                          mkLens2: MkFieldLens.Aux[Exports[Predicted], k.T, A]): Bifocals[Spec[Predicted], Exports[Predicted], A] = {
    Bifocals[Spec[Predicted], Exports[Predicted], A](k)
  }

}