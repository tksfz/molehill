package org.tksfz.molehill.plan

import cats.Id
import cats.data.Kleisli
import org.tksfz.molehill.data.{Predicted, Quadfocals}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient

trait PlanBuilder {
  implicit class ModifyExtensionMethods[Spec[_[_]], Exports[_[_]]](val modify: Modify[Spec, Exports]) {
    // Deferred's can be handled with a tap for example
    /** Helper function to apply the common pattern of detecting whether a particular field in the
      * specification is inconsistent and, if so, runs a particular operation to make it consistent.
      *
      * The exported value for field f is then predicted to be the targetSpec value of f.
      *
      * A lens over the spec and exports is used to retrieve the value from the spec (to determine consistency)
      * and update the value in the exports.
      *
      * TODO: results can come from one of 3 places:
      * (1) describe after provisioning
      * (2) predicted targetSpec
      * (3) provisioning API response
      * Also, results may need to be pushed back into a Deferred within predictedExports
      *
      * @param field a quadruple of lenses over Spec and Exports that selects an attribute they hold in common
      */
    def withFieldSolver[A](field: Quadfocals[Spec[Predicted], Exports[Predicted], Predicted[A], Spec[Id], Exports[Id], A])
                          (mkConsistent: Exports[Id] => Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), A]): Modify[Spec, Exports] = {
      if (isInconsistent(field.lens1.get, modify.preSpecLocal, modify.targetSpec)) {
        modify.updated(field.copyFromTo(modify.targetSpec, _)) {
          exports =>
            mkConsistent(exports).map { a =>
              field.lens4.set(exports)(a)
            }
        }
      } else {
        modify
      }
    }

    // Lens composition should let us handle tuples just as easily
    type Focus[A, B] = Quadfocals[Spec[Predicted], Exports[Predicted], B, Spec[Id], Exports[Id], A]

    def withField[A, B](field: Focus[A, B])
                          (mkConsistent: A => Kleisli[PlanIO, Ec2AsyncClient, A]): Modify[Spec, Exports] = {
      if (isInconsistent(field.lens1.get, modify.preSpecLocal, modify.targetSpec)) {
        modify.updated(field.copyFromTo(modify.targetSpec, _)) {
          exports =>
            Kleisli[PlanIO, (Ec2AsyncClient, Spec[Id]), Exports[Id]] { case (ec2, targetSpec) =>
              val a = field.lens3.get(targetSpec)
              mkConsistent(a).map { a =>
                field.lens4.set(exports)(a)
              }.run(ec2)
            }
        }
      } else {
        modify
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
  }
}
