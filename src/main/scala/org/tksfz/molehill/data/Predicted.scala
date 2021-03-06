package org.tksfz.molehill.data

import cats.{Applicative, Id}
import cats.effect.Async
import cats.effect.concurrent.Deferred
import cats.sequence.RecordSequencer
import org.tksfz.molehill.data.Predicted.toPlanIO
import org.tksfz.molehill.plan.PlanIO
import shapeless.ops.hlist.{Align, Mapper}
import shapeless.{Generic, HList, LabelledGeneric, Poly1, ops, record}

/**
  * Holds a predicted value, which may be known completely (in the Local case), or not known entirely but assumed to
  * come fresh in the External case (and therefore assumed to have changed from its previous state).
  *
  * This predicted value is used to determine whether a value may have changed from its previous state during
  * the plan command.
  */
sealed trait Predicted[A] {
  def map[B](f: A => B): Predicted[B] = this match {
    case Local(a) => Local(f(a))
    case External(deferred) => ExternalDerived(deferred.get.map(f))
    case ExternalDerived(derived) => ExternalDerived(derived.map(f))
  }

  def flatMap[B](f: A => Predicted[B]): Predicted[B]

  def toPlanIO: PlanIO[A] = this match {
    case Local(a) => Async[PlanIO].pure(a)
    case External(deferred) => deferred.get
    case ExternalDerived(derived) => derived
  }
}

case class Local[A](a: A) extends Predicted[A] {
  override def flatMap[B](f: A => Predicted[B]) = {
    f(a)
  }
}

/**
  * Indicates that this value comes from the Provider and is predicted to *change* from its previous state.
  * Hence, if an External value appears in a Spec, then it is assumed to be inconsistent.
  */
case class External[A](deferred: Deferred[PlanIO, A]) extends Predicted[A] {
  def flatMap[B](f: A => Predicted[B]): Predicted[B] = {
    ExternalDerived(deferred.get.flatMap(f(_).toPlanIO))
  }
}
case class ExternalDerived[A](derived: PlanIO[A]) extends Predicted[A] {
  def flatMap[B](f: A => Predicted[B]): Predicted[B] = {
    ExternalDerived(derived.flatMap(f(_).toPlanIO))
  }
}

/** Helper type class that bundles all the constraints needed for sequencing
  */
trait SequencePredicted[T[_[_]]] {
  def apply(t: T[Predicted]): PlanIO[T[Id]]
}

object SequencePredicted {
  implicit def sequence[T[_[_]], L <: HList, M <: HList, N <: HList, R <: HList]
  (implicit gen: LabelledGeneric.Aux[T[Predicted], L],
            map: ops.record.MapValues.Aux[toPlanIO.type, L, M],
            sequence: RecordSequencer.Aux[M, PlanIO[N]],
            rgen: LabelledGeneric.Aux[T[Id], R],
            align: Align[N, R],
  ): SequencePredicted[T] = new SequencePredicted[T] {
    def apply(t: T[Predicted]): PlanIO[T[Id]] = {
      Predicted.sequence(t)
    }
  }

}

object Predicted {
  implicit val applicative: Applicative[Predicted] = new Applicative[Predicted] {
    override def pure[A](x: A) = Local(x)

    override def ap[A, B](ff: Predicted[A => B])(fa: Predicted[A]): Predicted[B] = {
      (ff, fa) match {
        case (Local(f), Local(a)) => Local(f(a))
        case (ff, fa) => ExternalDerived {
          for {
            f <- ff.toPlanIO
            a <- fa.toPlanIO
          } yield f(a)
        }
      }
    }
  }

  trait LowPriorityImplicits extends Poly1 {
    implicit def default[T] = at[T] { a => Async[PlanIO].pure(a) }
  }

  object toPlanIO extends Poly1 with LowPriorityImplicits {
    implicit def predicted[T] = at[Predicted[T]] { p => p.toPlanIO }
  }

  /** Suppose we have a data type e.g. EC2Exports[D[_]](instanceType: D[String], amiId: D[String]).
    * Let D = Predicted so in effect we have EC2Exports[Predicted](instanceType: Predicted[String], amiId: Predicted[String]).
    * The `sequence` function is used to convert that EC2Exports[Predicted] into a `PlanIO[EC2Exports[Id]]`.
    * In effect, it waits on the result of each Predicted member field, some of which may come from an external source.
    * This is equivalent to the following code:
    *
    * for {
    *   a <- exports.instanceType.toPlanIO
    *   b <- exports.amiId.toPlanIO
    * } yield {
    *   EC2Exports[Id](a, b)
    * }
    *
    * Consider that Predicted[A] has three cases: case class Local[A](t: A)` which yields a A value
    * immediately, `External[A](t: Deferred[PlanIO, A])`, and  `case class ExternalDerived[A](t: PlanIO[A])` which
    * yields a A value within a PlanIO context. We first convert each Predicted[A] member field of EC2Exports into
    * a PlanIO[A]. This gives us a tuple of PlanIO's, or in the code below an HList of PlanIO's. Finally, `RecordSequencer`
    * converts an HList of PlanIO's into a PlanIO of HList. The resulting HList is then Aligned back into T[Id].
    */
  def sequence[T[_[_]], L <: HList, M <: HList, N <: HList, R <: HList]
  (t: T[Predicted])(implicit gen: LabelledGeneric.Aux[T[Predicted], L],
                    map: ops.record.MapValues.Aux[toPlanIO.type, L, M],
                    sequence: RecordSequencer.Aux[M, PlanIO[N]],
                    rgen: LabelledGeneric.Aux[T[Id], R],
                    align: Align[N, R],
  ): PlanIO[T[Id]] = {
    import record._
    val l = gen.to(t)
    val planIO: PlanIO[N] = sequence(l.mapValues(toPlanIO))
    planIO.map(align).map(rgen.from)
  }
}

/** Converts a Spec or Exports from Exports[Id] => Exports[Predicted] by wrapping each
  * member in a [[Local]].
  *
  * Since there could be custom implementation, we define our own type class rather than
  * re-using an existing one.
  *
  * We also don't make this higher-kinded e.g. trait ToPredicted[Spec[_[_]] again to support
  * custom implementations in the future for non-higher-kinded types.
  */
trait ToPredicted[A] {
  type Out
  def apply(t: A): Out
}

object ToPredicted {
  object toPredicted extends Poly1 {
    implicit def main[T]: Case.Aux[T, Predicted[T]] = at[T].apply { t => Local(t) }
  }

  type Aux[A, B] = ToPredicted[A] { type Out = B }

  implicit def product[T[_[_]], L <: HList, M <: HList]
  (implicit gen: Generic.Aux[T[Id], L],
   mapper: Mapper.Aux[toPredicted.type, L, M],
   mgen: Generic.Aux[T[Predicted], M],
  ): Aux[T[Id], T[Predicted]] = new ToPredicted[T[Id]] {
    override type Out = T[Predicted]

    override def apply(t: T[Id]): T[Predicted] = {
      mgen.from(mapper(gen.to(t)))
    }
  }
}