package org.tksfz.molehill.plan

import cats.Id
import org.tksfz.molehill.data.{Predicted, Quadfocals}
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{LeftFolder, ZipWithKeys}
import shapeless.ops.record.{Updater, UpdaterMacros}
import shapeless.{DepFn2, HList, Lens, MkLabelledGenericLens, MkRecordSelectLens, Poly2, Witness}

/** Simultaneously creates lenses for (Spec, Exports) X (Predicted, Id) for fields that they hold in common.
  * This implementation works around https://github.com/milessabin/shapeless/issues/889 where type inference
  * seems to fail for ops.record.Selector[Spec[Id]].
  */
class QuadfocalsBuilder[Spec[_[_]], Exports[_[_]], R1 <: HList, R2 <: HList, R3 <: HList, R4 <: HList]
(implicit g1: MkLabelledGenericLens.Aux[Spec[Predicted], R1],
 g2: MkLabelledGenericLens.Aux[Exports[Predicted], R2],
 g3: MkLabelledGenericLens.Aux[Spec[Id], R3],
 g4: MkLabelledGenericLens.Aux[Exports[Id], R4]) {

  type Focus[A] = Quadfocals[Spec[Predicted], Exports[Predicted], Predicted[A], Spec[Id], Exports[Id], A]

  def field[A](k: Witness)
              (implicit l1: MkRecordSelectLens.Aux[R1, k.T, Predicted[A]],
               l2: MkRecordSelectLens.Aux[R2, k.T, Predicted[A]],
               l3: MkRecordSelectLens.Aux[R3, k.T, A],
               l4: MkRecordSelectLens.Aux[R4, k.T, A]
              ): Quadfocals[Spec[Predicted], Exports[Predicted], Predicted[A], Spec[Id], Exports[Id], A] = {
    Quadfocals(l1() compose g1(), l2() compose g2(), l3() compose g3(), l4() compose g4())
  }

  trait MkRecordSelectLens2x2[K, A] {
    def _1: Lens[R1, Predicted[A]]
    def _2: Lens[R2, Predicted[A]]
    def _3: Lens[R3, A]
    def _4: Lens[R4, A]
  }

  object MkRecordSelectLens2x2 {
    implicit def instance[K, A](implicit l1: MkRecordSelectLens.Aux[R1, K, Predicted[A]],
                                              l2: MkRecordSelectLens.Aux[R2, K, Predicted[A]],
                                              l3: MkRecordSelectLens.Aux[R3, K, A],
                                              l4: MkRecordSelectLens.Aux[R4, K, A]
                                             ) =
      new MkRecordSelectLens2x2[K, A] {
        override def _1 = l1()
        override def _2 = l2()
        override def _3 = l3()
        override def _4 = l4()
      }
  }

  def fields[A, B, C](k1: Witness, k2: Witness, k3: Witness)
                        (implicit l: MkRecordSelectLens2x2[k1.T, A],
                         l2: MkRecordSelectLens2x2[k1.T, B]): Focus[(A, B, C)] = {
    (l._1 compose g1(), l2._1 compose g1())
    new Lens[Spec[Predicted], (A, B, C)] {
      override def get(s: Spec[Predicted]) = l._1.get(s)

      override def set(s: Spec[Predicted])(a: (A, B, C)) = ???
    }
  }

  private def tupled[T, A, B](lens1: Lens[T, A], lens2: Lens[T, B]) = {
    new Lens[T, (A, B)] {
      override def get(s: T) = {
        (lens1.get(s), lens2.get(s))
      }

      override def set(s: T)(a: (A, B)) = {
        lens2.set(lens1.set(s)(a._1))(a._2)
      }
    }
  }
}

trait MkRecordSelectAllLens[R <: HList, KL <: HList] extends Serializable {
  type Elem
  def apply(): Lens[R, Elem]
}

object MkRecordSelectAllLens {
  type Aux[R <: HList, KL, Elem0] = MkRecordSelectAllLens[R, KL] { type Elem = Elem0 }

  implicit def mkRecordSelectLens[R <: HList, KL, E, U <: HList]
  (implicit selector: shapeless.ops.record.SelectAll.Aux[R, KL, E],
   zipWithKeys: ZipWithKeys.Aux[KL, E, U],
   updater: UpdateAll.Aux[R, U],
  ): Aux[R, KL, E] =
    new MkRecordSelectAllLens[R, KL] {
      type Elem = E
      def apply(): Lens[R, E] =
        new Lens[R, E] {
          def get(r: R) = selector(r)
          def set(r: R)(e: E) = updater(r, zipWithKeys(e))
        }
    }
}

trait UpdateAll[L <: HList, FL <: HList] extends DepFn2[L, FL] with Serializable { type Out <: HList }

object updateAll extends Poly2 {
  implicit def updateOne[L <: HList, F](implicit update: Updater[L, F]) = at[L, F]((l, f) => update(l, f))
}

object UpdateAll {
  type Aux[L <: HList, FL <: HList] = UpdateAll[L, FL] { type Out = L }

  implicit def instance[U <: HList, L <: HList](implicit l: LeftFolder.Aux[U, L, updateAll.type, L]) = new UpdateAll[L, U] {
    override type Out = L

    override def apply(t: L, u: U) = {
      l.apply(u, t)
    }
  }

  def apply[L <: HList, FL <: HList](implicit updater: UpdateAll[L, F]): Aux[L, F, updater.Out] = updater

  implicit def mkUpdater[L <: HList, F, O]: Aux[L, F, O] = macro UpdaterMacros.applyImpl[L, F]
}

