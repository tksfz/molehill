package org.tksfz.molehill.plan

import cats.Id
import org.tksfz.molehill.data.{Predicted, Quadfocals}
import shapeless.ops.hlist.{LeftFolder, ZipWithKeys}
import shapeless.ops.record.Updater
import shapeless.{DepFn2, HList, Lens, MkLabelledGenericLens, MkRecordSelectLens, Poly2, SingletonProductArgs, Witness}

/** Simultaneously creates lenses for (Spec, Exports) X (Predicted, Id) for fields that they hold in common.
  * This implementation works around https://github.com/milessabin/shapeless/issues/889 where type inference
  * seems to fail for ops.record.Selector[Spec[Id]].
  */
class QuadfocalsBuilder[Spec[_[_]], Exports[_[_]], R1 <: HList, R2 <: HList, R3 <: HList, R4 <: HList]
(implicit g1: MkLabelledGenericLens.Aux[Spec[Predicted], R1],
 g2: MkLabelledGenericLens.Aux[Exports[Predicted], R2],
 g3: MkLabelledGenericLens.Aux[Spec[Id], R3],
 g4: MkLabelledGenericLens.Aux[Exports[Id], R4]) extends SingletonProductArgs {

  type Focus[A] = Quadfocals[Spec[Predicted], Exports[Predicted], Predicted[A], Spec[Id], Exports[Id], A]

  def field[A](k: Witness)
              (implicit l1: MkRecordSelectLens.Aux[R1, k.T, Predicted[A]],
               l2: MkRecordSelectLens.Aux[R2, k.T, Predicted[A]],
               l3: MkRecordSelectLens.Aux[R3, k.T, A],
               l4: MkRecordSelectLens.Aux[R4, k.T, A]
              ): Quadfocals[Spec[Predicted], Exports[Predicted], Predicted[A], Spec[Id], Exports[Id], A] = {
    Quadfocals(l1() compose g1(), l2() compose g2(), l3() compose g3(), l4() compose g4())
  }

  class SelectHList[KL <: HList] {
    def lenses[AL <: HList, BL <: HList]
    (implicit l1: MkRecordSelectAllLens.Aux[R1, KL, BL],
     l2: MkRecordSelectAllLens.Aux[R2, KL, BL],
     l3: MkRecordSelectAllLens.Aux[R3, KL, AL],
     l4: MkRecordSelectAllLens.Aux[R4, KL, AL]
    ): Quadfocals[Spec[Predicted], Exports[Predicted], BL, Spec[Id], Exports[Id], AL] = {
      Quadfocals(l1() compose g1(), l2() compose g2(), l3() compose g3(), l4() compose g4())
    }

    def fieldTypes[AL <: HList] = new SelectHListFieldTypes[AL, KL]
  }

  def hlist2Product[KL <: HList](kl: KL)
   = new SelectHList[KL]

  def hlistProduct[KL <: HList, AL <: HList, BL <: HList](kl: KL)(implicit l1: MkRecordSelectAllLens.Aux[R1, KL, BL],
                                        l2: MkRecordSelectAllLens.Aux[R2, KL, BL],
                                        l3: MkRecordSelectAllLens.Aux[R3, KL, AL],
                                        l4: MkRecordSelectAllLens.Aux[R4, KL, AL]
  ): Quadfocals[Spec[Predicted], Exports[Predicted], BL, Spec[Id], Exports[Id], AL] = Quadfocals(l1() compose g1(), l2() compose g2(), l3() compose g3(), l4() compose g4())

  class SelectHListFieldTypes[AL <: HList, KL <: HList] {
    def lenses[BL <: HList]
    (implicit l1: MkRecordSelectAllLens.Aux[R1, KL, BL],
     l2: MkRecordSelectAllLens.Aux[R2, KL, BL],
     l3: MkRecordSelectAllLens.Aux[R3, KL, AL],
     l4: MkRecordSelectAllLens.Aux[R4, KL, AL]
    ): Quadfocals[Spec[Predicted], Exports[Predicted], BL, Spec[Id], Exports[Id], AL] = {
      Quadfocals(l1() compose g1(), l2() compose g2(), l3() compose g3(), l4() compose g4())
    }
  }

  class SelectFieldTypes[AL <: HList] extends SingletonProductArgs {
    def fieldsProduct[KL <: HList, BL <: HList](kl: KL)(implicit l1: MkRecordSelectAllLens.Aux[R1, KL, BL],
                                                        l2: MkRecordSelectAllLens.Aux[R2, KL, BL],
                                                        l3: MkRecordSelectAllLens.Aux[R3, KL, AL],
                                                        l4: MkRecordSelectAllLens.Aux[R4, KL, AL]
    ): Quadfocals[Spec[Predicted], Exports[Predicted], BL, Spec[Id], Exports[Id], AL] = {
      Quadfocals(l1() compose g1(), l2() compose g2(), l3() compose g3(), l4() compose g4())
    }
  }

  def fieldTypes[AL <: HList] = new SelectFieldTypes[AL]
}

trait MkRecordSelectAllLens[R <: HList, KL <: HList] extends Serializable {
  type Elem
  def apply(): Lens[R, Elem]
}

object MkRecordSelectAllLens {
  type Aux[R <: HList, KL <: HList, Elem0] = MkRecordSelectAllLens[R, KL] { type Elem = Elem0 }

  implicit def mkRecordSelectLens[R <: HList, KL <: HList, E <: HList, U <: HList]
  (implicit selector: shapeless.ops.record.SelectAll.Aux[R, KL, E],
   zipWithKeys: ZipWithKeys.Aux[KL, E, U],
   updater: UpdateAll.Aux[R, U, R],
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

trait UpdateAll[L <: HList, FL <: HList] extends DepFn2[L, FL] with Serializable

object updateOne extends Poly2 {
  implicit def updateOne[L <: HList, F](implicit update: Updater[L, F]) = at[L, F]((l, f) => update(l, f))
}

object UpdateAll {
  type Aux[L <: HList, FL <: HList, Out0] = UpdateAll[L, FL] { type Out = Out0 }

  implicit def instance[L <: HList, U <: HList](implicit l: LeftFolder[U, L, updateOne.type]): Aux[L, U, l.Out] = new UpdateAll[L, U] {
    override type Out = l.Out

    override def apply(t: L, u: U) = {
      l.apply(u, t)
    }
  }

  //def apply[L <: HList, FL <: HList](implicit updater: UpdateAll[L, F]): Aux[L, F, updater.Out] = updater

}

