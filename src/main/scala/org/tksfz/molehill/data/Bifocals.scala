package org.tksfz.molehill.data

import shapeless.{Lens, MkFieldLens, OpticDefns, Witness}

/**
  * A pair of lenses (lens1: S1 => A, lens2: S2 => A) from two different source types S1 and S2 to a common target
  * type A.
  */
case class Bifocals[S1, S2, A](lens1: Lens[S1, A], lens2: Lens[S2, A]) {
  def >>(k: Witness)(implicit mkLens: MkFieldLens[A, k.T]): Bifocals[S1, S2, mkLens.Elem] = Bifocals(mkLens() compose this.lens1, mkLens() compose this.lens2)

  def copyInto(s1: S1, s2: S2) = {
    val a = lens1.get(s1)
    lens2.set(s2)(a)
  }
}

case class Quadfocals[S1, S2, A, T1, T2, B](lens1: Lens[S1, A], lens2: Lens[S2, A], lens3: Lens[T1, B], lens4: Lens[T2, B]) {
  def >>(k: Witness)(implicit mkLens: MkFieldLens[A, k.T],
                     mkLensT: MkFieldLens[B, k.T]): Quadfocals[S1, S2, mkLens.Elem, T1, T2, mkLensT.Elem] =
    Quadfocals(mkLens() compose this.lens1, mkLens() compose this.lens2, mkLensT() compose this.lens3, mkLensT() compose this.lens4)

  def copyFromTo(s1: S1, s2: S2) = {
    val a = lens1.get(s1)
    lens2.set(s2)(a)
  }

  def copyFromTo2(t1: T1, t2: T2) = {
    val b = lens3.get(t1)
    lens4.set(t2)(b)
  }
}

object Bifocals {
  def apply[C1, C2, A](k: Witness)
                      (implicit mkLens1: MkFieldLens.Aux[C1, k.T, A],
                       mkLens2: MkFieldLens.Aux[C2, k.T, A]): Bifocals[C1, C2, A] =
    new Bifocals[C1, C2, A](OpticDefns.apply[C1] >> k, OpticDefns.apply[C2] >> k)
}

object Quadfocals {
  def apply[C1, C2, A, D1, D2, B](k: Witness)
                                 (implicit mkLensC1: MkFieldLens.Aux[C1, k.T, A],
                                  mkLensC2: MkFieldLens.Aux[C2, k.T, A],
                                  mkLensD1: MkFieldLens.Aux[D1, k.T, B],
                                  mkLensD2: MkFieldLens.Aux[D2, k.T, B]) =
    new Quadfocals[C1, C2, A, D1, D2, B](OpticDefns.apply[C1] >> k, OpticDefns.apply[C2] >> k, OpticDefns.apply[D1] >> k, OpticDefns.apply[D2] >> k)
}