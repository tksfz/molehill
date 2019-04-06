package org.tksfz.molehill.data

import shapeless.{Lens, MkFieldLens, OpticDefns, Witness}

/**
  * A pair of lenses (lens1: S1 => A, lens2: S2 => A) from two different source types S1 and S2 to a common target
  * type A.
  */
case class Bifocals[S1, S2, A](lens1: Lens[S1, A], lens2: Lens[S2, A]) {
  def >>(k: Witness)(implicit mkLens: MkFieldLens[A, k.T]): Bifocals[S1, S2, mkLens.Elem] = Bifocals(mkLens() compose this.lens1, mkLens() compose this.lens2)
}

object Bifocals {
  def apply[C1, C2, A](k: Witness)
                      (implicit mkLens1: MkFieldLens.Aux[C1, k.T, A],
                       mkLens2: MkFieldLens.Aux[C2, k.T, A]): Bifocals[C1, C2, A] =
    new Bifocals[C1, C2, A](OpticDefns.apply[C1] >> k, OpticDefns.apply[C2] >> k)
}

