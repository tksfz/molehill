package org.tksfz.molehill

import cats.effect.Async
import cats.free.Free

package object plan {
  type PlanIO[A] = Free[Plan, A]

  implicit val asyncFreePlan: Async[PlanIO] = ???
}
