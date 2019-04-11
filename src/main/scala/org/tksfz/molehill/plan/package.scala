package org.tksfz.molehill

import cats.effect.Async
import cats.free.Free

package object plan extends PlanBuilder {
  type PlanIO[A] = Free[Plan, A]

  implicit val asyncPlanIO: Async[PlanIO] = ???
}
