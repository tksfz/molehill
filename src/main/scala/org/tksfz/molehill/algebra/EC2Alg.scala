package org.tksfz.molehill.algebra

case class EC2Spec[D[_]](instanceType: D[String], amiId: D[String], disableApiTermination: D[Boolean])
case class EC2Exports[D[_]](instanceId: D[String], instanceType: D[String], amiId: D[String], disableApiTermination: D[Boolean])

abstract class EC2Alg[F[_], D[_]] {
  def ec2(key: String, spec: EC2Spec[D]): F[EC2Exports[D]]
}
