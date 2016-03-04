package org.scalage.core.algebras

/**
  * A Monoid is a semigroup with identity
  * Created by paddy on 26/02/16.
  */
trait Monoid[@specialized(Int, Long, Float, Double) MonoidElement] extends SemiGroup[MonoidElement] {

  implicit class MonoidElementClass (monoidElement: MonoidElement){
    def isIdentity = monoidElement.equals(identity)
  }

  def identity:MonoidElement
}

