package org.scalage.core.algebras

/**
  * A semi group is a set equipped with an associative operation
  * Created by paddy on 26/02/16.
  */
trait SemiGroup[@specialized(Int, Long, Float, Double) SemiGroupElementType] {
  def op(a:SemiGroupElementType,b: SemiGroupElementType):SemiGroupElementType
}

