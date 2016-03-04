package org.scalage.core.algebras

import org.scalage.core.algebras.Group.IntegerAdditionGroup

/**
  * Created by paddy on 26/02/16.
  */
trait Ring[Element] extends Group.AdditiveGroup[Element]{
  implicit class RingElementClass(element: Element){
    def *(other:Element)=times(element,other)
    def **(exponent:Int):Element={
      if (exponent==1) element
      else times(element,RingElementClass(element)**(exponent-1))
    }
    def isOne=element.equals(one)
    def isZero=element.equals(zero)
  }

  def add(a:Element,b:Element) = op(a,b)
  def negate(a:Element):Element = inverse(a)
  def times(a:Element,b:Element):Element
  def zero = identity
  def one:Element
}

object Ring{
  class IntegerRing extends IntegerAdditionGroup with Ring[Int] {
    override def times(a: Int, b: Int): Int = a*b
    override def one: Int = 1
  }

  implicit val integerRing = new IntegerRing

}
