package org.scalage.core.algebras

/**
  * A Group is a set with an associative, invertible binary operation, and an identity
  * Created by paddy on 26/02/16.
  */
trait Group[GroupElement] extends Monoid[GroupElement]{
  def inverse(groupElement: GroupElement):GroupElement
}

object Group{

  abstract class AdditiveGroup[GroupElement] extends Group[GroupElement]{
    implicit class AdditiveGroupElement(element:GroupElement){
      def +(other:GroupElement): GroupElement ={
        op(element,other)
      }

      def -(other:GroupElement): GroupElement ={
        op(element,inverse(other))
      }

    }

    def apply(element:GroupElement)=AdditiveGroupElement(element)
  }
  class ModularIntegerAdditionGroup(modulus:Int) extends AdditiveGroup[Int]{

    override def inverse(groupElement: Int): Int = -groupElement

    override def identity: Int = 0

    override def op(a: Int, b: Int): Int = (a + b) % modulus
  }

  class IntegerAdditionGroup extends AdditiveGroup[Int]{

    override def inverse(groupElement: Int): Int = -groupElement

    override def identity: Int = 0

    override def op(a: Int, b: Int): Int = a + b
  }

  def ModularIntegerAdditionGroup(modulus:Int)=new ModularIntegerAdditionGroup(modulus)
  def IntegerAdditionGroup = new IntegerAdditionGroup
}
