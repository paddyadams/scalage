package org.scalage.core.algebras

import scala.reflect.ClassTag

/**
  * Created by paddy on 28/02/16.
  */
trait Module[ScalarElement,ModuleElement] extends Group.AdditiveGroup[ModuleElement] {

  implicit class ScalarElementClass(s:ScalarElement){
    def *(other:ModuleElement):ModuleElement = {
      scalarMultiplication(s,other)
    }
  }

  def scalarMultiplication(scalar:ScalarElement,moduleElement: ModuleElement):ModuleElement
}


object Module{

  abstract class VectorModule[ScalarElement,VectorElement:ClassTag](elementGroup:Group[VectorElement])
    extends Module[ScalarElement,Vector[VectorElement]]{

    override def inverse(groupElement: Vector[VectorElement]): Vector[VectorElement] = groupElement.map(elementGroup.inverse)

    override def identity: Vector[VectorElement] = Vector[VectorElement]()

    override def op(a: Vector[VectorElement], b: Vector[VectorElement]): Vector[VectorElement] = {
        a.zipAll(b,0,0).map{case(first:VectorElement,second:VectorElement)=>elementGroup.op(first,second)}
    }

  }

  abstract class FiniteDimensionalVectorModule[ScalarElement,VectorElement:ClassTag](dimension:Int,elementGroup:Group[VectorElement])
    extends VectorModule[ScalarElement,VectorElement](elementGroup){
    override def op(a: Vector[VectorElement], b: Vector[VectorElement]): Vector[VectorElement] = {
      if (a.size == dimension && b.size == dimension) {
        super.op(a, b)
      }
      else {
        throw new NotAnElementException()
      }
    }

    override def identity: Vector[VectorElement] = super.identity
  }

  class RingVectorModule[RingElement:ClassTag](elementRing:Ring[RingElement])
    extends VectorModule[RingElement,RingElement](elementRing){
    override def scalarMultiplication(scalar: RingElement, moduleElement: Vector[RingElement]): Vector[RingElement] =
      moduleElement.map(x=>elementRing.times(scalar,x))
  }

  class FiniteDimensionalRingVectorModule[RingElement:ClassTag](dimension:Int,elementRing:Ring[RingElement])
    extends FiniteDimensionalVectorModule[RingElement,RingElement](dimension,elementRing){
    override def scalarMultiplication(scalar: RingElement, moduleElement: Vector[RingElement]): Vector[RingElement] =
      moduleElement.map(x=>elementRing.times(scalar,x))
  }

  class IntegerModule(dimension:Int) extends FiniteDimensionalRingVectorModule[Int](dimension,Ring.integerRing)

  def IntegerModule(dimension:Int)=new IntegerModule(dimension)
}