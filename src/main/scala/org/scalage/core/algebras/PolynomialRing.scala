package org.scalage.core.algebras

import scala.reflect.ClassTag

/**
  *
  * Created by paddy on 26/02/16.
  */
class PolynomialRing[Coefficients:ClassTag](implicit val coeffRing:Ring[Coefficients])
  extends Ring[Polynomial[Coefficients]] {

    private implicit val implicitThis = this

    implicit class CoefficientAsPolynomial(coefficient: Coefficients) extends Polynomial[Coefficients](Vector(coefficient)){
      def *(other:Polynomial[Coefficients])=times(this,other)
      def +(other:Polynomial[Coefficients])=add(this,other)
    }

    val vectorModule = new RingVectorModule[Coefficients](coeffRing)

    def apply(coefficients: Vector[Coefficients]):Polynomial[Coefficients] = new Polynomial[Coefficients](coefficients)

    override def times(a: Polynomial[Coefficients], b: Polynomial[Coefficients]): Polynomial[Coefficients] = {
      apply(a.coefficients.zipWithIndex
        .map{case(aCoeff,index)=>(aCoeff,Vector.fill(index)(coeffRing.zero)++b.coefficients)}
        .map{case(aCoeff,shiftedb)=>vectorModule.scalarMultiplication(aCoeff,shiftedb)}
        .reduce(vectorModule.op))
    }

    override def one: Polynomial[Coefficients] = apply(Vector[Coefficients](coeffRing.one))

    override def inverse(polynomial: Polynomial[Coefficients]): Polynomial[Coefficients] = {
      apply(vectorModule.inverse(polynomial.coefficients))
    }

    override def identity: Polynomial[Coefficients] = new Polynomial[Coefficients](vectorModule.identity)

    override def op(a: Polynomial[Coefficients], b: Polynomial[Coefficients]): Polynomial[Coefficients] = {
      apply(vectorModule.op(a.coefficients,b.coefficients))
    }

    def generator = apply(Vector[Coefficients](coeffRing.zero,coeffRing.one))

  }

object PolynomialRing {
}
