package org.scalage.core.elements

import org.scalage.core.algebras.PolynomialRing

/**
  * Created by paddy on 26/02/16.
  */
class Polynomial[Coefficients](val coefficients:Vector[Coefficients])(implicit val polynomialRing: PolynomialRing[Coefficients]) {
  override def toString: String = {
    val indeterminates = List.fill(coefficients.size)("x").zipWithIndex.
      map{case(x,power)=>if (power>1) x+"^"+power else if (power==1) x else ""}
    coefficients.zip(indeterminates).filter( !_._1.equals(polynomialRing.coeffRing.zero)).
      map{case(coeff,x)=>if (coeff.equals(polynomialRing.coeffRing.one) && x.nonEmpty) x else coeff.toString + x}
      .reverse.mkString(" + ")
  }
}

