package org.scalage.core.elements

import org.scalage.core.algebras.Group.AdditiveGroup
import org.scalage.core.algebras.{NotAnElementException, Module}

/**
  * Created by paddy on 04/03/16.
  */
case class Matrix[Components](val nRows:Int,val nCols:Int,val elements:Seq[Components]) {
  def +(other:Matrix[Components])(implicit addGroup:AdditiveGroup[Components]): Matrix[Components] ={
    if (other.nRows == nRows && other.nCols == nCols) {
      val newElements = elements.zip(other.elements).map { case (a, b) => addGroup.op(a, b) }
      Matrix(nRows,nCols,newElements)
    }
    else{
      throw new NotAnElementException
    }
  }

  def apply(row:Int,column:Int):Components=elements(nCols*(row-1) + column - 1)
}

object Matrix{

}
