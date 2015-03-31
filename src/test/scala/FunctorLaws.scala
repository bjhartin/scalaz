package com.banno.scalaz

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._

class FunctorLawSpec extends Specification {

  "Given the FunctorLaws example code" >> {

    import FunctorLawsExample._

    "Layin down the law" >> {
      "Identity element" >> {
        Some(1) map identity must_== Some(1)
      }

      "Associativity" >> {

        val plusOne: Int => Int  = (_:Int) + 1
        val minusTwo: Int => Int  = (_:Int) - 2

        (Some(1) map (plusOne map minusTwo)) must_== ((Some(1) map plusOne) map minusTwo)
      }

      /*
       Hmm...does this mean Functor's map is a monoid?

       - Has identity element
       - Has associativity
       - Maps from F[A] to F[B] (Is that considered closed with respect to F[_]?)
       -

       */

    }

    "Using ScalaCheck to test the laws" >> {
      val res = functor.laws[List].check
      1 must_== 1
    }

  }

  object FunctorLawsExample {}
}
