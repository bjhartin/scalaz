package com.banno.scalaz

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._

class ApplicativeLawSpec extends Specification {

  "Given the ApplicativeLaws example code" >> {

    import ApplicativeLawsExample._

    "Layin down the law" >> {

      "Identity element" >> {
        val identity_pure = {(a:Int) => a}.some
        1.some <*> identity_pure must_== 1.some
        1.some <*> Some(identity[Int] _) must_== 1.some

        // Not sure how to use point with a function?
       }

      "Composition" >> {
        /* Maybe a bit clearer from Haskell:

           pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

           Hmm...what does this mean?

           My val names below mimic the ScalaZ example, since
           I was trying to understand how it mapped to the Haskell version.

         */

        val fa = 1.some
        val fab = ((_:Int).toDouble).some
        val fbc = ("" + (_:Double)).some
        val comp = ((f:Double => String) => (g: Int => Double) => f compose g).some

        // Have to accomodate for a difference in how <*> works, I think.
        fa <*> fab <*> fbc must_== "1.0".some // Sanity check

        // The law, which holds:
        fa <*> (fab <*> (fbc <*> comp)) must_== fa <*> fab <*> fbc

        /*

         What I think this means is that <*> is equivalent to composition.

         How might it not be?  Maybe the applicative would have to define <*>
         in a way that tracked some other state in the context,
         similar to how we violated the Functor law.

         */

      }


      "Homomorphism" >> {
        val a = 1
        val f = ((_:Int).toDouble)

        a.some <*> f.some must_== f(a).some

        /*

         What is the significance of this?  How might we violate it?

         */
      }

      /*

      Come back to this

      "Interchange" >> {
        val a = 1
        val f = ((_:Int).toDouble).some

        a.some <*> f must_== //?

       }*/

      /*
       Hmm...does this mean Functor's map is a monoid?

       - Has identity element
       - Has associativity
       - Maps from F[A] to F[B] (Is that considered closed with respect to F[_]?)
       -

       */

    }

    /*
    "Using ScalaCheck to test the laws" >> {
      val res = functor.laws[List].check
      1 must_== 1
    }*/

  }

  object ApplicativeLawsExample {}
}
