package com.banno.scalaz

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._

class FoldableSpec extends Specification {

  "Given the Foldable example code" >> {

    import FoldableExample._

    "Foldable provides foldMap" >> {

      /*

       Hmm...so what of this foldMap?  It's defined as:

       def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

       What does this mean?  Well, it means that for a Foldable F[A],
       and a monoid B, we can map the As to Bs and then leverage their
       'append' (they're monoids, remember?).  So:

       */

      val nums = List("1","2","3")

      nums.foldMap(_.toInt) must_== 6

      // This is, essentially, a more concise way of doing this:

      nums.map(_.toInt).fold(0) {_ + _} must_== 6

      /*

       Notice that:

       1) fold[A](init:A)(f:(A,A)=>A):A requires an initial value
          - in foldMap, the monoid's identity value is used

       2) fold takes an (A,A)=>A
          - in foldMap, the  monoid's append is used

       3) fold doesn't guarantee the order in which the pairs will be
          processed, i.e. we could have:

          f(f(1,2),3) or f(1,f(2,3))

          i.e.

          (1 + 2) + 3 or 1 + (2 + 3)

          In other words, fold requires an associative function.

       */
    }
  }

    object FoldableExample {}
}
