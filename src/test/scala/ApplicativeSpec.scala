package com.banno.scalaz

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._

class ApplicativeSpec extends Specification {

  "Given the Applicative example code" >> {

    import ApplicativeExample._

    "Functor's map can only handle single-argument functions" >> {

      /*
         This makes sense, because 'map' is essentially mapping
         the range of one function into the domain of another.

         But what if we want to map the ranges of two functions
         into the two domain(s) of a binary function?  For example,
       */

      val o1: Option[Int] = Some(1)
      val o2: Option[Int] = Some(2)
      val addEm: (Int,Int) => Int = (a:Int, b:Int) => a + b

      /*
         To do this, we need to first curry addEm so it can be
         partially applied, then lift this function and apply
         it to one option and then the other.
       */

      val addEm2 = addEm.curried // (a:Int)(b:Int) => a + b
      val addEm3 = o1 map addEm2 // Some({_ + 1})

      /*

        Now we gots problems.  We want to do this,
        to apply the function to the value inside o2:

        o2 map addEm3

        This doesn't compile, because map wants (Int => Int),
        and we now have Some(Int => Int)

        We need some new construct/operators, which might let us do something
        like:

        o1 $ o2 $ addEm

        or

        o1 $ addEm $ o2

       */


      "Applicative Functors to the rescue" >> {
         /*

          An applicative functor supports a new kind of
          apply method, <*>, which is like a functor's map
          only taking a Functor with a function in it,
          'unwrapping' that function and using it.

          In other words, we can take values in a context,
          and a computation in a context, and combine them
          to get a result in the same kind of context.

          It may not be obvious why this is useful, and that's
          OK.  We'll get to that soon.  Let's first use familiar
          syntax to compare <*> to map:

          */
        val plusOne = (a:Int) => a + 1

        1.some map plusOne must_== 2.some
        1.some <*> plusOne.some must_== 2.some
        1.some <*> Some(plusOne) must_== 2.some

        // Now we can do what we set out to do!
        o1 <*> (o2 <*> addEm.curried.some)  must_== 3.some

        // See how a computation error is handled nicely
        None <*> (o2 <*> addEm.curried.some) must_== None

        // Some syntactic sugar
        (o1 |@| o2) {addEm} must_== 3.some
        (3.some |@| 5.some |@| 6.some) {_ + _ + _} must_== 14.some
      }

      "Strange operators" >> {
        /*

         At first I was confused by <* and *>.  The description
         from 'learing scalaz' just says they return the lhs and rhs.
         They do, but it's a bit more.

         */

        1.some *> 2.some must_== 2.some
        None *> 2.some must_== None // Hmmm...sort of like 0 * x = 0
        List() *> List(1,2,3) must_== List() // Okay, seems consistent
        List(1) *> List(1,2,3) must_== List(1,2,3) // Okay
        List(1,2) *> List(1,2,3) must_== List(1,2,3,1,2,3) // Waitaminnit...
        List(2,4) *> List(1,2,3) must_== List(1,2,3,1,2,3) /// okay...

        /*

         Seems sort of like *> and *< are like multiplication / repetition
         within the context.  The contexts have to be of the same type, i.e.

         1.some *> List(1) -- doesn't compile

         */
      }
    }


    "Heavy lifting" >> {
      val addEm: (Int, Int) => Int = _ + _
      val lifted: (Option[Int], Option[Int]) => Option[Int] =
        Apply[Option].lift2(addEm)

      lifted(1.some, 2.some) must_== 3.some
    }

    "Sequence example" >> {
      /*

       Going to try to implement the Haskell function defined as:

       sequenceA :: (Applicative f) => [f a] -> f [a]
       sequenceA [] = pure []
       sequenceA (x:xs) = (:) <$> x <*> sequence

       */

      def sequenceA[F[_]: Applicative, A] (l: List[F[A]]): F[List[A]] = l match {
        case Nil => (Nil: List[A]).point[F]
          case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
      }
      sequenceA(List(1.some, 2.some)) must_== List(1,2).some
    }
  }
  object ApplicativeExample {
  }
}
