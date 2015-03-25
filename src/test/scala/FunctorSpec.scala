package com.banno.scalaz

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._

class FunctorSpec extends Specification {

  "Given the Functor example code" >> {

    import FunctorExample._

    "A functor can be mapped over" >> {

      /*

       I like the wording found in https://hseeberger.wordpress.com/2011/01/31/applicatives-are-generalized-functors.  To paraphrase, the function given to map is lifted
       into the functor's context, meaning it's executed under control of the functor.

       This makes the term 'functor' make sense to me - the functor can execute
       a function within its context, without destroying that context.

       */

      "Scala already defines this for some classes" >> {
        List(1,2,3) map {_ + 1} must_== List(2,3,4)
      }

      "Scalaz adds 'map' behavior to other classes via Functor type class" >> {

         /*

          Would not compile without scalaz.

          This example also shows that the functor is responsible for
          defining what it means to execute the function in its context.
          For tuple, it applies the function to the last value.

          */

        (1,2,3) map {_ + 1} must_== (1,2,4)
      }

      "Add a new class to the Functor typeclass" >> {
        case class Container[A](val contents:A){}

        implicit def ContainerFunctor = new Functor[Container] {
          def map[A, B](r: Container[A])( f: A => B): Container[B] = new Container(f(r.contents))
        }

        val container = new Container(1)
        container.contents must_== (1)

        container.map("x" + _) must_== Container("x1")
      }

      "Functions are functors" >> {

        /*

         How do we think about this?  A clue is in the different
         ways we can think about map.  Consider List(1,2,3) map plusOne.
         I can envision this three ways:

         0 - List(1,2,3) executes plusOne for each element and returns the
            results in a new List

         1 - map 'lifts' plusOne (Int => Int) into a new function of type
            List[Int] => List[Int] and applies it, returning a new List

         2 - map effectively composes plusOne with the List's apply method, so that when apply is called, its result is given to plusOne.  The term 'map' now takes on a new life: a map between the range of one function (apply) to the domain of another (plusOne).

         For me, the second and third make functions-as-functors easy to think about.
         */

        val list = List(1,2,3)
        val plusOne: Int => Int = (x: Int) => x + 1

        // First, some simple functor examples

        list.apply(0) must_== 1 // List has a function 'apply'
        list(0) must_== 1 // Just apply again
        (list map plusOne).apply(0) must_== 2 // Effectively apply compose plusOne

        val timesSeven: Int => Int = (x: Int) => x * 7

        (plusOne map timesSeven).apply(1) must_== 14 // Similarity to the others

        /*
           The 'context' of a function is the function's expression itself.
           Getting a value out of the context is evaluation.  What does that
           imply about functors-as-computational contexts?  That taking a
           value out need not return that value at all?  I think so.
         */

      }
    }


  }

  object FunctorExample {
  }
}
