package com.banno.scalaz

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._

class FunctorSpec extends Specification {

  "Given the Functor example code" >> {

    import FunctorExample._

    "A functor is something that can be mapped over" >> {

      "Scala already defines this for some classes via the FilterMonadic trait" >> {
        List(1,2,3) map {_ + 1} must_== List(2,3,4)
      }

      "Scalaz adds 'map' behavior to other classes via Functor type class" >> {

         // Would not be compile without scalaz.
         // STRANGE: Only applies function to last tuple member
         (1,2,3) map {_ + 1} must_== (1,2,4)
      }

      "Can do this for our own classes" >> {
        class Container[A](val contents:A){}

        implicit def ContainerFunctor = new Functor[Container] {
          def map[A, B](r: Container[A])( f: A => B): Container[B] = new Container(f(r.contents))
        }


        val container = new Container(1)
        container.contents must_== (1)

        //container map {_ + 1} must_== Container(2)
      }
    }
  }

  object FunctorExample {
  }
}
