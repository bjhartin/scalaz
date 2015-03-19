package com.banno.scalaz

//import org.specs2.specification.Scope
import org.specs2.mutable.Specification
//import scalaz._
//import Scalaz._

class CanTruthySpec extends Specification {
  "Given a parameterized type CanTruthy[A] " >> {

    trait CanTruthy[A] { self =>
      def truthys(a: A): Boolean
    }

    "Can define CanTruthys for a type (Int)" >> {
      val intTruthys = new CanTruthy[Int] {
        def truthys(i: Int): Boolean = i >= 0
      }

      intTruthys.truthys(1) must_== false
      intTruthys.truthys(-1) must_== false

      "Can define a companion object" >> {
        object CanTruthy {
          def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
          def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
            def truthys(a: A): Boolean = f(a)
          }
        }

        1 must_== 1
      }

    }
  }

/*







  /**
    The companion object for CanTruthy gives us:

    - an apply for a type A which expects an implicit instance of
    CanTruthy[A] to be in scope and, finding it, returns it.
    - a factory method to create an instance of CanTruthy[A] given
    a function to use as its truthys() implementation.
    */
  /*

   */


   */
}
