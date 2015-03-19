package com.banno.scalaz

import org.specs2.mutable.Specification

class CanTruthySpec extends Specification {

  "Given the CanTruthy example code" >> {

    import CanTruthyExample._

    "Can define an instance of CanTruthy" >> {

      val intTruthys = new CanTruthy[Int] {
        def truthys(i: Int): Boolean = i >= 0
      }

      intTruthys.truthys(1) must_== true
    }

    "CanTruthy[A]() returns any implicitly available instance for A" >> {

      implicit val intTruthys = new CanTruthy[Int] {
        def truthys(i: Int): Boolean = i >= 0
      }

      CanTruthy[Int] must_== intTruthys
    }

    "CanTruthys.truthys[A]() creates an instance using an anonymous function" >> {

      val intTruthys = CanTruthy.truthys[Int](_ >= 0)

      intTruthys.truthys(1) must_== true
    }

    "Can 'pimp' type A via implicit conversion to CanTruthyOps[A] " >> {

      implicit val intTruthys = CanTruthy.truthys[Int](_ >= 0)
      import ToCanIsTruthyOps._

      1.truthy must_== true
      -1.truthy must_== false

      /*

       Let's break down how this works.

       - We've imported the examples, of course.
       - We've defined an implicit instance of CanTruthy[Int].
          - This defines what truthy means for Int.
          - We've used the factory method on the companion object to do this succinctly

       - By calling 1.truthy, Scala finds no 'truthy' on Int or its ancestors
         so it looks for an implicit conversion.
         - We've imported one from ToCanIsTruthyOps.
           - This, in turn, needs an implicit parameter of type CanTruthy[Int]
             - We got that, as described above
           - Now it can create a new CanTruthyOps, using the implicit parameter.
             This has the 'truthy' implementation, which delegates to CanTruthy[Int].
      */
    }
  }

  object CanTruthyExample {
    trait CanTruthy[A] { self =>
      def truthys(a: A): Boolean
    }

    object CanTruthy {
      def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
      def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
        def truthys(a: A): Boolean = f(a)
      }
    }

    trait CanTruthyOps[A] {
      def self: A
      implicit def F: CanTruthy[A]
      final def truthy: Boolean = F.truthys(self)
    }

    object ToCanIsTruthyOps {
      // Added return type for clarity.
      implicit def toCanIsTruthyOps[A](v: A)(implicit ev: CanTruthy[A]): CanTruthyOps[A] =
        new CanTruthyOps[A] {
          def self = v
          implicit def F: CanTruthy[A] = ev
        }
    }
  }
}
