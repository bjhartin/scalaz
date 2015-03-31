package com.banno.scalaz

import org.specs2.mutable.Specification
import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._

class OptionAsMonoidSpec extends Specification {

  "Given the OptionAsMonoid example code" >> {

    import OptionAsMonoidExample._

    "Option as monoid" >> {
      /*

       Scalaz defines Option as a monoid only when its
       type parameter A is a semigroup.  This means A
       supports mappend (|+|).

       implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] =
         new Monoid[Option[A]] {...}

       Hmm...A doesn't have to be a monoid though, unlike Haskell.
       What's the difference?

       Semigroup: an operator ($) which is associative and closed under a domain
       Monoid: a semigroup along with an identity element (called zero), and for which
               x $ zero = zero $ x (commutativity when involving the identity)

       So, defining Option[A: Semigroup] to be a monoid means ScalaZ must
       provide the identity and limited commutativity.  Inspecting the ScalaZ code,
       we see both:

       1) def zero: Option[A] = None

       2) [...]
          case (Some(a1), None)     => f1
          case (None, Some(a2))     => f2

       In other cases, ScalaZ will call append on the values in the Option
       themselves, since they're part of a semigroup:

       case (Some(a1), Some(a2)) => Some(Semigroup[A].append(a1, a2))

       */


      val ident = (None: Option[String])
      val x = "x".some

      ident |+| x must_== x
      x |+| ident must_== x
    }


    "Alternate definitions of Option as Monoid, using tagged types" >> {

      /*

       It is neither the type (domain), nor the operation which identifies
       a structure such as a semigroup, monoid, group or ring.  It's the
       combination.  So, we can have different behavior for Option if we
       tag its type.

      */


      val a = "a".some
      val b = "b".some

      Tags.First(a) |+| Tags.First(b) must_== a
      Tags.Last(a) |+| Tags.Last(b) must_== b
    }

  }

  object OptionAsMonoidExample {}
}
