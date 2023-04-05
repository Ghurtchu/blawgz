# Immutability by example — Building a purely functional data validator

In this blogpost while evangelising immutability we end up creating a useful reusable piece of code as a side effect which addresses the limitations of Either[+E, +A]** in Scala standard library with respect to performing accumulative data validations.

A bold statement which I’ll justify later:

**“Immutable State is king!”**

Functional data structures are by definition immutable meaning that their inner state can not be modified in place.

There are many benefits to this but the most explicit ones are:

- Code becoming easier to reason about
- Localising state
- Thread safety
- Eliminating a whole class of bugs related to mutable state (functional programmers being lazy to list all the reasons :D)

While working with immutable data it is worth it to keep in mind that we need to make data copies and return new values instead of changing existing ones and possibly introducing bugs related to mutable state.

A small example for demonstrating the basic immutability:
```scala
final case class Person(name: String, age: Int)

val me = Person("Nika", 23)
val myAlterEgo = me.copy(name = "Anzori") 
// new value returned, thread-safe, pure, easy to inspect in any scope
```

A counterexample involving mutable state would look like:

```scala
class Person(var name: String, var age: Int) { 
  // thread unsafe
  // side-effecting function that modifies the field value in place
  def setName(newName: String): Unit = 
    this.name = newName
}

val me = new Person("Nika", 23)
me.setName("Anzori") // mutable state, difficult to inspect in any scope
```

Ok great, back to our goal, what is the problem of **Either[+E, +A]?** Unfortunately it’s not well suited for accumulating validation failures because by design it supports short circuiting.

Imagine we have a following:

```scala
final case class Person private (name: String, age: Int)

object Person { 

  sealed trait PersonValidationError
  
  object PersonValidationError { 
    case object EmptyName extends PersonValidationError
    case object NonAdult  extends PersonValidationError
  }
  
  import PersonValidationError._

  // constructor which "fails fast" with the first occurring error
  def fromNameAndAge(name: String, age: Int): Either[PersonValidationError, Person] = 
    for { 
      _ <- Either.cond(name.nonEmpty, name, EmptyName) // in case of failure for-comprehension exits here aka "fail fast"
      _ <- Either.cond(age >= 18, age, NonAdult)
    } yield Person(name, age)
}
```

So, if we try to build a Person by giving it an empty name it’s not gonna check age validity at all and will “fail fast” by returning **Left(PersonValidationError.EmptyName)**.

```scala
val p = Person.fromNameAndAge(name = "", age = 10)
// Left(PersonValidationError.EmptyName)
// however we know that it would have also failed the second rule if it checked age
```

Instead we want to check all rules and accumulate all the failed validations so that we know all the reasons why **Person** validation may fail.

In fact it may have nothing to do with creating a **Person** instance at all. Validations are more general and they may be associated to completely different things.

We may already have a running instance of **Person** and just want to check him/her against some eligibility rules defined for some other domain, e.g if **Person** is eligible to participate in game, if **Person** is eligible to be a patron, if **Person** is eligible to contribute and so on.

While writing a solution for this problem we will be using the principles of immutability. We won’t be changing state in place and we won’t be reassigning any variables in any scope.

This is the kind of API we want to have in the end:

```scala
val person = Person("", 10) // both validations should fail

val validatedPerson =
  Validator(person)
    .satisfying(_.name.nonEmpty, PersonValidationError.EmptyName)
    .satisfying(_.age >= 18, PersonValidationError.NonAdult)
    .applied

validatedPerson match {
  case Validated.Passed(value)  => println(s"passed $value")
  case Validated.Failed(errors) => println(s"failed: $errors") // List(EmptyName, NonAdult)
}
```

Let’s begin!

We will have a sealed trait **Validated[+E, +S]** where **E** is a type for errors and **S** — successful value.

Both types are prefixed with **+** sign which means that it’s going to be Covariant data structure — producing values. This will help us in type inference issues later.

We’ll have two subtypes: **Passed[S]** and **Failed[E]**:

```scala
sealed trait Validated[+E, +S]

object Validated {
  final case class Passed[S](value: S)        extends Validated[Nothing, S]
  final case class Failed[E](errors: List[E]) extends Validated[E, Nothing]
}
```

Why **Nothing** there? We use **Nothing** where necessary because **Nothing** is a subtype of any type in **Scala** and it conforms to covariance rules as well.

So **Passed[S](value: S)** extends **Validated[Nothing, S]** means that **Passed[S]** will not have any values in **E** type which is meant to be representing errors. Same goes to **Failed[E]** — it won’t have any values in **S** which is meant to be successful validated value.

Great! we have **ADT** that represents successful and failed validations.

Now we need something that can validate our data — **Validator**.

```scala
// covariant in E which is a placeholder for errors, S - success.
trait Validator[+E, S] {
  // input is going to be validated
  protected def input: S

  // this is going to be used for accumulating predicates which failed
  protected def failedPredicates[E1 >: E]: List[PredicateMeta[E1, S]] = List.empty // default starting value
}

object Validator {

  def apply[E, S](s: S): Validator[E, S] = new Validator[E, S] {
    override def input: S = s
  }

  // a tuple-like case class which represents predicate with its' failure reason
  // e.g PredicateMeta[String, Int](i => i % 2 == 0, "Number is odd")
  protected[Validator] final case class PredicateMeta[+E, S](f: S => Boolean, failureReason: E)
}
```

As I already showed you we will have two methods solving different problems:

**satisfying** — registering a tuple of predicate and the reason of validation failure (e.g negative amount, empty string and so on)
**applied** — a method that returns a concrete subtype of **Validated[E, S]** (e.g **Passed[Person]**, **Failed[String]** and so on)

So, let’s rewrite trait **Validator[+E, S]** by first adding **satisfying**:

```scala
trait Validator[+E, S] { previous => // for referring to the "outer" state

  import Validator.PredicateMeta

  protected def input: S

  protected def failedPredicates[E1 >: E]: List[PredicateMeta[E1, S]] = List.empty
 
              // E1 >: E means "type widening"
  def satisfying[E1 >: E](f: S => Boolean, failureReason: E1): Validator[E1, S] = new Validator[E1, S] {
    override def input: S = previous.input // copying outer input
                         // again E2 >: E1 "type widening"
    override def failedPredicates[E2 >: E1]: List[PredicateMeta[E2, S]] =
      if (f(input)) previous.failedPredicates // if predicate holds returning the same failed predicates
      else PredicateMeta[E2, S](f, failureReason) :: previous.failedPredicates // adding failed case to the head if predicate does not hold
  }
}
```

So, satisfying will just add the failed predicate to the previous failed predicates if the current predicate does not hold and return a new state of **Validator[+E, S]** where it will have one more element in **failedPredicates**.

And now let’s add the final method — applied which either returns the accumulated errors wrapped by **Failed** or validated data wrapped by **Passed**:

```scala
trait Validator[+E, S] { previous =>

  import Validator.PredicateMeta

  protected def input: S

  protected def failedPredicates[E1 >: E]: List[PredicateMeta[E1, S]] = List.empty

  def satisfying[E1 >: E](f: S => Boolean, failureReason: E1): Validator[E1, S] = new Validator[E1, S] {
    override def input: S = previous.input

    override def failedPredicates[E2 >: E1]: List[PredicateMeta[E2, S]] =
      if (f(input)) previous.failedPredicates
      else PredicateMeta[E2, S](f, failureReason) :: previous.failedPredicates
  }

  // match on failedPredicates and if it's empty return the input
  // or else return reversed (in order to preserve order) failure reasons
  def applied: Validated[E, S] = failedPredicates match {
    case Nil => Validated.Passed(input)
    case _   => Validated.Failed(failedPredicates.map[E](_.failureReason).reverse)
  }
}
```

That’s it, here you have it — an immutable **Validator** that accumulates all of the failed validations which are registered by client without mutating the current state.

One more time, let’s see the typical usage:

```scala
final case class Point(x: Int, y: Int)

object Point {

  sealed trait PointValidationError

  object PointValidationError {
    case object NegativeX extends PointValidationError
    case object NegativeY extends PointValidationError
  }
}

import Point.PointValidationError

val point = Point(-10, -20) // both should fail

val validatedPoint =
  Validator(point)
    .satisfying(_.x >= 0, PointValidationError.NegativeX)
    .satisfying(_.y >= 0, PointValidationError.NegativeY)
    .applied

validatedPoint match {
  case Validated.Passed(value) => println(s"got $value")
  case Validated.Failed(errors) => println(s"failed due to: $errors") // this will be executed
  // failed due to: List(NegativeX, NegativeY)
}
```

Please notice that in the client we haven’t added any types ourselves hence — the type inference powered by the covariance and the Scala compiler.

The things we used such as: **+E**, **E1 >: E** and **E2 >: E1** helped us to achieve that.

Source code: https://gist.github.com/Ghurtchu/904049da40a3fb892c73490e4d83f64c
