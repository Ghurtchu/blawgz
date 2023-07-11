# Comprehensive data validation in Functional Programming — A case study

Data validation is no new thing in software development, it’s been here all along and will continue to stay with us. Comprehensive data validation leads to achieving the following goals:
- Disallowing anyone to represent the illegal state in our apps
- Accumulating all of the exact reasons for the validation failure (Can not be achieved by using `Either[E, A]` since it’s a monadic structure — supports short circuiting in `flatMap` calls and returns to the caller as soon as it encounters the first `Left` case, so we need something different)

In this blog post we will solve the practical problem — We will write the fully fledged validation for the case where the user is trying to create the password for his/her profile.

Our goal is to accumulate validation errors so that the user is able to view all the faults on the UI while at the same time trying to fix the password.

Please keep in mind that we need to make sure that the provided password conforms to the security standards i.e it must be validated against four rules imposed by our business case:

- It must be non empty (who would’ve thought?! 😃)
- It must contain at least one uppercase character
- It must contain more than 8 characters to be classified as sufficiently long enough
- It must contain at least one of these special symbols: !@#$%^&*()

We will make us of the famous `cats` library and more importantly the constructions we have under `cats.data` package, things such as: `Validated` (addresses the limitations of `Either[E, A]`), `NonEmptyList` and a special type alias associated with the aforementioned.

First of all, let’s get familiar with `Validated` and `NonEmptyList` and then proceed with the implementation. 

`Validated` is a [sum type](https://scalajobs.com/blog/functional-data-modeling-in-scala/) data structure which is represented either as `Valid` or `Invalid` that wraps the underlying data that is subject to validation.

A simple example demonstrating the hierarchy of `Validated`:

```scala
sealed trait Validated[+E, +A] { 
  // a bunch of abstract methods
}

object Validated {
  final case class Valid(data: A) extends Validated[Nothing, A]
  
  final case class Invalid(error: E) extends Validated[E, Nothing]
}
```

So, clearly the left part `+E` is dedicated to hold validation errors while the right part `+A` is used to store the passed values.

At the first glance it looks pretty much the same as `Either[+E, +A]` with its `Left` and `Right` parts but it works in a different way in practice.

A simple example of creating `Valid` data:

```scala
import cats.data.Validated

val validPass: Validated[Nothing, String] = 
  Validated.Valid("MyPassword!2#")
```

A simple example of creating `Invalid` data:

```scala
import cats.data.Validated

val invalidPass: Validated[String, Nothing] = 
  Validated.Invalid("The password is invalid due to ...")
```

We can also use the extension methods that helps us to wrap anything with `Validated

```scala
import cats.syntax.all._

val validPass: Validated[Nothing, String] = 
  "MyPassword!2#".valid

val invalidPass: Validated[String, Nothing] = 
  "The password is invalid due to ...".invalid
```

Simple bruv, innit?

Let’s move on `NonEmptyList`. As the name suggest it’s basically a non empty list. Sometimes `NonEmptyList` is referred as `Nel` by geeks (pun intended on `Nil` which is an empty list 😄) and it’s based on singly linked list implementation.

A simple demonstration of the `NonEmptyList` usage:

```scala
import cats.data.NonEmptyList

val oneElementList: NonEmptyList[Int] = 
  NonEmptyList.one(1)

val maybeNonEmptyList1: Option[NonEmptyList[Int]] = 
  NonEmptyList.fromList(Nil) // None, due to Nil param

val maybeNonEmptyList2: Option[NonEmptyList[Int]] = 
  NonEmptyList.fromList(List(1, 2, 3)) // Some(...) due to List(1, 2, 3) param

NonEmptyList.fromListUnsafe(Nil) // throws Exception due to Nil param
```

Even simpler, innit?

And now let’s see the very type alias which suits our needs:

```scala
type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]
```

So, after applying the validation rules we’re either getting the `NonEmptyList` of domain errors — `E` or underlying data — `A` that has passed the validation rules.

Great! Now, let’s create a domain model for password and possible error hierarchy `ADT` associated with it:

```scala
final case class Password(value: String) extends AnyVal // value class

object Password {
  // validation error ADT
  sealed trait ValidationError

  object ValidationError {
    case object IsEmpty  extends ValidationError
    
    case object LacksUppercaseChar extends ValidationError
    
    case object IsShort extends ValidationError
    
    case object LacksSpecialSymbol extends ValidationError
  }
}
```

Looks pretty straightforward so far, now let’s design a protocol which describes the password validation on the high level:

```scala
import Password.ValidationError

trait PasswordValidations {
  def validate(password: String): ValidatedNel[ValidationError, Password]
}
```

And finally let’s write the implementation in the companion object of `PasswordValidations`:

```scala
def create: PasswordValidations = password => {

  import Password.ValidationError
  import Password.ValidationError._

  def validate(
    condition: Boolean,
    failureReason: ValidationError
  ): ValidatedNel[ValidationError, Unit] =
    if (!condition) failureReason.invalidNel 
    else ().valid

  def isNonEmpty =
    validate(password.nonEmpty, IsEmpty)

  def containsUpperCaseCharacter =
    validate(password.exists(_.isUpper), LacksUppercaseChar)

  def isLong =
    validate(password.length > 8, IsShort)

  def hasSpecialSymbol =
    validate(
      password.exists(c => "!@#$%^&*()".contains(c.toString)),
      LacksSpecialSymbol
    )
  
  // Here we need to call the following methods: 
  // 1. isNonEmpty
  // 2. containsUpperCaseCharacter
  // 3. isLong
  // 4. hasSpecialSymbol
  // and accumulate errors in the Invalid channel if there are any
  // or return the successful password wrapped by Valid constructor

  ???
  }
}
```

We’re almost there, now we need a way to use all of our four inner validation functions and:

- if at least one or more of them fails — return concatenated `Invalid(NonEmptyList[ValidationError](...))`
- if none of them fails — return `Valid(Password(...))`

There are a few ways to achieve this but I’ll show you my favourite approach that involves using `.tupled` extension method which is defined on tuples of arbitrary values that are wrapped by something like `Option`, `Either` or in our case `Validated`.

Under the hood `.tupled` is using `Semigroupal` typeclass and product operation to achieve the following:

```scala
import cats.syntax.all._

val maybe1 = Option(1)
val maybe2 = Option(2)

val result: Option[(Int, Int)] = (maybe1, maybe2).tupled // Some(1, 1)
```

In our case the tuple will contain the inner function calls with the return type of: `ValidatedNel[ValidationError, Unit].`

So, we’d write something like:

```scala
import cats.syntax.all._

// .tupled concatenates the NonEmptyList[ValidationError] if they are Invalid
// and tuples the Unit values if are Valid
val result: ValidatedNel[ValidationError, (Unit, Unit, Unit, Unit)] = 
(
  isNonEmpty,
  containsUpperCaseCharacter,
  isLong,
  hasSpecialSymbol
).tupled
```

But.. it’s not enough, we want to have this type of signature in the end: `ValidatedNel[ValidationError, Password]`, so we could easily `map` the result like:

```scala
result.map(_ => Password(password)) // ValidatedNel[ValidationError, Password]
```

or use `.as` extension method which makes it even more succinct and readable:

```scala
result.as(Password(password)) // ValidatedNel[ValidationError, Password]
```

So, now let’s put it all together:

```scala
object PasswordValidations {

  def create: PasswordValidations = password => {

    import Password.ValidationError
    import Password.ValidationError._

    def validate(
      condition: Boolean,
      failureReason: ValidationError
    ): ValidatedNel[ValidationError, Unit] =
      if (!condition) failureReason.invalidNel else ().valid

    def isNonEmpty =
      validate(password.nonEmpty, IsEmpty)

    def containsUpperCaseCharacter =
      validate(password.exists(_.isUpper), LacksUppercaseChar)

    def isLong =
      validate(password.length > 8, IsShort)

    def hasSpecialSymbol =
      validate(
        password.exists(c => "!@#$%^&*()".contains(c.toString)),
        LacksSpecialSymbol
      )
    
    (
      isNonEmpty,
      containsUpperCaseCharacter,
      isLong,
      hasSpecialSymbol
    ).tupled.as(Password(password))
  }
}
```

Now let’s write the tests for our validation service:

Expecting `Invalid(NonEmptyList(IsEmpty, LacksUppercaseChar, IsShort, LacksSpecalSymbol))` since all of the four rules fail:

```scala
import Password.ValidationError._

val service = PasswordValidations.create
val expected = NonEmptyList.fromListUnsafe {
  List(IsEmpty, LacksUppercaseChar, IsShort, LacksSpecialSymbol)
}.invalid

assert(service.validate("") == expected)
```

Expecting `Invalid(NonEmptyList(LacksUppercaseChar, IsShort)):`

```scala
import Password.ValidationError._

val service = PasswordValidations.create
val expected = NonEmptyList.fromListUnsafe {
  List(LacksUppercaseChar, IsShort)
}.invalid

assert(service.validate("pa$$") == expected)
```

Expecting `Valid(Password(...))` since it conforms to all of the four rules:

```scala
import Password.ValidationError._

val service = PasswordValidations.create
val pass = "Very$trongPassword123"
val expected = Password(pass).valid

assert(service.validate(pass) == expected)
```

I hope you enjoyed it and learned something new today!
