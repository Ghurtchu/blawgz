# Functional error handling

It is an article about Functional error handling which will *Either* turn out to be *Success* or *Failure*. In my opinion it is *Right* to say that you have *None* to lose and *Some* to gain if you read it. So I suggest you to at least *Try* and check this out — the *Option* is *Left* to you 😎.

How many times have you blown up the stack on purpose by doing something like `throw new RuntimeException("Boom!")?` I myself have done that gazillion times and I know the drill.

I can guarantee you that you can do better if you learn a little about functional error handling.

The whole idea of functional error handling is to treat errors as ordinary values. With this principle in mind we distribute our attention evenly between successful and failed computations making both cases equally important.

Sure, there are situations when we fall back to raising errors because there is no other way out, however, sometimes we can avoid throwing exceptions and follow the systematic approach of functional error handling which inherently leads to writing better programs that are pure, composable and most importantly, stack safe.

In Scala standard library we have a few data structures which help us to practice what we preach, these are — `Option`, `Either` and `Try`.

It is highly likely that you’re already familiar with Option due to the fact that it’s present in almost all mainstream programming languages. `Option` is an effect that communicates the possible absence of value in any arbitrary computation whatsoever.

`Option` is ADT, a covariant sum type with two exclusive cases: `Some[+A]` — a placeholder of value and `None` — a case of absence, which extends `Option[Nothing]` and `Nothing` is a subtype of anything in Scala and all that conforms to covariance rules.

People have lots of different opinions about the benefits of `Option` but in my view it’s a functional analogue of working with `null` values. In fact `None` is a pure value equivalent of `null`.

Moreover, in Scala we rarely deal with nulls directly unless we work with Java API-s which are imperative in nature and even for them there are workarounds - we can write Scala API wrappers and make them use `Option` instead which turns our impure functions into pure ones and we all know that pure functions are gold!

Let’s see a few examples of `Option` usage:

```scala
object imperative {
  // impure function
  def midRange(nums: Seq[Double]): Double = {
    if (nums.isEmpty) throw new IllegalArgumentException("numbers must be non empty")
    else (nums.min + nums.max) / 2
  }
}

object functional {
  // pure function
  def midRange(nums: Seq[Double]): Option[Double] =
    Option.when(nums.nonEmpty)((nums.min + nums.max) / 2)
}
```

Why throwing an exception when we can simply return None?

```scala
object imperative_client { 
  import imperative._
  
  midRange(Seq.empty) // unsafe, blows the stack up :D 
}

object functional_client { 
  import functional._

  midRange(Seq.empty) // None, safe
  midRange(Seq(1, 2)) // Some(1.5), safe
  midRange(Seq(1))    // Some(1.0), safe
}
```

Now let’s see a more complicated example where the true power of these data structures come in — compositionality. All of those three data structures support `flatMap` and `map` methods that allow us to chain the computations which may be depended on previous ones.

Composition is my favourite characteristic of functional programming, especially in the domain of error handling because they inherently support short circuiting.

Imagine having a scenario where we’re trying to build an instance of our domain model which may fail for any arbitrary reason whatsoever.

Usually such constraints are imposed by our business requirements.

Consider an example:

```scala
final case class Transaction (
  id: TransactionId,
  from: Account,
  to: Account,
  amount: Amount,
  currency: Currency
)

final case class TransactionId private(value: String) extends AnyVal
final case class AccountId private(value: String) extends AnyVal
final case class OwnerId private(value: String) extends AnyVal
final case class Amount private (value: BigDecimal) extends AnyVal
final case class FirstName private (value: String) extends AnyVal
final case class LastName private (value: String) extends AnyVal

final case class Account (
  id: AccountId,
  owner: Owner
)

final case class Owner (
  id: OwnerId,
  firstName: FirstName,
  lastName: LastName
)

sealed trait Currency

object Currency {
  case object USD extends Currency
  case object EUR extends Currency
  case object GBP extends Currency

  // smart constructor
  def fromString: String => Option[Currency] = {
    case "USD" => Some(Currency.USD)
    case "EUR" => Some(Currency.EUR)
    case "GBP" => Some(Currency.GBP)
    case _     => None
  }
}
```

`Transaction` creation may fail for which we could use `Option` to signify that, but first we need to create constructors for domain models such as: `TransactionId`, `AccountId`, `Account` and so on, let’s start with simple ones — `ID`s:

```scala
object TransactionId { 
  import java.util.UUID._
  def gen: TransactionId = 
    new TransactionId(randomUUID().toString)
}

object AccountId { 
  import java.util.UUID._
  def gen: AccountId = 
    new AccountId(randomUUID().toString)
}

object OwnerId {
  import java.util.UUID._
  def gen: OwnerId = 
    new OwnerId(randomUUID().toString)
} 
```

Great, now let’s write smart constructors for the rest:

```scala
object Amount {
  def fromBigDecimal(amount: BigDecimal): Option[Amount] =
    Option.when(amount > 0 && amount <= 10_000)(new Amount(amount))
}

object FirstName {
  def fromString(name: String): Option[FirstName] =
    Option.when((1 to 20).contains(name.length))(FirstName(name))
}

object LastName {
  def fromString(name: String): Option[LastName] = 
    Option.when((1 to 30).contains(name.length))(LastName(name))
}
```

Now let’s try to instantiate `Transaction` with the help of for comprehensions which is a syntactic sugar over nested `flatMap` calls which ends with `yield` keyword that is essentially equivalent of calling final `map`:

```scala
// Some(Transaction(...))
val someTransaction: Option[Transaction] = for {
  fromFName <- FirstName.fromString("Nika")
  fromLName <- LastName.fromString("Ghurtchumelia")
  from       = Account(AccountId.gen, Owner(OwnerId.gen, fromFName, fromLName))
  toFName   <- FirstName.fromString("Martin")
  toLName   <- LastName.fromString("Odersky")
  to         = Account(AccountId.gen, Owner(OwnerId.gen, toFName, toLName))
  amount    <- Amount.fromBigDecimal(BigDecimal(5_000))
  currency  <- Currency.fromString("USD")
} yield Transaction(TransactionId.gen, from, to, amount, currency)

// Me paying huge props to Martin Odersky for creating such a beautiful programming language xD!
someTransaction match { 
  case Some(tx) =>
    println(s"${tx.from.owner.firstName} sending ${tx.currency} ${tx.amount} to ${tx.to.owner.firstName}")
  case _ => println("No money no honey")
}
```

Please notice that instantiation of `Transaction` may fail due to the following reasons:

Invalid `FirstName` — either empty or too long, more than 20 chars
Invalid `LastName` — either empty or too long, more than 30 chars
Invalid `Amount` — either non positive or too much, more than 10_000 units
Invalid `Currency` — any other currency than USD, EUR or GPB

Let’s see a failed attempt:

```scala
// None
val noTransaction: Option[Transaction] = for {
  fromFName <- FirstName.fromString("") // creation fails with short circuiting and for-comprehension exits with None
  fromLName <- LastName.fromString("Ghurtchumelia")
  from       = Account(AccountId.gen, Owner(OwnerId.gen, fromFName, fromLName))
  toFName   <- FirstName.fromString("Martin")
  toLName   <- LastName.fromString("Odersky")
  to         = Account(AccountId.gen, Owner(OwnerId.gen, toFName, toLName))
  amount    <- Amount.fromBigDecimal(BigDecimal(5_000))
  currency  <- Currency.fromString("USD")
} yield Transaction(TransactionId.gen, from, to, amount, currency)
```

As I already mentioned there are more than one ways where creating an instance of `Transaction` can go wrong and Voila! that’s where `Either` steps in. `Either` is similar to `Option` but it’s richer in terms of representing concrete failure reasons by incorporating a special domain error hierarchies within the data structure whereas `Option` only communicates possible absence of value.

`Either` gives you one out of two possible outcomes:

Successful value wrapped by `Right`
Customised failed value wrapped by `Left`

`Either` like `Option` is a covariant data structure having two subtypes: `Right[+A]` and `Left[+A]`. `Right` represents the success case while `Left` — a failure case. Most methods defined for `Either` are `Right`-biased such as: `map`, `flatMap` and so on.

Let’s rewrite the smart constructors defined for our business models and use `Either` instead of `Option`, the code will speak for itself:

```scala
sealed trait TransactionCreationError

object Currency {

  case object USD extends Currency
  case object EUR extends Currency
  case object GBP extends Currency

  trait UnsupportedCurrency extends TransactionCreationError
  object UnsupportedCurrency extends UnsupportedCurrency

  def fromString: String => Either[UnsupportedCurrency, Currency] = {
    case "USD" => Right(Currency.USD)
    case "EUR" => Right(Currency.EUR)
    case "GBP" => Right(Currency.GBP)
    case _     => Left(UnsupportedCurrency)
  }
}

object Amount {

  sealed trait AmountCreationError extends TransactionCreationError 

  object AmountCreationError {
    case object NonPositiveAmount extends AmountCreationError
    case object MoreThanTenK      extends AmountCreationError
  }
  
  import AmountCreationError._

  def fromBigDecimal(amount: BigDecimal): Either[AmountCreationError, Amount] = for {
    _ <- Either.cond(amount > 0, amount, NonPositiveAmount)
    _ <- Either.cond(amount <= 10_000, amount, MoreThanTenK)
  } yield Amount(amount)
}

object FirstName {

  sealed trait FirstNameCreationError extends TransactionCreationError

  object FirstNameCreationError {
    case object EmptyFirstName  extends FirstNameCreationError
    case object MoreThan20Chars extends FirstNameCreationError
  }
  
  import FirstNameCreationError._

  def fromString(firstName: String): Either[FirstNameCreationError, FirstName] = for {
    _ <- Either.cond(firstName.nonEmpty, firstName, EmptyFirstName)
    _ <- Either.cond(firstName.length <= 20, firstName, MoreThan20Chars)
  } yield FirstName(firstName)
}

object LastName {

  sealed trait LastNameCreationError extends TransactionCreationError

  object LastNameCreationError {
    case object EmptyLastName   extends LastNameCreationError
    case object MoreThan30Chars extends LastNameCreationError
  }
  
  import LastNameCreationError._

  def fromString(lastName: String): Either[LastNameCreationError, LastName] = for {
    _ <- Either.cond(lastName.nonEmpty, lastName, EmptyLastName)
    _ <- Either.cond(lastName.length <= 30, lastName, MoreThan30Chars)
  } yield LastName(lastName)
}
```

And now we have a concrete reason why `Transaction` instantiation may fail:

```scala
// Left(MoreThanTenK)
val transactionOrError: Either[TransactionCreationError, Transaction] = for {
  fromFName <- FirstName.fromString("James") 
  fromLName <- LastName.fromString("Gosling")
  from       = Account(AccountId.gen, Owner(OwnerId.gen, fromFName, fromLName))
  toFName   <- FirstName.fromString("Martin")
  toLName   <- LastName.fromString("Odersky")
  to         = Account(AccountId.gen, Owner(OwnerId.gen, toFName, toLName))
  amount    <- Amount.fromBigDecimal(BigDecimal(50_000)) // for comprehension short circuits with Left(MoreThanTenK)
  currency  <- Currency.fromString("USD")
} yield Transaction(TransactionId.gen, from, to, amount, currency)

transaction match {
  case Right(tx) =>
    println(s"${tx.from.owner.firstName} sending ${tx.currency} ${tx.amount} to ${tx.to.owner.firstName}")
  case Left(reason) => println(s"failed due to $reason")
}
```

Ok, nice, `Either` is cool!

What about `Try` though? `Try` is a functional equivalent of imperative `try/catch` procedures which has a special constructor that receives a by name parameter that is unevaluated at call site thereby guaranteeing stack safety.

`Try` constructor looks like this:

```scala
object Try { 
  def apply[A](thunk: => A): Try[A] = 
    try Success(thunk) // try returning value wrapped by Success
    catch { case t: Throwable => Failure(t) } // if fails, return error wraped by Failure
}
```

You may think of `Try` as a specialised `Either` with a fixed error channel parameterised by `java.lang.Throwable` as a supertype, so the mental model looks like `Either[Throwable, A]` where `A` is a generic type parameter.

Like `Either` and `Option`, `Try` is also covariant sum type and the concrete subtypes may be either `Success[+A]` or `Failure[Throwable]` representing successful and failed computations respectively.

Let’s compare imperative `try/catch` to functional `Try`:

```scala
import scala.util.{Try, Success, Failure}

object imperative {
  // look at all that imperative verbose jazz...
  try { 
    println(1 / 0)
  } catch { 
    case ae: ArithmeticException => println(s"failed due to ${ae.getMessage}")
  }

  val nums = (1 to 3).toList
  try {
    println(nums(5))
  } catch {
    case iobe: IndexOutOfBoundsException => println(s"failed due to ${iobe.getMessage}")
  }

}

object functional { 
  // versus nice functional analogue
  Try(1 / 0) match { 
    case Success(v) => println(v)
    case Failure(t) => println("failed due to $t") // Failure(ArithmeticException("/ by zero"))  
  }

  val nums = (1 to 3).toList
  Try(nums(5)) match { 
    case Success(v) => println(v)
    case Failure(t) => println("failed due to $t") // Failure(java.lang.IndexOutOfBoundsException: 5)
  }
}
```

And of course you can use methods defined on `Try` which take higher order functions to enable functional programming:

```scala
import scala.util.Try

objet example_1 { 
  val input = "I love Scala"
  val res: String = Try(input.substring(50, 100))
    .fold(_ => "", _.toUpperCase) // "", since it fails with IndexOutOfBoundsException
}

object example_2 { 
  val filePath = "my_project/config.json"

  // return Config.default if config loading fails
  val cfg1 = Try(loadConfig(filePath))
    .getOrElse(Config.default)

  // or if you'd like to be more specific
  // recover with Config.default if loadConfig fails with FileNotFoundException
  val cfg2 = Try(loadConfig(filePath)).recover { 
    case _: FileNotFoundException => Config.default
  }
}

object example_3 { 
  def isNumber(input: String): Boolean = 
    Try(input.toDouble).isSuccess

  isNumber("boom") // false
  isNumber("42") // true
}
```

`Try`, `Option` and `Either` are interoperable with each other, a few examples to demonstrate that:

```scala
Try(42 / 6).toOption      // Some(7)
Try(42 / 0).toOption      // None
Try(42 / 6).toEither      // Right(7)
Try(42 / 0).toEither      // Left(ArithmeticException)
Right(10).toOption        // Some(10)
Left("Error").toOption    // None
Some(10).toRight("error") // Right(10)
None.toRight("Error")     // Left("Error")
Some(10).toLeft(20)       // Left(10)
None.toLeft(50)           // Right(50)
```

So, the big idea is that `Try`, `Option` and `Either` help us in treating errors as first class values instead of throwing exceptions.

Representing errors as values and operating on them via higher-order functions enables us to encapsulate and propagate them to lower channels where they can be treated properly.

If you’re interested in more advanced treatment of this topic I’d suggest you to check out `cats` library and especially `cats.data.Validated`.

And remember, no matter what it is — just ***flatMap that shit***.
