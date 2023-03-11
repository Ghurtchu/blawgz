# Typeclassery — A sure way of making generic programs context aware

Sooner or later you will stumble upon the concept of type classes while traversing your Scala journey. Type class is not something peculiar about Scala, rather it’s a special design approach that is aimed at achieving ad-hoc polymorphism in statically typed functional programming languages, originating from the one and only — Haskell.

What in the world is ad-hoc polymorphism? You’ve probably heard of:

- Subtype polymorphism
- Parametric polymorphism

Let’s review them before we dive deep into the dark magic of type classes.

**B Subtype Polymorphism 101 🐥** — traditional A extends B relationship, hence passing B in place of whereA is expected is totally fine.

A simple example:

```scala
sealed trait PlayerAction

object PlayerAction {
  
  sealed trait Party

  object Party { 
    case object Spartans extends Party
    case object Athens   extends Party
  }

  final case class ChooseParty(side: Party)         extends PlayerAction
  final case class Attack(against: Party)           extends PlayerAction
  final case class Retreat(coordinates: (Int, Int)) extends PlayerAction
}

object PlayerActionLogger extends App { 
  
  // a method that handles all concrete subtypes of PlayerAction
  def logPlayerAction: PlayerAction => Unit = {
    case ChooseParty(side)    => s"Player just joined $side"
    case Attack(against)      => s"Player is about to attack $against"
    case Retreat(coordinates) => s"Player will hide at $coordinates"
  }
  
  import PlayerAction._
  
  val playerActions = 
    ChooseParty(Party.Spartans) :: Attack(Party.Athens) :: Retreat((100, 200)) :: Nil

  playerActions.foreach(logPlayerAction)
}
```

**Parametric Polymorphism 101 🐥** —program implementation is type agnostic, hence written only once for all types, eliminating code duplication.

A simple example:
```scala
sealed trait Tree[+A] { self =>
  // type agnostic method that applies a function to each element an transforms the whole tree
  def map[B](f: A => B): Tree[B] = self match {
    case Tree.Node(current, left, right) => Tree.Node(f(current), left map f, right map f)
    case Tree.Leaf(current)              => Tree.Leaf(f(current))
  }
}

object Tree {
  final case class Node[A](current: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](current: A)                                extends Tree[A]
}

object TreeTransformations extends App { 
  
  import Tree._

  val tree: Tree[Int] = Node(1, Node(2, Leaf(3), Leaf(4)), Node(5, Leaf(6), Leaf(7)))
  val prosperityTree: Tree[Int] = tree.map(_ * 5) // multiply each value by 5
  val stringifiedTree: Tree[String] = tree.map(_.toString) // stringify
  val tupledTree: Tree[(Int, Int)] = tree.map(i => (i, i)) // tupled
  val optionTree: Tree[Option[Int]] = tree.map(Option.apply) // wrap each element in Option

  /* Please notice, that all these transformations are implemented
   * with a single 'map' definition, that works for any type.
   * This is a basic parametric polymorphism
   */
}
```

Ok, makes sense 🆒!

What about Ad-hoc polymorphism? If you combine it with aforementioned ones you will get a true, powerful polymorphism.

Ad-hoc polymorphism enables you to enrich generic programs by providing them with type classes — generic interfaces that describe the specific capabilities of a type which may behave differently at runtime. In Scala this is achieved by implicit resolution.

A type class can be as abstract as something that is capable of:
- combining two types
- comparing two types
- formatting any type to String
- serialising any type to Json ADT (Algebraic Data Type)
- abstracting over things that can be mapped
- and so on..

You get the idea.

Now, here is the big why — why do we need it? what’s so special about it? Well, the answer is — with type classes our generic programs become context aware, meaning that once we provide them with type classes we can leverage this to create highly abstract programs, which are extremely flexible and configurable and most importantly — written only once.

Consider a simple example — we need to create a program which takes a list of things and reduces it to a single value. Can we write it only once? For any type whatsoever? Yes! as long as we have a `Monoid` instance for that type. Here begins the fun 😄.

`Monoid` is a big word for something that is extremely simple, the code below is pretty self explanatory:

```scala
trait Semigroup[A] { 
  // Semigroup is a type class which is capable of combining two types into one
  def combine(left: A, right: A): A
}

trait Monoid[A] extends Semigroup[A] { 
  // Monoid is a Semigroup, with additional "zero" method indicating empty case
  def zero: A
}
```

Ok, now let’s imagine we don’t have any monoids and we want to implement the same functionality without them.

A typical attempt could look like:

```scala
def reduce[A](list: List[A]): A = 
  list.reduce((a, b) => ???) // we're stuck here, we don't know how to combine them 
```
An implementation with the help of Monoid type class:

```scala
def reduce[A](list: List[A])(implicit monoid: Monoid[A]): A = 
  list.fold(monoid.zero)(monoid.combine) // eta expansion
/*same as
 *list.fold(monoid.zero)((a, b) => monoid.combine(a, b))
 */
```

The implementation above is going to work for any type as long as we have an implicit `Monoid` instance in scope. I don’t know about you but I find it really beautiful and elegant, if the code below does not convince you then I will declare bankruptcy 😄.

```scala
final case class PurchasedItems(items: List[String], totalPrice: BigDecimal)

object PurchasedItems {
  // this will be implicitly injected into reduce method without us passing it explicitly
  implicit val purchasedItemsMonoid: Monoid[PurchasedItems] = new Monoid[PurchasedItems] {
    override def zero: PurchasedItems = PurchasedItems(Nil, 0)

    override def combine(left: PurchasedItems, right: PurchasedItems): PurchasedItems =
      PurchasedItems(left.items ::: right.items, left.totalPrice + right.totalPrice)
  }
}

object Program extends App {
 
  def reduce[A](list: List[A])(implicit monoid: Monoid[A]): A = 
    list.fold(monoid.zero)(monoid.combine)

  val items = List(
    PurchasedItems(items = List("banana", "chocolate"), totalPrice = 5.5),
    PurchasedItems(items = List("t-shirt", "trousers"), totalPrice = 150),
    PurchasedItems(items = List("chocolate", "berries"), totalPrice = 7.5)
  )

  println(reduce(items))
  // PurchasedItems(List(banana, chocolate, t-shirt, trousers, chocolate, berries), 163.0)
} 
```

If you’re still not convinced I can show you more complicated example involving famous `M` word — `Monad`.

Remember, when you hear `Monad` it means whatever it is — you can ***flatMap that shit***.

Monads from the Scala standard library are: `Option`, `Either`, `List`, `Try`, `Future` and so on.

Let’s say we have an online shop system that deals with a large set of users and currently we are planning to ship a new feature which would give them an opportunity to participate in the lottery organised by our company.

This can be implemented in many ways and I really trust your rich imagination, however let’s follow the simplified approach to better illustrate the mechanics.

Technically, we need to read the user from the database, validate it against some eligibility rules defined by business and then generate a lottery link for them to which they can navigate to and register.

We need to define three things for implementing this feature:
- Algebras (A set of operations, abstract formula)
- Interpreters (Concrete implementations for algebras)
- Programs (Conjunction of Interpreters)

Before all that, let’s define our domain:

```scala
sealed trait Status

object Status {
  case object Vip    extends Status
  case object Normal extends Status
  
  // smart constructor
  def fromString(status: String): Option[Status] = status match {
    case "Vip"    => Some(Vip)
    case "Normal" => Some(Normal)
    case _        => None
  }
}

final case class Link(value: String) extends AnyVal // value class
final case class User(userId: String, name: String, email: String, status: Status, boughtItems: Int)
```

Let’s start with defining Algebras — interfaces for which we will create interpreters later:

```scala
trait Users[F[_]] {
  def lookup(userId: String): F[User]
}

trait UserValidations[F[_]] {
  def validate(user: User): F[User]
}
```

The interesting thing is that we can skip the second step (creating concrete implementations — interpreters for Algebras) and actually write a program which is purely coded to an interface and is effect `F[_]` agnostic.

However there is one restriction for our program to work properly apart from remaining abstract and type agnostic. Since the program logic requires to have `flatMap` and `pure` methods we need to have an implicit `Monad` instance in scope. `flatMap` calls are aimed at chaining computations, whereas `pure` method wraps the value into the type constructor.

So, as long as we have `Monad` instance in scope, our program compiles. In this example the concrete `Monads` will be `Either` and `Option`.

Let’s use famous `cats` library to write the program for generating lottery links for each user with the help of `Monads`:

```scala
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._

final case class LotteryLinkGenerator[F[_]: Monad](
  users: Users[F],
  userValidations: UserValidations[F],
 ) {
  def genLink(userId: String): F[Link] = for {
    user      <- users.lookup(userId) // flatMap
    validUser <- userValidations.validate(user) // flatMap
    link      <- Link(s"https://monad-lottery.com/secure?userId=${validUser.userId}").pure[F] // pure
  } yield link
}
```

Cool, we have a program which depends on two algebras (`Users[F]`, `UserValidations[F]`) and `Monad`
type class. By depending on Monad type class we abstract over effect type `F[_]`, in this case for `Option` and `Either`, meaning that this program will work for both of them.

Let’s define some domain error hierarchy as well:

```scala
sealed trait AppError

sealed trait DbError extends AppError

object DbError {
  case object InvalidId      extends DbError
  case object UnknownDbError extends DbError
}

sealed trait ValidationError extends AppError

object ValidationError {
  case object NonVipStatus   extends ValidationError
  case object PassiveSpender extends ValidationError
}
```

For simplicity we will have a fake database client which always returns the same response:

```scala
final case class QueryResult(private val fields: Map[String, String]) {
  def read(key: String): Option[String] = fields.get(key)
}

trait DatabaseClient {
  def execute(query: String): Option[QueryResult] = Some {
    QueryResult {
      Map(
        "user_id"     -> "1",
        "name"        -> "nika",
        "email"       -> "flatmap@that.shit",
        "status"      -> "Vip",
        "boughtItems" -> "100"
      )
    }
  }
}
```

Let’s implement Users interpreters typed with concrete effects — `Either` and `Option`. `Option` just describes a possible absence of value whereas `Either` either returns `Right` or `Left` subtypes but not both. `Either` is right-biased, meaning that whatever is wrapped in `Right` represents the success case, `Left` — a failure case.

A cool thing with `Either` is that it can be typed with domain error hierarchies which helps us to incorporate context into our programs, e.g we will use a specific `AppErrorOr[A]` type alias to implement the interpreters:

```scala
// fails with subtype of AppError or succeeds with A
type AppErrorOr[A] = Either[AppError, A] // either Left(AppError) or Right(A)
```
```scala
object Users {

  def ofOption(implicit dbClient: DatabaseClient): Users[Option] = userId => {
    val queryRes = dbClient.execute(s"select * from user where user.user_id = $userId")

    queryRes.fold[Option[User]](None)(parseQueryResultToUser)
  }

  def ofEither(implicit dbClient: DatabaseClient): Users[AppErrorOr] = userId => {
    val queryRes = dbClient.execute(s"select * from user where user.user_id = $userId")

    queryRes.fold[AppErrorOr[User]](Left(DbError.UnknownDbError("Who knows what went wrong...")))(parseQueryResultToUser(_).toRight(DbError.InvalidId))
  }

  private def parseQueryResultToUser(res: QueryResult): Option[User] = for {
      userId      <- res.read("user_id")
      name        <- res.read("name")
      email       <- res.read("email")
      status      <- res.read("status").flatMap(Status.fromString)
      boughtItems <- res.read("boughtItems").flatMap(items => Try(items.toInt).toOption)
    } yield User(userId, name, email, status, boughtItems)
}
```

Let’s do the same for UserValidations:

```scala
object UserValidations {

  def ofOption: UserValidations[Option] = user =>
    Option.when(user.status == Status.Vip && user.boughtItems >= 50)(user)

  def ofEither: UserValidations[AppErrorOr] = user => for {
    _ <- Either.cond(user.status == Status.Vip, user, ValidationError.NonVipStatus)
    _ <- Either.cond(user.boughtItems >= 50, user, ValidationError.PassiveSpender)
  } yield user  

}
```

So, now we can create two values of `LotteryLinkGenerator`, one parameterised by `Option` and another one — by `Either` and we can observe the output. I will remind you, that we wrote the main program logic — `LotteryLinkGenerator.genLink` only once which works for any effect `F[_]` as long as it has a `Monad`, meaning it has `pure` and `flatMap` methods defined `implicitly`. We achieved this by implementing different interpreters for `Users[F]` and `UserValidations[F]` traits and `implicitly` injecting `Monad` instances for `Option` and `Either`.

What helped us to achieve such a wonderful result? `Monads`! Yes, `Monad` type class for `Option` and `Either` helped us to do that since both of these data structures support `flatMap` and `pure` methods which can be generalised.

The client:

```scala
import cats.instances.option._ // monad instance for option
import cats.instances.either._ // monad instance for either

implicit val dbClient: DatabaseClient = new DatabaseClient {}

val options = LotteryLinksGenerator(Users.ofOption, UserValidations.ofOption)
val eithers = LotteryLinksGenerator(Users.ofEither, UserValidations.ofEither)

val userId = "1"

println(options.genLink(userId)) // Some(https://monad-lottery.com/secure?userId=1)
println(eithers.genLink(userId)) // Right(https://monad-lottery.com/secure?userId=1)
```

Remember, generics aka Parametric Polymorphism forgets about type capabilities, however type classes regain the structure of types which helps us to stay within the land of generics but also make them context aware, this leads us to writing highly abstract code that ends up being extremely flexible and configurable.

