# Underrated Scala features and a few hidden gems in the standard library

Back to biznisss! 😌

#### 🤓 scala.util.Using 🤓
A data structure which manages resources automatically. It lets us focus on the task at hand by giving us a handle on the acquired resource which is then automatically released in the end so that we avoid resource leaks.

Let’s say we want to read a file, count words and print it. A typical approach would be to wrap everything in the `try/catch/finally` but we can at least use `scala.util.Try` which makes it less verbose and nicer:

```scala
import scala.io.Source
import scala.util.Try

Try { 
  val src = Source.fromFile("file1.txt")
 
  println {
    src.getLines
      .foldLeft(Map.empty[String, Int]) { (map, word) =>
        map.updated(word, map.getOrElse(word, 0) + 1)
      }
  }
  
  src.close() // we must not forget to close src
}
```

Now let’s try the same with `scala.util.Using`:

```scala
import scala.io.Source
import scala.util.Using

Using(Source.fromFile("file1.txt")) { src => 
  println {
    src.getLines
      .foldLeft(Map.empty[String, Int]) { (map, word) =>
        map.updated(word, map.getOrElse(word, 0) + 1)
      }
  }
} // src.close() is called once the operation is done
```

To achieve this `Using` is “using” `Releasable` type class and for `Closeable` types such as `BufferedReader` there is an `implicit object` defined — `AutoCloseableIsReleasable` which makes it possible to call something like `resource.close()` behind the scenes.

Though for the same task one might use `java.io.Files` which has a really nice API:

```scala
import java.nio.file.{Files, Path}

val lines = Files.readAllLines(Path.of("file.txt"))
```

But there are times when you want to inspect each line and do some operations for which you’ll need to write some lower level code.

❓ ❓ ❓
A special method defined in `scala.Predef` that returns `Nothing` by throwing `NotImplementedError`. In other languages you would have to manually write something like `return null;` at the end of method body to make it work.

Why is this useful? It can be used as last expression in any method which helps us to quickly build prototypes and relationships among them which actually compiles since `Nothing` is a subtype of any type in Scala, let’s see an example:

```scala
def f1: Long => String = ??? // idk implementation
def f2: String => Boolean = ??? // idk implementation
def f3: Boolean => Char = ??? // idk implementation
def f4: Char => (Int, Int) = ??? // idk implementation
def f5: ((Int, Int)) => AnyRef = ??? // idk implementation
def f6: AnyRef => Unit = ??? // idk implementation

// this compiles
def f7: Long => Unit = 
  f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6
```

#### ⚡️ PartialFunction[-A, +B] ⚡️
PartialFunction is a special function which is defined for only a certain set of inputs in any arbitrary domain.

It let’s us to do something cool like this:

```scala
val map = Map[String, Either[Int, Option[String]]] (
  "1" -> Right(Some("world")),
  "two" -> Right(Some("bye")),
  "three" -> Left(1),
  "4" -> Left(2),
  "5" -> Right(None),
  "6" -> Right(Some("hello"))
)

val strings = map.collect {
  case (k, Right(Some(v))) if Try(k.toInt).isSuccess => v
}

println(strings) // List(hello, world)
```

#### ⛓ scala.util.chaining ⛓
We can add `pipe` and `tap` extension methods to any object whatsoever.

`pipe` takes the function `A => B` and returns `B` where `A` is an object you call `pipe` on:

```scala
import scala.util.chaining._

val nums: List[Int] = "1 two 3 four 5" // String
  .pipe(_.split(" ")) // Array("1", "two", "3", "four", "5") 
  .pipe(_.flatMap(n => Try(n.toInt).toOption)) // Array(1, 3, 5)
  .pipe(_.toList) // List(1, 3, 5)
```

`tap` is more interesting and useful, it let’s us call a function on any object and return itself while doing something else on the side:

```scala
import scala.util.chaining._

val nums: List[Int] = "1 two 3 four 5"
  .tap(str => println(s"trying to parse $str to Array"))
  .split(" ") // Array("1", "two", "3", "four", "5")
  .tap(arr => println(s"trying to parse each element of $arr to Int"))
  .flatMap(n => Try(n.toInt).toOption) // Array(1, 3, 5)
  .tap(arr => println(s"$arr parsed successfully"))
  .toList // List(1, 3, 5)
  .tap(println)
```

#### 👉 Function types 👈
If you design traits with apply methods like this:
```scala
trait Transform[A, B] { 
  def apply(input: A): B
}
```

You can write the same more concisely:
```scala
trait Transform[A, B] extends (A => B)
```

And it will be the same because `apply` will be inherited from `Function1` trait, so it basically becomes a named function.

#### 👹 Implicits & term inference 👹
implicit conversions for wrapped types like `F[A]` with the help of `implicit def` and adding syntax to objects via `implicit class`:

```scala
trait Equals[A] {
  def same(left: A, right: A): Boolean
}

implicit class EqualsSyntax[A: Equals](self: A) {
  def =?(that: A): Boolean = Equals[A].same(self, that)
}
object Equals {

  def apply[A: Equals]: Equals[A] = implicitly

  implicit def optionEquals[A: Equals]: Equals[Option[A]] = {
    case (Some(left), Some(right)) => left =? right
    case (None, None) => true
    case _ => false
  }

  implicit def listEquals[A: Equals]: Equals[List[A]] = {
    case (lh :: lt, rh :: rt) if lh =? rh && lt.size == rt.size =>
      lt.zip(rt).forall { case (l, h) => l =? h }
    case (Nil, Nil) => true
    case _ => false
  }
}
implicit val intEquals: Equals[Int] = _ == _
println(List(1, 2, 3) =? List(1, 2, 3)) // true
println(Option(1) =? None) // false
println(List(Option(1), Option(2)) =? List(Option(1), Option(2))) // true
```

#### 🌿 Local functions 🌿
Local functions are intended for local calculations only. There are times when we need to define functions in local scope because in outer scope they might make no sense or they might not be intended to belong there, e.g:

```scala
def genMap(size: Int): Map[String, List[Int]] = {
  
  def map[A](n: Int, f: Int => A): List[A] =
    (1 to n).map(f).toList

  def randNumList(n: Int): List[Int] =
    map(n, _ => (math.random() * 100).toInt)

  def genLists(n: Int): List[List[Int]] = 
    map(n, _ => randNumList(n))

  TreeMap.from {
    genLists(size)
      .map(_.filter(_ % 2 == 0))
      .groupBy(_.sum)
      .map { case (key, value) => key.toString -> value.flatten.sorted }
  }
}
```

#### 💪 lazy vals & by name parameters💪
Imagine that you have a computation which needs to be evaluated in some function inside, otherwise it makes no sense to proceed. Let’s see how we’d do that without lazy val and by name parameters first:

```scala
def process[A](block: () => A): Unit = {
  println("processing...")
  val result = block() // evaluated inside the function
  println(result)
}

// unevaluated block here, just defined as () => Int
val block = () => {
  println("this is a block!")
  // ..

  scala.util.Random.nextInt()
}
 
     // unevaluated here, just passed
process(block) 
```

Now let’s make it less verbose and more “Scalaesque” with the help of `lazy val` and `by name` parameter:

```scala
              // by name param
def process[A](block: => A): Unit = {
  println("processing...")
  val result = block // evaluated inside the function
  println(result)
}

// unevaluated here, just defined
lazy val block = {
  println("this is a block!")
  // ..
  
  scala.util.Random.nextInt()
}

        // unevaluated here, just passed
process(block) 
```

Most well know examples where by name params are useful are things such as:

```scala 
IO { // something horrible here :D }
Try { // something horrible here :D }
```
and so on 😄.

#### 🐸 Type aliases 🐸
Let’s say you’re designing an API and you haven’t yet decided which one to use as a return type, `Option` or `Either`:

```scala
final case class Player(name: String, age: Int)

                       // make it higher kinded
trait PlayerValidations[F[_]] { 
  def validate(p: Player): F[Player]
}

sealed trait PlayerValidationFailure

object PlayerValidationFailure { 
  case object EmptyName extends PlayerValidationFailure
  case object NonAdult  extends PlayerValidationFailure
}

// create type alias so that it conforms to F[_]
type PlayerValidationFailureOr[A] = Either[PlayerValidationFailure, A]

// parameterized by Option
val playerValidations1: PlayerValidations[Option] =
  p => Option.when(p.name.nonEmpty && p.age >= 18)(p)

// parameterized by PlayerValidationFailureOr
val playerValidations2: PlayerValidations[PlayerValidationFailureOr] = 
  p => for {
    _ <- Either.cond(p.name.nonEmpty, p, EmptyName)
    _ <- Either.cond(p.age >= 18, p, NonAdult)
  } yield p
```

Honorable mentions:

- `scala.util.Properties` / `sys` utilities which help you access OS & JVM level information easily
- The whole `scala.collections` 😂
- structural types
- `ClassTag[A]`
- Kotlin developers who say that Scala looks a lot like Kotlin 💌
- Java developers who are excited about pattern matching, sealed classes and records 🍭
- `Nothing`
