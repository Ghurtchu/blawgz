# Demystifying Variance magic in Scala

Invariant? Covariant? Contravariant?… What the `F[_]`?!

Scala programming language has an extremely powerful feature called “Variance”. Variance, if used properly can guarantee us to benefit from the increased ability of the compiler to infer types at compile time.

It saves us from adding type annotations to every single expression manually which in turn helps us to avoid unnecessary verbosity.

Everything starts with `F[_]` which is essentially some kind of wrapper (Type Constructor) over underlying type, examples can be:

`Option[Int]`, `Future[String]`, `List[Action]`, `Vector[Transaction]`, `Try[UserProfile]`. Here — `Option`, `Future`, `List`, `Vector`, and `Try` are type constructors, whereas `Int`, `String`, `Action`, `Transaction` and `UserProfile` are underlying, normal types.

Variance tries to reflect the subtyping relationship of the underlying types and apply it to the wrapper type `F[_]`-s to form some kind of relationship between them as well.

Variance is characterised by three representations — Invariance, Covariance and Contravariance.

What’s the motivation behind all these? Well, in my opinion not every line of code needs to be typed to the max, instead we should let the compiler work for us, and… Voila! Variance helps us exactly in that regard.

So, the big questions are:
- What is the relationship between `F[A]` and `F[B]` if `A` is a subtype of `B`?
- Will `F[A]` also be a subtype of `F[B]`? or vice versa?
- Is it possible, that there is no relationship between `F[A]` and `F[B]`?
- What can be done from our side to achieve one of these outcomes?
- 
We have a few tools in our toolbox to make `F[_]` Invariant, Covariant or Contravariant.

Let’s start with the simplest one — Invariance.

Invariant data structures do not mirror the subtyping relationship of underlying types to the `F[_]`-s which wrap them. So, invariant data structures are called invariant — because for them the relationship between underlying types do not matter.

if `Text` is a subtype of `Message` then it does not mean that `Future[Text]` will be a subtype of `Future[Message]`, it turns out that `Future[Text]` and `Future[Message]` will have no explicit subtyping relationship whatsoever.

Let’s see an example:

We have a `Message` (algebraic data type) which has two finite representations: `Text` and `Binary`. We want to put them into Mailbox which may be processed later.

```scala
sealed trait Message // ADT - sum type

object Message { 
  final case class Text(value: String)        extends Message 
  final case class Binary(value: Array[Char]) extends Message 
}

import Message._

                      // A means invariant type here
final case class Mailbox[A](a: A)
/* e.g if A is subtype of B, then Mailbox[A] won't be subtype of Mailbox[B]
 * So, invariance does not reflect the original relationship between A and B
 */

val txt = Mailbox(Text("Hello dear")) // inferred as Mailbox[Text]
val bnr = Mailbox(Binary(Array('!', 'c', '#'))) // inferred as Mailbox[Binary]

def process(mailbox: Mailbox[Message]): Unit = () // implementation not important

process(txt) // does not compile, Mailbox[Text] is not a subtype of Mailbox[Message]
process(bnr) // does not compile, Mailbox[Binary] is not a subtype of Mailbox[Message]
```

Invariant data structures do not care about the relationship of underlying types. This can be done on purpose (by-design) or by accident.

You can read more about why Arrays in Scala are invariant: https://stackoverflow.com/questions/6684493/why-are-arrays-invariant-but-lists-covariant

We can solve this problem in two ways:

1) Manually typing everything to the max
2) Using declaration-site covariance `[+A]` annotation

If we follow the first approach (which we wanted to avoid in the first place) then we could manually add the types to value definitions and then everything would compile:

```scala
val txt: Mailbox[Message] = Mailbox(Text("Hello")) // manually typed by us
val bnr: Mailbox[Message] = Mailbox(Binary(Array('a', 'b', '#'))) // manually typed by us

process(txt) // compiles
process(bnr) // compiles

/* now all of a sudden it compiles
 * the disadvantage: we need to add types ourselves explicitly to make it work
 * typing everything to the max can lead to verbosity...
 */
```

The better approach would be to use the declaration-site covariance annotation `[+A]` which would solve this problem for us:

```scala
                      // +A means covariant type here
final case class Mailbox[+A](message: A)
/*e.g if A is subtype of B, then Mailbox[A] will be a subtype of Mailbox[B]
 * So, covariance reflects the original relationship between A and B 
 */

val txt = Mailbox(Text("Hello dear")) // inferred as Mailbox[Text]
val bnr = Mailbox(Binary(Array('!', 'c', '#'))) // inferred as Mailbox[Binary]

def process(mailbox: Mailbox[Message]): Unit = ??? // implementation not important

process(txt) // compiles, because Mailbox[Text] is a subtype of Mailbox[Message]
process(bnr) // compiles, because Mailbox[Binary] is a subtype of Mailbox[Message]
```

Ok, cool! Let’s unleash the power of `Covariance` further. What if we want to add new subtype `Json` to `Text` and a new method `setNewMsg` to `Mailbox` which would set new message to it and return the new instance? Would the type inference remain working properly?
The answer is — Yes.

Let’s dive into an example:

```scala
sealed trait Message

object Message {
  
  sealed trait Text extends Message {
    def value: String
  }

  final case class Plain(value: String)       extends Text
  final case class Json(value: String)        extends Text
  final case class Binary(value: Array[Char]) extends Message
}

import Message._

final case class Mailbox[+A](message: A) {
              // A1 >: A means that A1 must be at least A or its' supertype here to make "type widening" work
  def withNewMsg[A1 >: A](newMsg: A1): Mailbox[A1] =
    Mailbox(newMsg)
}

val jsn = Mailbox(Json("""{"myFavLang":"Scala"}""")) // inferred as Mailbox[Json]
val txt = jsn.withNewMsg(new Text {
    override def value: String = "Anon!"
  }) // inferred as Mailbox[Text]

def process(mailbox: Mailbox[Message]): Unit = ()

process(jsn) // compiles, since Mailbox[Json] <: Mailbox[Message]
process(txt) // compiles, since Mailbox[Text] <: Mailbox[Message]
```

So, we saw that `Mailbox` is a covariant data structure.

More canonical examples of Covariant data structures can be: `List`, `Vector`, `Option`, `Try`, `Either`, `Future` from the Scala standard library.

Covariant data structures behave as producers — they produce values, they reflect the subtyping relationship between underlying types and most importantly — they have a capacity to widen the type based on the nearest parent of underlying types.

Let’s forget about `Mailbox` and see an example with `List`:

```scala
sealed trait Message

object Message {
  
  sealed trait Text extends Message {
    def value: String
  }

  final case class Plain(value: String)       extends Text
  final case class Json(value: String)        extends Text
  final case class Binary(value: Array[Char]) extends Message
}

import Message._

val jsn1 = Json("""{"myFavLang":"Scala"}""")
val jsn2 = Json("""{"myFavBand":"Necrophagist"}""")
val jsons = List(jsn1, jsn2) // inferred type: List[Json]

val pln = Plain("Tagless Final rocks!")
val texts = pln :: jsons // inferred widened type: List[Text]
// List[Text] because Text is the nearest common supertype for Json and Plain

val bnr = Binary(Array('a', '#', '!', 'z'))

def process(list: List[Message]): Unit = ??? // implementation not important

val msgs = bnr :: texts // inferred widened type: List[Message] since Message is the nearest parent
// List[Message] because Message is the nearest common supertype for Binary and Text

process(jsons) // compiles, since List[Json] <: List[Message]
process(texts) // compiles, since List[Text] <: List[Message] 
process(msgs) // compiles, since List[Message] <: List[Message]
```

See? we haven’t added any types manually, however the compiler found out by itself when to widen it.

In short, imagine `pln :: jsons` implemented as the following:

```scala
sealed trait List[+A]

object List {
  final case class Cons[+A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing] // Nothing is subtype of any type
}

// this enables the syntax like 1 append List(2, 3) yielding List(1, 2, 3)
implicit class OpsWithList[A](self: A) {
  def append[A1 >: A](list: List[A1]): List[A1] = list match {
    case Cons(_, _) => Cons(self, list)
    case Nil        => Cons(self, Nil) 
  }
}
```

What the heck is `Contravariance` though? Well… `Contravariance` is an opposite of `Covariance`, so it inverts the subtyping relationship of underlying types for `F[_]`-s, meaning: if `A` is a subtype of `B` then `F[B]` is a subtype of `F[A]`. It may be weird, however it really makes sense if you think about the usage.

Let’s dive into an example, let’s say we have a `Formattable` type class which formats anything to `String`:

```scala
sealed trait Message

object Message {

  sealed trait Text extends Message {
    def value: String
  }

  final case class Plain(value: String)       extends Text
  final case class Json(value: String)        extends Text
  final case class Binary(value: Array[Char]) extends Message
}

import Message._

               // -A here means that this trait is Contravariant
trait Formattable[-A] {
  def format(input: A): String
} // meaning that if A is a subtype of B then F[A] is a supertype of F[B]


object Formattable { 

  // handles only Text and its' subtypes (Json)
  implicit val txtFormattable: Formattable[Text] = {
     case Text(v) => s"Text: $v"
     case Json(v) => s"Json: $v"
  }

  // handles only Binary
  implicit val binFormattable: Formattable[Binary] = { 
     case Binary(v) => s"Binary: $v"
  }

  // Handles any subtype of Message (Binary, Text and Json)
  implicit val msgFormattable: Formattable[Message] = {
     case Text(v)   => s"Text: $v"
     case Json(v)   => s"Json: $v"
     case Binary(v) => s"Binary: $v"
  }

  /* let's rewrite the analogy
   * if A is subtype of B === if Json is a subtype of Text
   * then 
   * F[B] is a subtype of F[A] === Formattable[Text] is a subtype of Formattable[Json]
   * which means, that we can pass Formattable[Text] where Formattable[Json] is expected
   * and it would compile, since Formattable[Text] <: Formattable[Json]
   */
}
```

Contravariant data structures are generally considered to be consumers, they consume types and do something with them, they invert the subtyping relationship of underlying types.

Generally, `subtype` means that it has all properties of the `supertyp` eand also unique properties of itself — which means, that `Formattable[Text]` can do everything that `Formattable[Json]` does and even more — format `Text` messages which `Formattable[Json]` is unable to do.

Let’s prove our reasoning with the following example:

```scala
// client
def format[A](input: A)(implicit formattable: Formattable[A]): Unit = 
  println(formattable.format(input))

val jsn = Json("""{"myFavLang":"Scala"}""")
/* for Json, normally we'd expect to use Formattable[Json]
 * however, we know that Formattable[Text] or Formattable[Message] could also work
 * since they are subtypes of Formattable[Json]
 */

format(jsn)(Formattable.msgFormattable) // compiles, since Formattable[Message] <: Formattable[Json]
format(jsn)(Formattable.txtFormattable) // compiles, since Formattable[Text] <: Formattable[Json]
```

Cool, let’s see something from Scala standard library — `Function1` is a contravariant data structure as well which looks like `Function1[-A, +B]`.

```scala
sealed trait Message

object Message {

  sealed trait Text extends Message {
    def value: String
  }

  final case class Plain(value: String)       extends Text
  final case class Json(value: String)        extends Text
  final case class Binary(value: Array[Char]) extends Message
}

import Message._

         // A is any type which is a subtype of Message
def process[A <: Message](message: A, f: A => Unit): Unit = f(message)

// type is Function1[Plain, Unit] or just Plain => Unit
val plainFunc: Plain => Unit = {
  case Plain(value) => println(s"Plain: $value")
}

// type is Function1[Text, Unit] or just Text => Unit
val txtFunc: Text => Unit = {
  case Json(value)  => println(s"Json: $value")
  case Plain(value) => println(s"Plain: $value")
}

// type is Function1[Text, Unit] or just Text => Unit
val msgFunc: Message => Unit = {
  case Plain(value)  => println(s"Plain: $value")
  case Json(value)   => println(s"Json: $value")
  case Binary(value) => println(s"Binary: $value")
}

val pln = Plain("Scala rocks!")

process(pln, plainFunc) // compiles
process(pln, txtFunc) // compiles, because Text => Unit <: Plain => Unit, since Plain <: Text
process(pln, msgFunc) // compiles, because Message => Unit <: Plain => Unit, since Plain <: Message
```

So, in essence Variance is a compiler mechanism applied to `F[_]` type constructors which can turn the type inference problem into a piece of cake and make more code compile.

Cheat sheet:
`Invariance`— stays fixed no matter what the subtyping relationship of underlying types may be, meaning that relationship between `F[_]`-s are not apparent for the compiler.

`Covariance` — mirrors the subtyping relationship for `F[_]`-s, meaning that the relationship between wrapped types (`F[_]`) co-varies with the relationship of the underlying types (`A, B`)

more generally, if `A` <: `B` then `F[A]` <: `F[B]`

specifically, if `Json` <: `Text` then `Option[Json]` <: `Option[Text]`

Contravariance — inverts the subtyping relationship for `F[_]`-s, meaning that the relationship between wrapped types (`F[_]`) contra-varies with the relationship of the underlying types (`A`, `B`)

more generally, if `A` <: `B` then `F[A]` >: `F[B]`

specifically, if `Json` <: `Text` then `Formattable[Json]` >: `Formattable[Text]`.

But then again… what the `F[_]`?!
