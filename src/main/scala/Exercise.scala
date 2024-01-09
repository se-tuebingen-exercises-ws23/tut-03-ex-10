import java.io.FileWriter
import java.io.File
import scala.io.AnsiColor
import JavaArrays.kaboom

import scala.collection.immutable.HashMap

//////////////////////////////  PART 1  //////////////////////////////

class Item(val productNo: String) {
  override def toString: String = s"Item, productNo: $productNo"
}
class Buyable(override val productNo: String, val price: Int) extends Item(productNo) {
  override def toString: String = s"Buyable, productNo: $productNo, price: $price"
}
class Book(override val productNo: String, override val price: Int, val isbn: String)
    extends Buyable(productNo, price) {
  override def toString: String = s"Book, productNo: $productNo, price: $price, ISBN: $isbn"
}

// Subtyping relations:
//
// Book <: Buyable ... we read as "Book is a subtype of Buyable"
// Buyable <: Item
// Book <: Item
//
// We get the last one from "transitivity":
// If A <: B and B <: C, then A <: C.

val myMonitor = Item(productNo = "189954")
val mateDrink = Buyable(productNo = "5955530", price = 110)
val seAtGoogle = Book(productNo = "55999", price = 5095, isbn = "9781492082798")

object Assignment {
  // TODO: Why does the following work?
  val seAtGoogleAsItem: Item = seAtGoogle

  // val myMonitorAsBook: Book = myMonitor
  // TODO: Why doesn't the line above work? (Uncomment it and try it out)
  /* The error:
  [error]  val myMonitorAsBook: Book = myMonitor
  [error]                              ^^^^^^^^^
  [error]                              Found:    (myMonitor : Item)
  [error]                              Required: Book
  ...
  [error]   <== (myMonitor : Item)  <:  Book = false
   */

  // TODO: Why does this work?
  val mateDrinkAsBuyable: Buyable = mateDrink

  // Note that this is not an _artificial limitation_,
  // but the type system is actively trying to protect you from yourself!
  //
  // You can use 'asInstanceOf' as an unsafe dangerous hack.
  // Try running `Assignment.myMonitorAsBookUnsafely` in `sbt console`.
  // Why does it crash at runtime?
  //
  val myMonitorAsBookUnsafely: Book = myMonitor.asInstanceOf[Book]
  // => Warning: Don't use `asInstanceOf` in your code if not *absolutely* necessary.
}

object Subtyping1 {
  def getProductNo(item: Item): String = {
    item.productNo
  }

  // TODO: Why do all of these work?
  getProductNo(myMonitor)
  getProductNo(mateDrink)
  getProductNo(seAtGoogle)
}

object Subtyping2 {
  def getIsbn(book: Book): String = {
    book.isbn
  }

  // getIsbn(myMonitor)
  // TODO: Uncomment the line above. Why doesn't it work?
  /*
   [error]  getIsbn(myMonitor)
   [error]          ^^^^^^^^^
   [error]          Found:    Item
   [error]          Required: Book
   ...
   ... Item  <:  Book = false
   */

  // getIsbn(mateDrink)
  // TODO: Uncomment the line above. Why doesn't it work?
  /*
   [error]  getIsbn(mateDrink)
   [error]          ^^^^^^^^^
   [error]          Found:    Buyable
   [error]          Required: Book
   ...
   ... Buyable  <:  Book = false
   */

  getIsbn(seAtGoogle) // TODO: Why does this work?
}

object Subtyping3 {
  def getPrice(buyable: Buyable): Int = {
    buyable.price
  }

  // getPrice(myMonitor) // TODO: uncomment this

  // TODO: Why do these two work?
  getPrice(mateDrink)
  getPrice(seAtGoogle)
}

// Takeaway:
//
// if a value of type A is accepted,
// then a value of type A' which is a subtype of A
// is also accepted

//////////////////////////////  PART 2  //////////////////////////////

// Let's go back to our favorite example: the 'Stack' data structure.
class Stack {
  var list: List[String] = List()

  def invariant(): Unit = {
    assert(list != null)
  }

  def contains(item: String): Boolean = {
    require(item != null)
    list.contains(item)
  }

  def push(item: String): Unit = {
    require(item != null)
    list = item :: list

    invariant()
  } ensuring { res => contains(item) }
}

// Let's make a subtype which has a stronger precondition on 'push'
class UniqueStack extends Stack {
  override def push(item: String): Unit = {
    require(item != null && !contains(item)) // <- see the change?
    list = item :: list

    invariant()
  } ensuring { res => contains(item) }
}
// UniqueStack <: Stack

def testStack(stack: Stack): String = {
  // TODO: Run `testStack(Stack())` in `sbt console`
  stack.push("hello")
  stack.push("world")
  stack.push("hello")

  // TODO: Run `testStack(UniqueStack())` in `sbt console`.
  //    This should be _always_ possible, right? [because UniqueStack <: Stack]
  //
  // But what happened and why?

  return s"""Inserted 'hello' into stack,
            |so according to the postconditions of 'Stack.push',
            |'.contains(\"hello\")' must be true
            |and it is: ${stack.contains("hello")}""".stripMargin
}

// TODO:
// Make a subtype of Stack called 'PermissiveStack'
// which allows the user to call `.push(null)`.
//
// Is the precondition going to be weakened or strengthened?
// And is that safe? (Unlike 'UniqueStack')
//
// What would happen if you actually added `null` onto the stack?
// Hint: What about the invariant?

// Let's make one more implementation.
class DummyStack extends Stack {
  override def push(item: String): Unit = {
    require(item != null)
    // Doesn't actually insert anything!
    invariant()
  } // and note the missing postcondition
}

// TODO: Try running `testStack(DummyStack())` in `sbt console`.
//       What happens?

// Takeaway:
//
// Design-by-Contract and subtyping interact.
// Preconditions on subtypes must be weaker (or equal).
// Postconditions on subtypes must be stronger (or equal).

//////////////////////////////  PART 3  //////////////////////////////

// A more realistic example of subtyping:
trait Logger {
  def setPrefix(prefix: String): Unit
  def getPrefix(): String

  def log(message: String): Unit
}

class FileLogger(val filename: String) extends Logger {
  private var prefix: String = ""

  override def setPrefix(newPrefix: String): Unit = {
    prefix = newPrefix
  }

  override def getPrefix(): String = prefix

  override def log(message: String): Unit = {
    require(message != null)
    val file = File(filename)
    val fileWriter = FileWriter(file, true)
    fileWriter.write(s"$prefix $message\n")
    fileWriter.close()
  }
}
// FileLogger <: Logger

class StdoutLogger extends Logger {
  private var prefix: String = ""

  override def setPrefix(newPrefix: String): Unit = {
    prefix = newPrefix
  }

  override def getPrefix(): String = prefix

  override def log(message: String): Unit = {
    require(message != null)
    println(message)
  }
}
// StdoutLogger <: Logger

// This makes a string bold in the terminal.
// You don't need to understand how it works.
def bold(s: String): String =
  s"${AnsiColor.BOLD}$s${AnsiColor.RESET}"

// This makes a string red in the terminal.
// You don't need to understand how it works.
def red(s: String): String =
  s"${AnsiColor.RED}$s${AnsiColor.RESET}"

// TODO: Finish this variant of Logger
class PrettyStdoutLogger extends StdoutLogger {
  override def log(message: String): Unit = {
    require(message != null)

    val boldedPrefix = getPrefix() // TODO: Call 'bold' here
    val colorfulMessage = if (message.contains("!")) {
      message // TODO: Call 'red' here
    } else {
      message
    }

    println(s"$boldedPrefix $colorfulMessage")
  }
}
// TODO: Write as many subtyping relations that 'PrettyStdoutLogger' is part of.
// (You should come up with at least three of them!)

object LoggerTest {
  def testLogger(logger: Logger): Unit = {
    logger.setPrefix("[TUT]")
    logger.log("Hello, world!")
    logger.log("What a nice online tutorial.")
    logger.setPrefix("Boom!")
    logger.log(message = "This message had a prefix, wow.")
  }

  def testStdoutLogger(): Unit =
    testLogger(StdoutLogger())

  def testFileLogger(): Unit =
    testLogger(FileLogger("test.log"))

  def testPrettyStdoutLogger(): Unit =
    testLogger(PrettyStdoutLogger())

  // 1. Try `LoggerTest.testStdoutLogger()` in `sbt console`.
  // 2. Try `LoggerTest.testFileLogger()` in `sbt console`.
  // 3. Try `LoggerTest.testPrettyStdoutLogger()` in `sbt console`.
  // 4. Formulate the answer to the following in terms of subtyping:
  //        Why can we put an argument of type 'StdoutLogger'
  //        into a function which expects a parameter of type 'Logger'?
}

object ListCovariance {
  // This works since StdoutLogger, FileLogger and PrettyStdoutLogger are subtypes of Logger.
  val loggerList: List[Logger] = List(StdoutLogger(), FileLogger("test.log"), PrettyStdoutLogger())

  val fileLogger1 = FileLogger("file1.log")
  val fileLogger2 = FileLogger("file2.log")
  val fileLogger3 = FileLogger("file3.log")
  var fileLoggerList: List[FileLogger] = List(fileLogger1, fileLogger2, fileLogger3)

  // fileLoggerList = fileLoggerList.appended(StdoutLogger())
  // TODO: Uncomment this ^. Why doesn't it work?

  var newLoggerList: List[Logger] = fileLoggerList
  // TODO: Why does the above work? This is no longer _only_ because of FileLogger <: Logger!

  newLoggerList = newLoggerList.appended(StdoutLogger())
  // TODO: Notice that unlike above, this works. Why?

  // val newFileLoggerList: List[FileLogger] = loggerList
  // TODO: Uncomment the above ^. Why doesn't this work?
}

object ArrayInvariance {
  // This works, since StdoutLogger, FileLogger and PrettyStdoutLogger are subtypes of Logger.
  val loggerArray: Array[Logger] = Array(StdoutLogger(), FileLogger("test.log"), PrettyStdoutLogger())

  val fileLogger1 = FileLogger("file1.log")
  val fileLogger2 = FileLogger("file2.log")
  val fileLogger3 = FileLogger("file3.log")
  val fileLoggerArray: Array[FileLogger] = Array(fileLogger1, fileLogger2, fileLogger3)

  // val newLoggerArray: Array[Logger] = fileLoggerArray
  // TODO: Uncomment the above ^. Why does it throw an error unlike the previous example?
}

// TODO: Go to the `JavaArrays.java` file in `src/main/java` for a while.
//       Then after you're done, come back here :)

//////////////////////////////  PART 4 (skip if short on time) //////////////////////////////

// Let's take a very simple interface, a _Store_,
// which has a single method that allows us to get a value from some unspecified container
// (REST API / database / Redis / command-line arguments / config file / GUI / environment variables / data structures ...)
trait Store[+T] { // Notice this +T. This states that the type `Store` is covariant.
  // TODO: Change it to -T. What happens? Make sense of the error message!

  def get(key: String): Option[T]
}

class HashMapStore[+T](val hashmap: HashMap[String, T]) extends Store[T] {
  def get(key: String): Option[T] = {
    hashmap.get(key)
  }
}

// Observe: Store is covariant.
val bookStore: Store[Book] = HashMapStore(HashMap(("seBook", seAtGoogle)))
val itemStore: Store[Item] = bookStore
// TODO: Remove the + from 'trait Store' and see that this breaks.

// Usually, producers of T (things that return a value of type T), are _covariant_.

class Serializer[-T] { // Notice this -T. This states that a Writer is _contravariant_.
  // TODO: Change it to -T. What happens? Make sense of the error message!

  def serialize(what: T): String = {
    what.toString()
  }
}
//   Contravariance: (a type `Consumer[T]` is contravariant in `T` <=>) 
//       If T <: U, then Consumer[U] <: Consumer[T].
//
//   This is a very confusing concept, but worth thinking about. :)

def contravariant() = {
  val itemSerializer = Serializer[Item]()
  println(itemSerializer.serialize(seAtGoogle))
  println(itemSerializer.serialize(myMonitor))

  val bookSerializer: Serializer[Book] = itemSerializer
  // TODO: Analyze why this works. This is the exact opposite of what happened with the Store!

  // val anotherItemSerializer: Serializer[Item] = bookSerializer
  // TODO: Uncomment, analyze why this doesn't work.
}

// Usually, consumers of T (things that accept a value of type T), are _contravariant_.

//////////////////////////////  PART 5  //////////////////////////////

// Why would we want this behaviour?
// Let's simplify:

def prettifyItem(item: Item): String = {
  s"""Item: ${item.productNo}
     |There's not much info, it's not for sale.
     |""".stripMargin
}

def prettifyBook: Book => String = prettifyItem
// TODO: Why does this work?
// The type of prettifyBook is Book => String
// and we assign to it a value type of Item => String.
//
// As we've seen above, this means that:
// (Item => String) <: (Book => String)
// ...
// Function types can also be subtyped!
// ...
// Remember contravariance?
// Functions are contravariant in their arguments!
// If Book <: Item, then (Item => String) <: (Book => String)

def prettyBook(book: Book): String =
  s"""Book: ${book.isbn}
     |selling for ${book.price}
     |under a product number ${book.productNo}.
     |
     |Hope you enjoy!
     |""".stripMargin

// def prettyItem: Item => String = prettyBook
// TODO: Uncomment, see that it doesn't work.
//
// (Book => String) <: (Item => String)
//     only if
// Item <: Book, because functions are contravariant in their argument.
// But Item is not a subtype of Book!
//
// This should also make sense on a semantic level.
// 'prettyBook' works with books and their fields,
// but 'prettyItem' would feed it items which don't have the ISBN and a price!

// However, functions are also covariant! ... in their result type.
def makeAStandardBook(isbn: String): Book = Book(productNo = isbn, price = 1000, isbn = isbn)

def makeAStandardItem: String => Item = makeAStandardBook
// This works! You should be able to reason about it.
//
// We can see (since the assignment works), that:
// (String => Book) <: (String => Item)
// We also know that Book <: Item.
// You should see that functions are covariant in their argument.

// Takeaway:
//
// As the designer of an interface, you should be able to tell whether
// it's covariant, contravariant or invariant.
// As a rule of thumb:
// - covariant types are producers,
// - contravariant types are consumers
// - invariant types are producers _and_ consumers (pipelines).
//
// Recommended reading: https://docs.scala-lang.org/scala3/book/types-variance.html

@main
def main() = {
  println("Please, run 'sbt console' instead.")
}
