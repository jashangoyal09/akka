package part2actors

import akka.actor.Status.Status
import akka.actor.{Actor, ActorRef, ActorSystem, Props, Status}
import part2actors.ActorCapabilities.Counter.{Decrement, Increment, Print}
import part2actors.ActorCapabilities.Person.LiveTheLife

object ActorCapabilities extends App {

  val actorSystem = ActorSystem("actorCapabilitiesDemo")

  class SimpleActor extends Actor {
    override def receive: Receive = {
      /**
       * context.sender is the who last send message
       * sender is the who last send message
       */
      case "Hi!" => sender() ! "Hello, there!" //replying to a message Here sender is alice due to self nature //case 1
      case message:String => println(s"[${context.self.path}] I have received :: $message") //case 2
      case number:Int => println(s"[$self] I have received a number :: $number") //case 3
      case SpecialMessage(content) => println(s"I have received special message :: $content") //case 4
      case SendMessageToYourself(content) => self ! content //case 5
      case SayHiTo(ref) => ref ! "Hi!" // Here ref is BOB Actor. alice is being passed as the sender //case 6
      case WirelessPhoneMessage(content, ref) => ref forward(content+"s") //I keep the original sender of the WPM //case 7
    }
  }

  val simpleActor = actorSystem.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"

  /**
   * 1.message can be of any type
   * message must be IMMUTABLE
   * message must be SERIALIZABLE
   * In practice use case classes or case objects
   */
  simpleActor ! 24

  case class SpecialMessage(content:String)
  simpleActor ! SpecialMessage("This is special message")

  /**
   * 2. Actors have information about their `context` and about themselves
   * context.self == this [In OOPS]
   */

  case class SendMessageToYourself(content:String)
  simpleActor ! SendMessageToYourself("I am an actor and I am proud of it ;)")
  // case 5 -> case 2

  /**
   * 3.Actors can REPLY to messages
   */
  val alice = actorSystem.actorOf(Props[SimpleActor],"alice")
  val bob = actorSystem.actorOf(Props[SimpleActor],"bob")

  case class SayHiTo(ref:ActorRef)
  alice ! SayHiTo(bob)

  // 4 - dead letters
  alice ! "Hi!" //send msg to no actor

  /**
   * 5 - forward message [forward the message chain by keeping original sender]
   * D -> A -> B
   * forwading = sending the message with original sender
   */
  case class WirelessPhoneMessage(conten:String,ref: ActorRef)
  alice ! WirelessPhoneMessage("Hi",bob) //no sender

  /**
   * Exercise
   *
   * 1. a Counter actor
   *    -Increment
   *    -Decrement
   *    -Print
   *
   * 2. a Bank account as an actor
   *    - Deposit an amount
   *    - Withdraw an amount
   *    - Statement
   *
   *    - Reply Success/Failure
   *
   *    interact with some other kind of actor
   */

  //Exercise 1

  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor {
    import Counter._
    var counter = 0
    def receive : Receive = {
      case Increment => counter+=1
      case Decrement => counter-=1
      case Print => println(s"[counter] My current count is $counter")
    }
  }

  val counter = actorSystem.actorOf(Props[Counter],"counterActor")

  (1 to 5).foreach(_=>counter!Increment)
  (1 to 3).foreach(_=>counter!Decrement)
  counter ! Print

  //Exercise 2
  object BankAccount{
    case class Deposit(amount:Int)
    case class Withdraw(amount:Int)
    case object Statement
    case class TransactionSuccess(message:String)
    case class TransactionFailure(reason:String)
  }
  class BankAccount extends Actor {
    import BankAccount._
    var balance = 0
    override def receive: Receive = {
      case Deposit(amount) =>
        if(amount < 0) sender() ! TransactionFailure("Invalid deposit amount")
        else{
          balance +=amount
          sender() ! TransactionSuccess(s"successfully deposited $amount")
        }
      case Withdraw(amount) =>
        if(amount < 0) sender() ! TransactionFailure("Invalid withdraw amount")
        else if(amount>amount) sender() ! TransactionFailure("Insufficient balance")
        else {
          balance -= amount
          sender() ! TransactionSuccess(s"successfully withdraw $amount")
        }
      case Statement => sender() ! s"Your balance is $balance"
    }
  }

  object Person{
    case class LiveTheLife(account:ActorRef)
  }

  class Person extends Actor{
    import Person._
    import BankAccount._

    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(1000)
        account ! Withdraw(500)
        account ! Statement

      case message => println(message.toString)
    }
  }

  val account = actorSystem.actorOf(Props[BankAccount],"bankAccount")
  val person = actorSystem.actorOf(Props[Person],"billionaire")

  person ! LiveTheLife(account)
}
