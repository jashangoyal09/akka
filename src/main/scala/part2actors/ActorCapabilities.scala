package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

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

}
