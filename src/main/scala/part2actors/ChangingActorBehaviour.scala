package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChangingActorBehaviour.Counter.{Decrement, Increment, Print}
import part2actors.ChangingActorBehaviour.Mom.{Food, MomStart}

object ChangingActorBehaviour extends App {

  object FussyKid {
    case object KidAccept
    case object KidReject
    val HAPPY = "happy"
    val SAD = "sad"
  }
  class FussyKid extends Actor {
    import FussyKid._
    import Mom._
    // internal state of the kid
    var state = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) =>
        if(state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor {
    import FussyKid._
    import Mom._

    override def receive: Receive = happyReceive

    /**
     * context.become(_, true)
     *    True means discard the old message handlers or fully replace the old message handler with a new message
     * context.become(_, false)
     *    False does the following instead of replacing or discarding the old message handler we will simply stack
     *    the new message handler on to a stack of message handlers
     */
    def happyReceive:Receive = {
      case Food(VEGETABLE) => context.become(sadReceive,false)//change my receive handler to sadReceive
      case Food(CHOCOLATE) =>
      case Ask(_) => sender() ! KidAccept
    }
    def sadReceive:Receive = {
      case Food(VEGETABLE) => //stay sad
      case Food(CHOCOLATE) => context.become(happyReceive,false)//change my recieve handle to happyReceive
      case Ask(_) => sender()!KidReject
    }
  }
  object Mom{
    case class MomStart(kidRef:ActorRef)
    case class Food(food:String)
    case class Ask(message:String) //do want to play?
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }
  class Mom extends Actor {
    import Mom._
    import FussyKid._
    override def receive: Receive = {
      case MomStart(kidRef) =>
        // test our interaction
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("do you want to play?")
      case KidAccept => println("Yay,my kid is happy")
      case KidReject => println("My kid is sad, but as he's healthy!")
    }
  }

  val system = ActorSystem("changingActorBehaviour")
  val fussyKid = system.actorOf(Props[FussyKid])
  val mom = system.actorOf(Props[Mom])
  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid])

//  mom ! MomStart(fussyKid)
  mom ! MomStart(statelessFussyKid)

  /*
    mom receive MomStart
      kid receives Food(veg) -> kid will change the handler to sadReceive
      kid receives Ask(play?) -> kid replies with the sadReceive handler =>
    mom receives kidReject
   */

  /*
      Food(veg) -> message handler turns to sadReceive
      Food(chocolate) -> become happyReceive

      with boolean values in context.become
      Food(veg) -> stack.push(sadReceive)
      Food(chocolate) -> stack.puch(happyReceive)

      Stack:
      1. happyReceive
      2. sadReceive
      3. happyReceive

   */

  /*
      new behavior
      Food(veg)
      Food(veg)
      Food(choco)
      Food(choco)

      Stack :
      1. happyReceive

   */

  /**
   * Immutable counter
   */
  object Counter{
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor{
    override def receive: Receive = countReceive(0)

    def countReceive(currentCount:Int):Receive={
      case Increment =>
        println(s"[$currentCount] Incrementing")
        context.become(countReceive(currentCount+1))
      case Decrement =>
        println(s"[$currentCount] Decrementing")
        context.become(countReceive(currentCount-1))
      case Print => println(s"This is the final count value :: $currentCount")
    }
  }

  val actor = system.actorOf(Props[Counter],"counterActor")

  (1 to 5).foreach(_=>actor!Increment)
  (1 to 5).foreach(_=>actor!Decrement)
  actor!Print

}
