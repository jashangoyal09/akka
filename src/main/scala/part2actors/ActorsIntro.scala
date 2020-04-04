package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorsIntro extends App{

  //part1 - actor systems
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  /**
   * Actors are uniquely identified
   * Messages are asynchronous
   * Each actor may respond differently
   * Actors are (really) encapsulated
   */

  // part2 - create actors
  // word count actors
  var totalWords = 0
  class WordCountActors extends Actor {

    //behaviour
    override def receive: PartialFunction[Any,Unit] = {
      case message:String =>
        println(s"Received message $message")
        totalWords += message.split(" ").length
        println("CCCCCCCCCCcc"+totalWords)
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  //part 3 - instantiate our actor
  val wordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActors],"wordCounter")
  val anotherWordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActors],"antoherWordCounter")

  println("AAAAAAAAAAAA"+totalWords)
  // part 4 - communicate!
  wordCounter ! "Learning akka and this is the message"
//  anotherWordCounter ! "From the another actor"
  println("BBBBBBBBBBBBB"+totalWords)

  class Person(name:String) extends Actor{
    override def receive: Receive = {
      case "Hi" => println(s"Hi this message from the $name")
      case _=>
    }
  }
  object Person {
    def props(name: String) = Props(new Person(name))
  }

  val person = actorSystem.actorOf(Person.props("jashan"))
  person ! "Hi"
}