package actorSystem

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {

  val actorSystem = ActorSystem("firstActorSystem")

  class WordCounterActor extends Actor {
    var totalWords = 0

    def receive : Receive = {
      case message: String => {
        totalWords += message.split(" ").length
        println(totalWords)
      }
      case msg => println("Some stuff")
    }
  }


  val wordCounter = actorSystem.actorOf(Props[WordCounterActor], "wordCounter")
  val wordCounter2 = actorSystem.actorOf(Props[WordCounterActor], "wordCounter2")
  wordCounter ! "learning akka"
  wordCounter2 ! "Yet another message"

  class Person(val name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println ("Hi my name is Bob")
      case _ =>
    }
  }

  val person = actorSystem.actorOf(Props(new Person("Bob")),"Bob")
  person ! "hi"

}
