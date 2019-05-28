package exercises

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActors extends App {

  object WordCounterMaster {

    case class Initialize(nChildren: Int)

    case class WordCountTask(task: String)

    case class WordCountReply(count: Int)

  }

  class WordCounterMaster extends Actor {

    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(number) => context.become(initialized(
        (0 until number).map(index => context.actorOf(Props[WordCounterWorker], "counter_" + index)),
        0
      ))
    }

    def initialized(refs: Seq[ActorRef], current: Int): Receive = {
      case t: WordCountTask => refs(current) forward  t; context.become(initialized(refs, (current + 1) % refs.length))
    }
  }


  class WordCounterWorker extends Actor {

    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(content) => {
        println(s"${self.path.name} is counting ")
        sender() ! WordCountReply(content.split(" ").length)
      }
    }
  }

  import WordCounterMaster._

  class Requestor extends Actor {
    override def receive: Receive = {
      case WordCountReply(reply) => println(reply)
      case _ => {
        wordCounter ! Initialize(2)
        wordCounter ! WordCountTask("please count this")
        wordCounter ! WordCountTask("this set of words")
        wordCounter ! WordCountTask("over and over and over")
        wordCounter ! WordCountTask("please please please please please")
      }
    }
  }

  val actorSystem = ActorSystem("actorSystem")
  val wordCounter = actorSystem.actorOf(Props[WordCounterMaster])
  val requestor = actorSystem.actorOf(Props[Requestor])



  requestor ! "request!"



}
