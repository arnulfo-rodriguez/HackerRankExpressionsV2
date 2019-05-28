package exercises

import akka.actor.{Actor, ActorSystem, Props}

object ImmutableCounter extends App {

  object CounterActor {
    case object Increment
    case object Decrement
    case object Print
  }

  class CounterActor extends Actor {
    import CounterActor._

    def receiveN(amount: Int): Receive = {
      case Increment => context.become(receiveN(amount + 1))
      case Decrement => context.become(receiveN(amount - 1))
      case Print => println(s"You have $amount")
    }

    override def receive: Receive = receiveN(0)
  }

  import CounterActor._

  val actorSystem = ActorSystem("firstActorSystem")
  val counterActor = actorSystem.actorOf(Props[CounterActor], "counterActor")
  counterActor ! Increment
  counterActor ! Increment
  counterActor ! Decrement
  counterActor ! Print

}
