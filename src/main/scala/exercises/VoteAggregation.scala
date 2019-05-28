package exercises

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.collection.immutable.{Map, HashMap}

object VoteAggregation extends App {

  case class Vote(candidate: String)

  case object VoteStatusRequest

  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor {
    override def receive: Receive = {
      case Vote(candidate) => context.become(voted(candidate))
      case VoteStatusRequest => sender() ! VoteStatusReply(None)
    }

    def voted(candidate: String): Receive = {
      case Vote(candidate) =>
      case VoteStatusRequest => sender() ! VoteStatusReply(Some(candidate))
    }
  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {
    override def receive: Receive = {
      case AggregateVotes(citizens) => {
        context.become(aggregate(citizens.size, HashMap(), sender()))
        citizens.foreach(citizen => citizen ! VoteStatusRequest)
      }
    }

    def aggregate(remaining: Int, aggregation: Map[String, Int], requestor: ActorRef): Receive = {
      case VoteStatusReply(vote) =>
        val newAggregation = vote match {
          case Some(candidate) => aggregation + (candidate -> aggregation.get(candidate).map(_ + 1).getOrElse(1))
          case None => aggregation
        }
        if (remaining - 1 > 0) {
          context.become(aggregate(
            remaining - 1,
            newAggregation,
            requestor
          ))
        } else {
          println(newAggregation)
        }
    }
  }

  val actorSystem = ActorSystem("firstActorSystem")

  val alice = actorSystem.actorOf(Props[Citizen])
  val bob = actorSystem.actorOf(Props[Citizen])
  val charlie = actorSystem.actorOf(Props[Citizen])
  val daniel = actorSystem.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voteAggregator = actorSystem.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))


}
