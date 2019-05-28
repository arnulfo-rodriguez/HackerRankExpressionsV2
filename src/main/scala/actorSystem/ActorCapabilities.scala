package actorSystem

import actorSystem.ActorsIntro.{WordCounterActor, actorSystem}
import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {
  val actorSystem = ActorSystem("firstActorSystem")

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message: String => println(s"Hi I received the message $message")
      case number: Int => println(s"Hi I received the message $number")
      case SendMessageTo(actorRef) =>
    }
  }

  //  val simpleActor = actorSystem.actorOf(Props[SimpleActor], "simpleActor")
  //  simpleActor ! "Hello World"
  //  simpleActor ! 42

  case class SendMessageTo(actorRef: ActorRef)




  class BankAccount extends Actor {
    private var transactions = Seq[Transaction]()

    private def balance() = transactions.foldLeft(0)((total,current) => current + total)


    override def receive: Receive = {
      case d: Transaction if validate(d) => {
        transactions = transactions :+ d
        context.sender() ! Success(d)
      }
      case d : Transaction => context.sender() ! Failure(d)
      case Statement() => context.sender() ! AccountStatement(transactions.to[scala.collection.immutable.Seq])
    }

    private def validate(d: Transaction) = {
      (d + balance()) >= 0
    }
  }

  class Teller(bankAccount: ActorRef) extends Actor {
    override def receive: Receive = {
      case t : Transaction => bankAccount !  t
      case t: Statement => bankAccount !  t
      case Success(t) => println(s"Transaction $t succeded")
      case Failure(t) => println(s"Transaction $t failed")
      case AccountStatement(transactions) => println(s"The statement is: $transactions")
    }
  }

  trait Transaction {
    protected def balanceAmount(): Int

    def + (other: Transaction): Int = balanceAmount() + other.balanceAmount()
    def + (other: Int): Int = balanceAmount() + other
  }

  case class Deposit(amount: Int) extends Transaction {
    override def balanceAmount(): Int = amount
  }

  case class Withdrawal(amount: Int) extends Transaction {
    override def balanceAmount(): Int = -1 * amount
  }

  case class Failure(transaction: Transaction)

  case class Success(transaction: Transaction)

  case class Statement()

  case class AccountStatement(transactions : scala.collection.immutable.Seq[Transaction])


  val bankAccount = actorSystem.actorOf(Props[BankAccount], "bankAccount")
  val teller = actorSystem.actorOf(Props(new Teller(bankAccount)), "teller")

  teller ! Deposit(1)
  teller ! Deposit(2)
  teller ! Withdrawal(1)
  teller ! Statement()
  teller ! Withdrawal(5)

}


