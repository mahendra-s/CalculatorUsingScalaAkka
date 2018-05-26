# CalculatorUsingScalaAkka

package com.example

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.example.AkkaQuickstart.{system}

object Calculator {
  case class Perform(val operandA: Int, val operator: String, val operandB: Int)
  case class Result(val msg: String)
}

class Calculator extends Actor with ActorLogging {

  import Calculator._

  def receive = {
    case Perform( a, op, b) =>
    {
      log.info(s"Operation request received $a $op $b")
      op match {
        case "+" => sender() ! Result((a + b).toString)
        case "-" => sender() ! Result((a - b).toString)
        case "*" => sender() ! Result((a * b).toString)
        case "/" => sender() ! Result( (if(b == 0) "E" else a/b).toString)
        case _ => log.warning("Invalid operation request ")
      }
    }
    case _ => sender() ! Result("Invalid Request to calculator")
  }
}

class Client extends Actor with ActorLogging {

  import Calculator._
  val calc: ActorRef = system.actorOf(Props[Calculator], "Calculator")

  override def preStart(): Unit = {
    log.info(" Starting client")
//    calc ! Perform(10, "+", 20)
    val s = "50+20+23-342-2342="
    var left_op: Int = 0
    var right_op: Int = 0
    var op :String = ""
//    var div: Int = 1
    s.toCharArray.foreach(e =>
      e match{
        case e if(e >='0' && e <= '9') =>  if(op == "") left_op = left_op*10 + e.toString.toInt else right_op = right_op*10 + e.toString.toInt
        case e if(e == '.' ) =>
        case e if(e == '+' || e == '-' ||e == '*' ||e == '/' ) => op = e.toString
        case e if(e == 'c' || e == 'C' ) => println("Clear Console")
        case e if(e == '=' ) => calc ! Perform(left_op, op , right_op)
      }
    )

  }
   def receive: Receive = {
    case Result(msg) => log.info( msg)
   }

}

object AkkaQuickstart extends App {

  // Create the actor system
  val system: ActorSystem = ActorSystem("Calculator_System")

  //#create-actors
    val client : ActorRef = system.actorOf(Props[Client],  "Client")


  //system.terminate()
  }
//main-class

