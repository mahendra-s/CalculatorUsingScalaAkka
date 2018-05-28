package com.example

import akka.actor.{Actor, ActorLogging}

object CalculatorActor {

  case class Perform(val operandA: Double, val operator: String, val operandB: Double)

  case class Result(val resultValue: Double)

}

class CalculatorActor extends Actor with ActorLogging {

  import CalculatorActor._

  def receive = {
    case Perform(a, op, b) => {
      log.info(s"Operation request received $a $op $b")
      op match {
        case "+" => sender() ! Result((a + b))
        case "-" => sender() ! Result((a - b))
        case "*" => sender() ! Result((a * b))
        case "/" => sender() ! Result((if (b == 0) 0 else a / b))
        case _ => log.warning("Invalid operation request ")
      }
    }
    //    case _ => sender() ! Result("Invalid Request to calculator")
  }
}

