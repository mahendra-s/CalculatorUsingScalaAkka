package com.example

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await
import CalculatorActor._

/**
  * Created by mshinde on 28-05-2018.
  */
object CalculatorClient extends App {
  //  1. remove spaces
  //  2. execute at every =
  val ops = collection.mutable.Stack[String]()
  val values = collection.mutable.Stack[Double]()

  val system = ActorSystem("CalculatorActorSystem")
  val calculatorActor = system.actorOf(Props[CalculatorActor], "calculator")


  //  val result : Double = evaluate("0001900 * ( 2 + 12.09 )".replace(" ", ""))
  //  if (result % 1 == 0 )

  var input = ""
  while (true) {
    Console.in.readLine() match {
      case "" => // Do nothing with empty
      case "C" | "c" => println("Clear Screen routine ")
      case "Off" => system.shutdown();  System.exit(0)
      case expr if (expr contains ("=")) => println(evaluate(expr))
      case expr => println(evaluate(expr))
    }
  }

  def isHighPreced(op1: String, op2: String): Boolean = {
    if (op2 == "(" || op2 == ")") false
    else if ((op1 == "*" || op1 == "/") && (op2 == "+" || op2 == "-")) false
    else true
  }

  /* Mathematical Calculation is performed by Calculator Actor
  * and encapsulated result is returned
  *  */
  def perform(a: Double, b: Double, op: String): Double = {
    println(s"Evaluating $a $op $b")
    //    op match {
    //      case "-" => a - b
    //      case "+" => a + b
    //      case "*" => a * b
    //      case "/" => if (b != 0) a / b else 0
    //      case _ => println(s"Invalid operator $op"); 0.0
    //    }
    implicit val timeout = Timeout(5 seconds)
    val future = calculatorActor ? Perform(b, op, a)
    Await.result(future, timeout.duration).asInstanceOf[Result].resultValue
  }

  def evaluate(expression: String): Double = {
    /*
     * I am using 'positive lookarounds' pattern matching for tokenizing input string
     * (?=) look to right
     * (?<=) look to-left
     * */
    val regex = "(?<=op)|(?=op)".replace("op", "[-+*/(){=}]")

    for (elem <- expression.replace(" ", "").split(regex)) {
      elem match {
        //case ' ' => // DO Nothing
        case "+" | "-" | "*" | "/" => while (ops.length != 0 && isHighPreced(elem, ops.head)) {
          values.push(perform(values.pop, values.pop, ops.pop))
        }
          ops.push(elem)
        //        case c if( c == "=") =>
        //Additional cases for BODMAS
        case "(" => ops.push(elem)
        case ")" => while (ops.head != "(") values.push(perform(values.pop, values.pop, ops.pop))
          ops.pop
        case "=" => while (ops.length != 0) values.push(perform(values.pop, values.pop, ops.pop))

        case _ => try {
          values.push(
            elem.toDouble)
        }
        catch {
          case e: Exception => println(s" Number format exception while converting$elem")
        }
      }
    }

    while (ops.length != 0)
      values.push(perform(values.pop, values.pop, ops.pop))
    values.pop
  }
}