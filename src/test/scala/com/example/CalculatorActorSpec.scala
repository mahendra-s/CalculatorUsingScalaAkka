package com.example

import akka.actor.ActorSystem
import akka.actor.Props
import akka.testkit.{ImplicitSender, TestKit}
import com.example.CalculatorActor.{Perform, Result}
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

class CalculatorActorSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("MySpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A Calculator actor" must {
    "send back a sum result on a request to perform sum" in {
      val calculatorActor = system.actorOf(Props[CalculatorActor])
      calculatorActor ! Perform(10, "+", 20)
      expectMsg(Result(30))
    }
  }

  "A Calculator actor" must {
    "send back a multiple result on a request to perform multiplication" in {
      val calculatorActor = system.actorOf(Props[CalculatorActor])
      calculatorActor ! Perform(10, "*", 20)
      expectMsg(Result(200))
    }
  }

}
