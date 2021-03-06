package ru.vsu.cs.traffic

import akka.actor.{ActorRef, Props, UntypedActor}

case class Time(timeStep: Double)

case class Done()

trait TrafficActor {

  val DefaultTimeStep = 1.0

  private[traffic] def act(timeStep: Double): Unit

  private[traffic] def !(message: Any): Unit = actor ! message

  private[traffic] val model: TrafficModel

  lazy protected val actor: ActorRef = model.actorSystem.actorOf(Props(new InnerActor))

  protected class InnerActor extends UntypedActor {
    @throws[Exception](classOf[Exception])
    override def onReceive(message: Any): Unit = TrafficActor.this.onReceive(message)
  }

  protected def onReceive(message: Any): Unit
}
