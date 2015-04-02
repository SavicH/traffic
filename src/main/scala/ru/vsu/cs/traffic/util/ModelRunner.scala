package ru.vsu.cs.traffic.util

import java.util.TimerTask

import ru.vsu.cs.traffic.TrafficModel
import ru.vsu.cs.traffic.event.ModelStopped

class ModelRunner(creator: () => TrafficModel, time: Int, count: Int, timeout: Int, callback: () => Unit = null) {

  private val timer = new java.util.Timer()
  private var lastStart = 0
  private var current = 0
  private var isFinished = true
  private var model: TrafficModel = _
  private var i = 0


  private def start(): Unit = {
    isFinished = false
    lastStart = current
    println(s"Start $i")
    model = creator()
    model.trafficModelEventHandlers += {
      case ModelStopped() =>
        isFinished = true
        println(s"Finish $i")
        i += 1
      case _ => Unit
    }
    model.asyncRun(time)
  }

  private class Task extends TimerTask {

    override def run(): Unit = {
      current += 1
      if (isFinished && i < count) {
        start()
      }
      if (i == count) {
        timer.cancel()
        if (callback != null) {
          callback()
        }
      }
      if (current - lastStart > timeout) {
        println(s"Restart $i")
        start()
      }
    }
  }

  def run(): Unit = {
    timer.schedule(new Task(), 0, 1000)
  }

}

object ModelRunner {
  def apply(creator: () => TrafficModel, time: Int, count: Int, timeout: Int, callback: () => Unit = null) = {
    new ModelRunner(creator, time, count, timeout, callback)
  }
}
