package ru.vsu.cs.traffic.gui

import java.awt.{Color, Dimension, Graphics2D}

import ru.vsu.cs.traffic.Color._
import ru.vsu.cs.traffic.Direction.FORWARD
import ru.vsu.cs.traffic.{TrafficFlow, TrafficLight, TrafficModel, Vehicle}

import scala.swing.Panel
import scala.swing.event.{FocusLost, MousePressed}

class TrafficModelPanel(val model: TrafficModel) extends Panel {

    private val Offset = 10

    private def offsetSign(f: TrafficFlow) = if (f.start.x - f.end.x + f.start.y - f.end.y > 0) 1 else -1

    private def getOffsetX(f: TrafficFlow) = if (f.start.y == f.end.y) 0 else Offset

    private def getOffsetY(f: TrafficFlow) = if (f.start.x == f.end.x) 0 else Offset

  protected def drawTrafficFlow(f: TrafficFlow, g: Graphics2D) = {
      g.setColor(Color.black)
      val offsetX = getOffsetX(f) * offsetSign(f)
      val offsetY = getOffsetY(f) * offsetSign(f)
      for (i <- 1 to f.lanes) {
        g.drawLine(f.start.x.toInt + offsetX * i, f.start.y.toInt + offsetY * i,
          f.end.x.toInt + offsetX * i, f.end.y.toInt + offsetY * i)
      }
    for (v <- f.vehicles) {
      if (!v.distance.isNaN) {
        drawVehicle(v, g)
      }
    }
    }

  protected def drawVehicle(v: Vehicle, g: Graphics2D) = {
    g.setColor(Color.blue)
    val vehicleSize = 5
      val offsetX = getOffsetX(v.trafficFlow) * offsetSign(v.trafficFlow)
      val offsetY = getOffsetY(v.trafficFlow) * offsetSign(v.trafficFlow)
      g.drawOval(v.location.x.toInt + offsetX * v.lane, v.location.y.toInt + offsetY * v.lane, vehicleSize, vehicleSize)
    }

  protected val TrafficLightColors = Map(RED -> Color.red, GREEN -> Color.green, YELLOW -> Color.yellow)

  protected def drawTrafficLight(t: TrafficLight, g: Graphics2D): Unit = {
    val trafficLightSize = 10
    g.setColor(TrafficLightColors(t.color))
    val offsetX = getOffsetX(t(FORWARD)) * offsetSign(t(FORWARD))
    val offsetY = getOffsetY(t(FORWARD)) * offsetSign(t(FORWARD))
    g.fillOval(t.location.x.toInt + offsetX - Offset / 2, t.location.y.toInt + offsetY - Offset / 2, trafficLightSize, trafficLightSize)
  }

    background = Color.white
    preferredSize = new Dimension(model.trafficFlows.map(_.start.x).max.toInt, model.trafficFlows.map(_.start.y).max.toInt)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    reactions += {
      case e: MousePressed  =>
        repaint()
      case _: FocusLost => repaint()
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      for {
        f <- model.trafficFlows
      } drawTrafficFlow(f, g)
      for {
        t <- model.trafficLights
      } drawTrafficLight(t, g)
      g.drawString(model.trafficLights(0).time.toString.substring(0, 4), 50, 50)
    }
}
