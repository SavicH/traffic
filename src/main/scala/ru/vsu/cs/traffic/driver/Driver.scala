package ru.vsu.cs.traffic.driver

import java.awt
import java.awt.event.ActionListener

import ru.vsu.cs.traffic.TrafficModel

import scala.swing.{MainFrame, SimpleSwingApplication}

trait Driver extends SimpleSwingApplication {

  val model: TrafficModel

  lazy val panel = new TrafficModelPanel(model)

  def top = new MainFrame {
    title = "Traffic Simulation"
    contents = panel
    val timer = new javax.swing.Timer(40, new ActionListener {
      override def actionPerformed(e: awt.event.ActionEvent): Unit = panel.repaint()
    })
    timer.start()
  }
}