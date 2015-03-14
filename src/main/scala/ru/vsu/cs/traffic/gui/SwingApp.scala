package ru.vsu.cs.traffic.gui

import java.awt
import java.awt.event.ActionListener

import ru.vsu.cs.traffic.TrafficModel

import scala.swing.{MainFrame, SimpleSwingApplication}

trait SwingApp extends SimpleSwingApplication {

  val model: TrafficModel

  val panel: TrafficModelPanel

  def top = new MainFrame {
    title = "Traffic Simulation"
    contents = panel
    val timer = new javax.swing.Timer(40, new ActionListener {
      override def actionPerformed(e: awt.event.ActionEvent): Unit = panel.repaint()
    })
    timer.start()
  }
}