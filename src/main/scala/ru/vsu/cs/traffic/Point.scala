package ru.vsu.cs.traffic

class Point(val x: Double, val y: Double) {

  def --(other: Point): Double = math.sqrt(math.pow(x, 2) + math.pow(y, 2))

  override def toString = "(" + x + ", " + y + ")"
}
