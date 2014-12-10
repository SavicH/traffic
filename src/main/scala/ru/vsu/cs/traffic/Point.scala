package ru.vsu.cs.traffic

class Point(val x: Double, val y: Double) {

  def --(other: Point): Double = math.sqrt(math.pow(x, 2) + math.pow(y, 2))

  def ==(other: Point): Boolean = (x == other.x) && (y == other.y)

  override def toString = "(" + x + ", " + y + ")"


}
