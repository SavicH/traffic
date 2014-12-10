package ru.vsu.cs.traffic.util

import ru.vsu.cs.traffic.Point

object line {

  def getA(start: Point, end: Point) =
    if (start.x == end.x) Double.NaN else (start.y - end.y)/(start.x - end.x)

  def getB(start: Point, end: Point) = start.y - start.x * getA(start, end)

  def distance2point(distance: Double, start: Point, end: Point) = {
    val k = distance/(start -- end)
    if ((k < 0) || (k > 1)) {
      throw new IllegalArgumentException("The distance must be positive and lesser than length of the segment")
    }
    val x = start.x + k * (end.x - start.x)
    val y = start.y + k * (end.y - start.y)
    new Point(x, y)
  }
}
