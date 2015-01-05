package ru.vsu.cs.traffic.util

import ru.vsu.cs.traffic.{Direction, Point}

import scala.math._

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

  private def isBetween(x: Double, a: Double, b: Double) =
    (math.min(a, b) <= x) && (x <= math.max(a, b))

  private def isBetween(p: Point, start: Point, end: Point): Boolean = {
    isBetween(p.x, start.x, end.x) && isBetween(p.y, start.y, end.y)
  }

  def intersection(s1: Point, e1: Point, s2: Point, e2: Point) = {
    val a1 = getA(s1, e1)
    val b1 = getB(s1 ,e1)
    val a2 = getA(s2, e2)
    val b2 = getB(s2, e2)
    val p = if (a1.isNaN) Point(s1.x, a2 * s1.x + b2)
      else if (a2.isNaN) Point(s2.x, a1 * s2.x + b1)
        else {
          val x = (b1 - b2)/(a2 - a1)
          val y = a1 * x + b1
          Point(x, y)
        }
    if (isBetween(p, s1, e1) && isBetween(p, s2, e2)) p else null
  }

  private class Vector(s: Point, e: Point) extends Point(s.x - e.x, s.y - e.y) {

    //a length of the projection of the cross product
    def **(other: Vector): Double = x * other.y - y * other.x
  }

  def direction(s1: Point, e1: Point, s2: Point, e2: Point): Direction = {
    val r = (s1.x - e1.x) * (s2.y - e2.y) - (s1.y - e1.y) * (s2.x - e2.x)
    if (r == 0) throw new IllegalArgumentException("Same direction")
    if (r < 0) Direction.RIGHT else Direction.LEFT
  }
}
