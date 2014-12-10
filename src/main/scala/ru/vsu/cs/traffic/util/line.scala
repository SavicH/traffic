package ru.vsu.cs.traffic.util

import ru.vsu.cs.traffic.Point

object line {

  def getA(start: Point, end: Point) =
    if (start.x == end.x) Double.NaN else (start.y - end.y)/(start.x - end.x)

  def getB(start: Point, end: Point) = start.y - start.x * getA(start, end)

}
