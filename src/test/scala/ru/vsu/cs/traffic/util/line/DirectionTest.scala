package ru.vsu.cs.traffic.util.line

import org.scalatest.FunSuite
import ru.vsu.cs.traffic.Direction._
import ru.vsu.cs.traffic.{Direction, Point}

class DirectionTest extends FunSuite {

  val q0 = Point(0, 1)
  val q1 = Point(0, 3)
  val p0 = Point(0, 0)
  val p1 = Point(1, 1)
  val p2 = Point(1, -1)
  val p3 = Point(-1, 1)
  val p4 = Point(-1, -1)

  test("Direction RIGHT 1") {
    assert(direction(q0, q1, p0, p1) === Direction.RIGHT)
  }

  test("Direction RIGHT 2") {
    assert(direction(q0, q1, p0, p2) === Direction.RIGHT)
  }

  test("Direction LEFT 1") {
    assert(direction(q0, q1, p0, p3) === Direction.LEFT)
  }

  test("Direction LEFT 2") {
    assert(direction(q0, q1, p0, p4) === Direction.LEFT)
  }

  test("Direction BACK") {
    assert(direction(q0, q1, q1, q0) == BACK)
  }

  test("Direction FORWARD") {
    assert(direction(q0, q1, q0, q1) == FORWARD)
  }

}
