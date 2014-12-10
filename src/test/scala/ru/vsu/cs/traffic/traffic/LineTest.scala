package ru.vsu.cs.traffic.traffic

import org.scalatest.FunSuite
import ru.vsu.cs.traffic.Point
import ru.vsu.cs.traffic.util.line

class LineTest extends FunSuite {

  trait TestPoints {
    val p1 = new Point(1, 1)
    val p2 = new Point(1, 3)
    val p3 = new Point(3, 2)
    val p4 = new Point(3, 1)
    val p5 = new Point(4, 5)
    val q1 = new Point(1, 5)
    val q2 = new Point(9, 5)
  }

  test("get A coefficient") {
    new TestPoints {
      assert(line.getA(p1, p3) === 0.5)
    }
  }

  test("get B coefficient") {
    new TestPoints {
      assert(line.getB(p1, p3) === 0.5)
    }
  }

  test("get A coefficient that equals NaN") {
    new TestPoints {
      assert(line.getA(p1, p2).isNaN)
    }
  }

  test("get A coefficient that equal NaN") {
    new TestPoints {
      assert(line.getB(p1, p2).isNaN)
    }
  }

  test("get A coefficient that equal zero") {
    new TestPoints {
      assert(line.getA(p1, p4) === 0)
    }
  }
}
