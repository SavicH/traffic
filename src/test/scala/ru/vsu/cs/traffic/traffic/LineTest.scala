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
    val q3 = new Point(5, 5)
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

  test("Get A coefficient that equal NaN") {
    new TestPoints {
      assert(line.getB(p1, p2).isNaN)
    }
  }

  test("Get A coefficient that equal zero") {
    new TestPoints {
      assert(line.getA(p1, p4) === 0)
    }
  }

  test("Distance test") {
    new TestPoints {
      assert(p1 -- p5 === 5)
    }
  }

  test("Distance to point transformation") {
    new TestPoints {
       assert(line.distance2point(math.sqrt(1.25), p1, p3) === new Point(2, 1.5))
    }
  }

  test("Distance to point transformation throws exception if distance is bigger than lenght of line") {
    new TestPoints {
      intercept[IllegalArgumentException] {
        line.distance2point(10, p1, p3)
      }
    }
  }

  test("Intersection 1") {
    new TestPoints {
      assert(line.intersection(q1, q2, Point(4, 3), Point(6, 7)) === q3)
    }
  }

  test("Intersection 2") {
    new TestPoints {
      assert(line.intersection(q1, q2, Point(5, 10), Point(5, 3)) === q3)
    }
  }

  test("Intersection 3") {
    new TestPoints {
      assert(line.intersection(Point(5, 10), Point(5, 3), q1, q2) === q3)
    }
  }

  test("Intersection that doesn't exist") {
    new TestPoints {
      assert(line.intersection(q1, q2, Point(0, 0), Point(-1, -1)) === null)
    }
  }
}
