package exercises

import scala.annotation.tailrec

object SolutionSierpinski {

  case class Point(x: Int, y: Int)

  case class Triangle(startingPosition: Point, height: Int) {

    def splitTop() = Triangle(startingPosition, height / 2)

    def splitLeft() = Triangle(Point(startingPosition.x + height / 2, startingPosition.y - height / 2), height / 2)

    def splitRight() = Triangle(Point(startingPosition.x + height / 2, startingPosition.y + height / 2), height / 2)

    @tailrec
    private def tailRectPoints(level: Int, list: Seq[Point]): Seq[Point] =
      if (level > height)
        list
      else
        tailRectPoints(level + 1, list ++ (1 until (level * 2))
          .map { it => Point(startingPosition.x + level - 1, startingPosition.y - level + it) })

    def points(): Seq[Point] = tailRectPoints(2, Seq(startingPosition))
  }

  class AsciiMatrix(width: Int, height: Int, points: Map[Int, Set[Int]]) {
    def this(width: Int, height: Int, toFlatten: Seq[Point]) {
      this(width, height, toFlatten.foldLeft(Map[Int, Set[Int]]()) { (acc, current) =>
        acc + (current.x -> (acc.getOrElse(current.x, Set[Int]()) + current.y))
      })
    }

    def draw(): Unit = {
      (1 to height)
        .foreach { x =>
          (1 to width).foreach { y =>
            print(
              points.get(x).map { set => set(y) }.map(b => if (b) "1" else "_").getOrElse("_")
            )
          }
          println()
        }
    }
  }


  def sierpinski(t: Triangle, iterations: Int): List[Triangle] = iterations match {
    case 1 => List(t)
    case n => sierpinski(t.splitTop(), n - 1) ::: sierpinski(t.splitLeft(), n - 1) ::: sierpinski(t.splitRight(), n - 1)
  }


  def main(args: Array[String]) {
    val iteration = scala.io.StdIn.readLine().trim.toInt
    new AsciiMatrix(129, 64,
      sierpinski(Triangle(Point(1, 65), 64), iteration + 1)
        .flatMap { it => it.points() })
      .draw()
  }

}
