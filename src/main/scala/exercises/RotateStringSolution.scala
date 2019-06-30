object RotateStringSolution {

  def rotate(s: String, nthRotation: Int) = {
    val initialOffset = nthRotation
    (0 until s.length).map { x =>
      (initialOffset + x) % s.length}
      .map { x => s(x) }
      .foreach(x => print(x))
  }

  def main(args: Array[String]) {
    val total = scala.io.StdIn.readLine().trim.toInt
    (1 to total).foreach { i =>
      val l = scala.io.StdIn.readLine()
      (1 to l.length).foreach { x =>
        rotate(l, x)
        print(" ")
      }
      println
    }

  }
}
