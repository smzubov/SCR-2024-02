package module1.homework

object LinearAlgebraOps {
  def sum(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length != v2.length) throw new Exception("Operation not supported!!!")


    val result = Array.ofDim[Int](v1.length)
    for (i <- v1.indices) {
      result(i) = v1(i) + v2(i)
    }
    result
  }

  val sumAlt: (Array[Int], Array[Int]) => Array[Int] = (vector1, vector2) => {
    if (vector1.length != vector2.length) throw new Exception("Operation not supported!!!")

    val result = (vector1, vector2).zipped.map(_ + _)
    result
  }


  def scale(a: Int, vector: Array[Int]): Array[Int] = {
    for (i <- vector.indices) {
      vector(i) = vector(i) * a
    }
    vector
  }

  val scaleAlt: (Int, Array[Int]) => Array[Int] = (a, vector) => {
    vector.map(a * _)
  }

  def scaleSum(a: Int, v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length != v2.length) throw new Exception("Lengths of these vectors aren't equals. Sorry(")
    val result = Array.ofDim[Int](v1.length)
    for(i <- v1.indices) {
      result(i) = (a * v1(i)) + v2(i)
    }
    result
  }

  val scaleSumAlt: (Int, Array[Int], Array[Int]) => Array[Int] = (a, v1, v2) => {
    if (v1.length != v2.length) throw new Exception("Lengths of these vectors aren't equals. Sorry(")
    (v1, v2).zipped.map((x, y) => a * x + y)
  }


  def main(args: Array[String]): Unit = {
    val vector1 = Array(1, 2, 3)
    val vector2 = Array(4, 5, 6)

    val a = 4


    val resultSum = sum(vector1, vector2)
    println("Result method of sum: " + resultSum.mkString(", "))

    val resultSumAlt = sumAlt(vector1, vector2)
    println("Result method of sumAlt: " + resultSumAlt.mkString(", "))

    val resultScaleSum = scaleSum(a, vector1, vector2)
    println("Result method of scaleSum: " + resultScaleSum.mkString(", "))

    val resultScaleSumAlt = scaleSumAlt(a, vector1, vector2)
    println("Result method of scaleSumAlt: " + resultScaleSumAlt.mkString(", "))

    val resultScale = scale(a, vector2)
    println("Result method of scale: " + resultScale.mkString(", "))

    val resultScaleAlt = scaleAlt(a, vector1)
    println("Result method of scaleAlt: " + resultScaleAlt.mkString(", "))

  }
}