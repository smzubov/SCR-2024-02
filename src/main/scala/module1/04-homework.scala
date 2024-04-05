package module1
import scala.util.Random

class BlackAndWhiteBalls {
  private val bucket: List[Int] = List.fill(3)(1) ++ List.fill(3)(0)
  def selectBalls(): (Boolean, Boolean) = {
    val shuffled = Random.shuffle(bucket)
    (shuffled.head == 1, shuffled.tail.head == 1)
  }
}

object Experiment extends App {
  val experiment = List.fill(10000)(new BlackAndWhiteBalls)
  val white = experiment.map(_.selectBalls()).count{ balls => {
    val (first, second) = balls
    first || second
  }}

  println(s"Вероятность появления белого шара составляет: ${white.toDouble / experiment.size}")
}