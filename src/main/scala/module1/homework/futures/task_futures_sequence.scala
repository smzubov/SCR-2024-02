package module1.homework.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val allFutures: Future[List[Either[Throwable, A]]] = Future.traverse(futures) { future =>
      future.map(Right(_)).recover { case t => Left(t) }
    }

    allFutures.map { results =>
      val (successfulResults, failedResults) = results.partition(_.isRight)
      (
        successfulResults.collect { case Right(a) => a },
        failedResults.collect { case Left(e) => e }
      )
    }
  }
}

