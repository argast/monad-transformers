package stack

import scala.concurrent.Future
import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext.Implicits.global

object Stack extends App {

  case class MetricsContext(start: Long)

  type Metrics[A] = Writer[MetricsContext, A]
  type Tracer[A] = WriterT[Metrics, List[String], A]
  type Result[A] = EitherT[Tracer, String, A]

  def trans(s: String): Result[String] = {
    val metrics: Metrics[String \/ String] = "aa".right[String].set(MetricsContext(0))
    val tracer: Tracer[String \/ String] = WriterT[Metrics, List[String], String \/ String](metrics.map((List(""), _)))
    EitherT[Tracer, String, String](tracer)
  }

  val a = for {
    b <- trans("b")
    c <- trans(b + "c")
    d <- trans(c + "d")
  } yield b + c + d

}
