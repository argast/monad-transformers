
import dispatch.Http

import scala.concurrent.{Await, Future}
import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext.Implicits.global
import dispatch._
import scala.concurrent.duration._



object Test extends App {

  type Tracer[A] = Writer[List[String], A]
  type Metrics[A] = Writer[List[String], A]

  import EitherT._

  type EitherTTracer = EitherT[Tracer, String, Int]

  def w(i: Int): Tracer[\/[String, Int]] = i.right[String].set(List("a"))

  def w1(i: Int): EitherTTracer = {
    EitherT[Tracer, String, Int](i.right[String].set(List("a")))
  }

  def f(i: Int): \/[String, Int] = i.right[String]

  val ttt: EitherT[Tracer, String, Int] = for {
    t0 <- w1(1)
    t1 <- w1(t0 + 1)
    t2 <- w1(t1 + 3)
  } yield t2


  val trace: Tracer[\/[String, Int]] => \/[String, Int] = t => {
    println(t.run._1.mkString("\n"))
    t.run._2
  }

  def print[A](v: A) = println(v)

  println(ttt)

  trace andThen print _ apply ttt.run



}
