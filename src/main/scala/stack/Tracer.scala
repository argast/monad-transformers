package stack

import scalaz._
import Scalaz._


case class Tracer[A](context: List[String], value: A) {

  def map[B](f: A => B): Tracer[B] = {
    Tracer(context, f(value))
  }

  def flatMap[B](f: A => Tracer[B]): Tracer[B] = {
    val tb = f(value)
    Tracer(context ++ tb.context, tb.value)
  }

  def trace(s: String) = {
    Tracer(s :: context, value)
  }
}

object Tracer {
  def apply[A](s: String, v: A): Tracer[A] = Tracer(List(s), v)

  implicit def toTracerOps[A](a: A) = new TracerOps(a)
}

final class TracerOps[A](self: A) {
  def trace(s: String) = Tracer(s, self)

}

case class Metrics[M, A](context: M, tracer: Tracer[A]) {
  
  def map[B](f: A => B): Metrics[M, B] = {
    Metrics(context, tracer map f)
  }

  def flatMap[B](f: A => Metrics[M, B])(implicit F: Semigroup[M]): Metrics[M, B] = {
    val mb = f(tracer.value)
    val tb = tracer.flatMap(_ => mb.tracer)
    Metrics(F.append(context, mb.context), tb)
  }
}

object Metrics {
  implicit def toMetricsOps[A](a: Tracer[A]) = new MetricsOps(a)
}

final class MetricsOps[A](self: Tracer[A]) {
  def start = Metrics(System.currentTimeMillis(), self)
  def noMetrics = Metrics(0L, self)
}

object Test extends App {

  import Tracer._
  import Metrics._

  val a = for {
    b <- "b".trace("tb").start
    c <- (b + "c").trace("tc").noMetrics
  } yield b + c

  def collectMetrics(m: Metrics[Long, String]) = {
    println("Metrics: " + (System.currentTimeMillis() - m.context))
    m.tracer
  }

  def trace(t: Tracer[String]) = {
    println(t.context)
    t.value
  }

  def handleResponse(a: String) = {
    println("Response: " + a)
    a
  }

  collectMetrics _ andThen trace andThen handleResponse _ apply a
}