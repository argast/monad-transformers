package trans

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, _}
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.{Future => ZFuture}


object EitherInWriterInFuture extends App {

  type FutureWriter[A] = Future[Writer[String, A]]
  type Tracer[A] = WriterT[Future, String, A]
  type Result[A] = EitherT[Tracer, String, A]

  implicit def futureWriterIsFutureTuple[W, A](f: Future[Writer[W, A]]): Future[(W, A)] = f.map(_.run)

  def trans(s: String): Result[String] = {
    val future: FutureWriter[String \/ String] = Future("aa".right[String].set("bb"))
    val writerT: Tracer[String \/ String] = WriterT[Future, String, String \/ String](future)
    EitherT[Tracer, String, String](writerT)
  }

  val result = for {
    p <- trans("1")
    r <- trans(p)
  } yield r

  def trace[A](w: Result[A]): Future[String \/ A] = {
    w.run.written.map(println)
    w.run.run.map(_._2)
  }

  def handle[A](v: Future[A]): Unit = {
    println(Await.result(v, 1 second))
  }

  trace[String] _ andThen handle apply result


}
