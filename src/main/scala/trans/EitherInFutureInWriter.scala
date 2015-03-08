package trans

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, _}
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.{Future => ZFuture}


object EitherInFutureInWriter extends App {

  case class FutureT[F[_], A](run: F[Future[A]]) {
    self =>

    def map[B](f: A => B)(implicit F: Functor[F]): FutureT[F, B] = {
      val aa = F.map(run)(_.map(f))
      FutureT(aa)
    }

    def flatMap[B](f: A => FutureT[F, B])(implicit F: Monad[F]): FutureT[F, B] = {

      FutureT(F.bind(run) { fa =>
        val fb = fa map f
        // can that be avoided?
        Await.result(fb, 1 second).run
      })

    }
  }

  implicit val futureTFunctor = new Functor[FutureTracer] {
    override def map[A, B](fa: FutureTracer[A])(f: (A) => B): FutureTracer[B] = {
      fa map f
    }
  }

  implicit val futureTMonad = new Monad[FutureTracer] {
    override def bind[A, B](fa: FutureTracer[A])(f: (A) => FutureTracer[B]): FutureTracer[B] = {
      fa flatMap f
    }

    override def point[A](a: => A): FutureTracer[A] = {
      val tracer: Tracer[Future[A]] = Future.successful(a).set("")
      FutureT(tracer)
    }
  }

  type Tracer[A] = Writer[String, A]
  type FutureTracer[A] = FutureT[Tracer, A]
  type EitherTracer[A] = EitherT[FutureTracer, String, A]

  def trans(s: String): EitherTracer[String] = {
    val tracer: Tracer[Future[String \/ String]] = Future("aa".right[String]).set("bb")
    val futureT: FutureTracer[String \/ String] = FutureT[Tracer, String \/ String](tracer)
    EitherT[FutureTracer, String, String](futureT)
  }

  val result = for {
    p <- trans("1")
    r <- trans(p)
  } yield r

  def trace[A](w: EitherTracer[A]): Future[String \/ A] = {
    println(w.run.run.written)
    w.run.run.run._2
  }

  def handleResult[A, B](a: Future[A]): Unit = {
    println(Await.result(a, 1 second))
  }

  trace[String] _ andThen handleResult apply result

}
