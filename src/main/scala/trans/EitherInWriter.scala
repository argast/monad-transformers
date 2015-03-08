package trans



import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, _}
import scala.concurrent.duration._
import scalaz.Id._
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.{Future => ZFuture}


object EitherInWriter extends App {
  
  type Result[A] = String \/ A
  type Tracer[A] = Writer[String, A]
  type EitherTracer[A] = EitherT[Tracer, String, A]

  def trans1(s: String): EitherTracer[String] = {
    val tracer: Tracer[String \/ String] = "aa".right[String].set("aa")
    EitherT[Tracer, String, String](tracer)
  }

  val result = for {
    p <- trans1("1")
    r <- trans1(p)
  } yield r

  println(result)
  println(result.run)
  println(result.run.run)


}
