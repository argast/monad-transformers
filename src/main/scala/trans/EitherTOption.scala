package trans

import scalaz._
import Scalaz._


object EitherTOption extends App {

  def fun(i: Int) = EitherT[Option, String, Int](i.right[String].some)

  val c = for {
    a <- fun(1)
    b <- fun(a + 2)
  } yield a + b

  println(c)
  c.run.foreach(println)
}

