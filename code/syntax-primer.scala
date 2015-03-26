// Algebraic Data Types

type User = String

sealed trait TwitterAPI
case class Timeline(user: User, count: Int) extends TwitterAPI
case class StatusUpdate(user: User, status: String) extends TwitterAPI
case class Search(query: String) extends TwitterAPI

// Typeclasses

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

implicit val optionFunctor = new Functor[Option] {
  def map[A, B](opt: Option[A])(f: A => B) = opt map f
}

def addWorld[F[_]](fs: F[String])(implicit f: Functor[F]): F[String] = 
  f.map(fs)(_ + " World!")

val addedWorld = addWorld(Option("Hello")) // Some(Hello World!)