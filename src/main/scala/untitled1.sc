trait Functor[F[_]] {
  def map[A,B](fa:F[A])(f:A=>B):F[B]
}

case class ListFunctor() extends Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
}

case class OptionFunctor() extends Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B) = fa.map(f)
}

implicit  val listFunctor: ListFunctor = ListFunctor()

val _f = (x:Int) => x * x

def foo(list:List[Int])(implicit listFunctor: ListFunctor) = listFunctor.map(list)(_f)


foo(List(1,2,3))







