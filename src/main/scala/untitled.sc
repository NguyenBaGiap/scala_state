trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A

}

val listMonoid: Monoid[List[String]] = new Monoid[List[String]] {
  override def op(a1: List[String], a2: List[String]):List[String] = List(a1,a2).flatten

  override def zero:List[String] = List[String]()
}

val stringMonoid: Monoid[String] = new Monoid[String] {
  def op(a1: String, a2: String) = a1 + a2
  val zero = ""
}

case class IntMonoid() extends Monoid[Int] {
  override def op(a1: Int, a2: Int) = a1 + a2
  override val zero = 0

  val _f = (a:Int) => a
}

case class IntModulo(value:Int) extends Monoid[Int] {
  override def op(a1: Int, a2: Int) = {
    val sum = a1 + a2
    if(sum < value){
      sum
    } else {
      sum % value
    }
  }
  override def zero = 0
  val _f = (a:Int) => if(a < value ) a else a % value
}

implicit val intModulo: IntModulo = IntModulo(2) // Z[2]
 val intMonoid: IntMonoid = IntMonoid() // Z

def sum[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.map(a => f(a)).foldLeft(m.zero)(m.op)

val test = sum(List(1,2,3),intModulo)(intModulo._f)
val test2 = sum(List(1,2,3),intModulo)(intMonoid._f)

def sumContext[A,B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B =
  as.map(f).foldLeft(m.zero)(m.op)


val _f = (a:Int) => a

sumContext(List(1,2,3))(_f)




