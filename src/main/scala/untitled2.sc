
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

case class IntMonoid() extends Monoid[Int] {
  override def op(a1: Int, a2: Int) = a1 + a2
  override def zero = 0
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
}

implicit val intModulo: IntModulo = IntModulo(3) // Z[2]
// implicit val intMonoid: IntMonoid = IntMonoid() // Z

def sumContext[A,B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B =
  as.map(f).foldLeft(m.zero)(m.op)

val _f = (a:Int) => a

implicit val intModulo: IntModulo = IntModulo(3) // Z[2]
val test = sumContext(List(1,2,3))(_f)
