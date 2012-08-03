import scala.annotation.tailrec

object chapter2 {
  val abs: Int => Int = n => if (n < 0) -n else n

  type Pred[A] = A => Boolean
  
  type BindF[A] = (A, A) => A

  def isDivisibleBy(k: Int): Pred[Int] = n => n % k == 0

  val isEven: Pred[Int] = n => isDivisibleBy(2)(n)

  val isOdd: Pred[Int] = not(isEven)

  def not(p: Pred[Int]): Pred[Int] = n => !(p(n))

  def isDivisibleBy3And5: Pred[Int] = n => lift(_ && _)(n)
  
  def isDivisibleBy3Or5: Pred[Int] = n => lift(_ || _)(n)

  def lift(bindF: BindF[Boolean]): Pred[Int] = n => 
    // TODO exercise 2.6.4 missing
    bindF(isDivisibleBy(3)(n), isDivisibleBy(5)(n))

  def applyIf[A](a: A, p: A => Boolean, f: A => A): A = {
    // TODO better names
    val bindF: (Boolean, A) => A = (pp, aa) => if (pp) f(aa) else aa
    bind(a, p, bindF)
  }

  def bind[A, B, C](a: A, p: A => B, f: (B, A) => C): C = f(p(a), a)
 
  def fib(n: Int): Int = n match {
    case 1 | 2 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }

  def fib1(n: Int): Int = {
    @tailrec def fib1Tr(n: Int, b: Int, a: Int): Int = n match {
      case 0 => a
      case _ => fib1Tr(n - 1, a + b, b)
    }
    fib1Tr(n, 1, 0)
  }
  
  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n
    iterateWhile(2.0)(x => x - f(x) / (2 * x),
                      x => f(x).abs > 1e-14)
  }

  @tailrec
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = p(a) match {
    case true => iterateWhile(f(a))(f, p)
    case false => f(a)
  }
}
