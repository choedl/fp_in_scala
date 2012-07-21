object chapter2 {

  val abs: Int => Int = n => if (n < 0) -n else n

  type Pred[A] = A => Boolean

  def isDivisibleBy(k: Int): Pred[Int] = n => n % k == 0

  val isEven: Int => Boolean = n => isDivisibleBy(2)(n)

  val isOdd: Int => Boolean = not(isEven)

  def not(p: Int => Boolean): Int => Boolean = n => !(p(n))
}
