object chapter2 {

  val abs: Int => Int = n => if (n < 0) -n else n

  type Pred[A] = A => Boolean

  def isDivisibleBy(k: Int): Pred[Int] = n => n % k == 0

  val isEven: Pred[Int] = n => isDivisibleBy(2)(n)

  val isOdd: Pred[Int] = not(isEven)

  def not(p: Pred[Int]): Pred[Int] = n => !(p(n))
  
}
