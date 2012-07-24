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

  // Not really what is wanted in 2.6.2?
  def lift(bindF: BindF[Boolean]): Pred[Int] = n => 
    bindF(isDivisibleBy(3)(n), isDivisibleBy(5)(n))
}
