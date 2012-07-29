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
//    bind(n, bindF, 
    bindF(isDivisibleBy(3)(n), isDivisibleBy(5)(n))

  def applyIf[A](a: A, p: A => Boolean, f: A => A): A = {
    // TODO better names
    val bindF: (Boolean, A) => A = (pp, aa) => if (pp) f(aa) else aa
    bind(a, p, bindF)
  }

  def bind[A, B, C](a: A, p: A => B, f: (B, A) => C): C = f(p(a), a)
}
