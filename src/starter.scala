object starter extends App {

	  //sum of all prime number less than max
	  def problem10(max: Int) : Int = {
	    def isPrime(no: Int, primes: List[Int]) : Boolean = if (primes.isEmpty) true else if (no % primes.head==0) false else isPrime(no, primes.tail)
	    
	    def problemImpl(cur: Int, primes: List[Int]) : List[Int] = 
	      if (cur>=max) primes else
	        if (isPrime(cur, primes)) {
	          problemImpl(cur+1, primes :+ cur)
	        } else problemImpl(cur+1, primes)
	        
	    def sumAll(list: List[Int]) : Int = if (list.isEmpty) 0 else list.head + sumAll(list.tail)
	        
	    val l = problemImpl(3, 2 :: Nil)
	    println(l)
	    sumAll( l )
	  }
	  
	  val max = 2000000
	  println( "Sum of all primes less then " + max + " equals " + problem10( max) )
	  
}