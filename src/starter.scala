object starter extends App {
  
	  //sum of all prime number less than max
	  def problem10(max: Long) : Long = {
	    def isPrime(no: Long, primes: List[Long], sqr: Long) : Boolean = if (primes.isEmpty || primes.head>sqr) true else if (no % primes.head==0) false else isPrime(no, primes.tail, sqr)
	    
	    def problemImpl(cur: Long, primes: List[Long]) : List[Long] = 
	      if (cur>=max) primes else
	        if (isPrime(cur, primes, Math.sqrt(cur).toLong)) {
	          problemImpl(cur+1, primes :+ cur)
	        } else problemImpl(cur+1, primes)
	        
	    def sumAll(list: List[Long], result:Long) : Long = if (list.isEmpty) result else sumAll(list.tail, result + list.head)
	        
	    val l = problemImpl(3, 2 :: Nil)
	    println("All elements count = " + l.length)
	    sumAll( l, 0 )
	  }
	  
	  def main_problem10 = {
		  val max = 2000000
		  val startTime = System.currentTimeMillis()
		  println( "Sum of all primes less then " + max + " equals " + problem10( max) )
		  val endTime = System.currentTimeMillis()
	      val dur = endTime - startTime
	      println(dur + " msecs")
	  }
	  
	  def problem11(data: Array[ Array[Int]], width:Int, height:Int, length:Int) : Long = {
	    def max(a: Long, b: Long) = if (a>b) a else b
	    
	    def vertical(x: Int, y:Int, l: Int): Long = if (l==0) 1 else if (x>=width || y>=height) 0 else data(x)(y) * vertical(x, y+1, l-1) 
	    
	    def horizontal(x: Int, y:Int, l: Int): Long = if (l==0) 1 else if (x>=width || y>=height) 0 else data(x)(y) * horizontal(x+1, y, l-1)
	    
	    def diagonal1(x: Int, y:Int, l: Int): Long = if (l==0) 1 else if (x>=width|| y>=height) 0 else data(x)(y) * diagonal1(x+1, y+1, l-1)
	    def diagonal2(x: Int, y:Int, l: Int): Long = if (l==0) 1 else if (x<0 || x>=width || y>=height) 0 else data(x)(y) * diagonal2(x-1, y+1, l-1)
	    
		def problem11Impl(x: Int, y: Int, length: Int): Long = 
		{	
	     val cur =  max( max( vertical(x, y, length), horizontal(x,y, length)), max(diagonal1(x, y, length), diagonal2(x,y, length)) )
	     if (x<width)
	       max(cur, problem11Impl(x+1, y, length))
	     else
	     if (y<height)
	       max(cur, problem11Impl(0, y+1, length))
	     else
	       0
		}
		
		problem11Impl(0, 0, 4)
	  }
	  
	  def main_problem11 = {
		  val startTime = System.currentTimeMillis()
		  println( "Largest product equals " + problem11(Problem11.Data, 20, 20, 4) )
		  val endTime = System.currentTimeMillis()
	      val dur = endTime - startTime
	      println(dur + " msecs")
	  }
	  
	  def main_problem12 = {
	      def divisorsBruteForce(no:Long, cur:Long) : Int = if (cur>=no) 1 else if (no%cur==0) divisorsBruteForce(no, cur+1) +1 else divisorsBruteForce(no, cur+1)
		  def problem12(required:Int): Long = {
		    
		    def triangle(cur:Long, next:Long) : Long = 
		      {
		    		val d = divisorsBruteForce(cur, 2)
		    		println("triangle no "+cur + " has " +d + " divisors")
		    		if (d > required) cur else triangle(cur+next, next+1)
		      }
		    
		    triangle(1, 2)
		  }
		  
		  def fast12(required:Int): Long = {
		    def isPrime(no: Long, primes: List[Long], sqr: Long) : Boolean = if (primes.isEmpty || primes.head>sqr) true else if (no % primes.head==0) false else isPrime(no, primes.tail, sqr)
		    
		    def divisors(no:Long, primes:List[Long], result:Int, prev:Int, level:Int, factors:List[Long]) : (Int, List[Long]) =
		    {
		      if (no==1)
		        (result, factors)
		      else
		      if (no%primes.head==0)
		      {
		        if (factors.isEmpty==false && primes.head==factors.head)
		        {
		        	divisors(no/primes.head, primes, result + prev, prev, level, primes.head :: factors)
		        } else {
		          divisors(no/primes.head, primes, result *2, result, level, primes.head :: factors)
		        }
		      } else
		        divisors(no, primes.tail, result , prev, level+1, factors)
		    }
		        
		    
		    def triangle(cur:Long, next:Long, maxChecked:Long, primes: List[Long]) : Long = 
		      {
		    		def genPrimes(no: Long, primes:List[Long]) :(Long, List[Long]) = 
		    		  if (no>cur) 
		    		    (no, primes) 
		    		  else 
		    		  if (isPrime(no, primes, Math.sqrt(no).toLong)) 
		    		    genPrimes(no+1, primes :+ no)
		    		  else 
		    		    genPrimes(no+1, primes)
		    		val newPrimes = genPrimes(maxChecked, primes)
		    		val d = divisors(cur, newPrimes._2, 1, 1, 1, 1 :: Nil)
		    		//val brute  = divisorsBruteForce(cur, 1)
		    		//if (brute!=d._1)
		    		//{
		    		//  println(d._2)
		    		//  throw new Exception("I'm wrong "+d._1 + " vs BF " + brute)
		    		//}
		    		//println("triangle no "+cur + " has " +d._1 + " divisors")
		    		if (d._1 > required) cur else triangle(cur+next, next+1, newPrimes._1, newPrimes._2)
		      }
		    
		    triangle(1, 2, 3, 2 :: Nil)
		  }
	    
		  val startTime = System.currentTimeMillis()
		  val max = 500
		  val fastMethod = fast12(max)
		  //val bruteForceMethod = problem12(max)
		  //if (fastMethod != bruteForceMethod)
		  //  println("Error")
		  //else
		    println("OK")
		  println( "Triangle number with over 500 divisors equals " + fastMethod )
		  //println( "Triangle number with over 500 divisors equals " + bruteForceMethod )
		  val endTime = System.currentTimeMillis()
	      val dur = endTime - startTime
	      println(dur + " msecs")
	  }
	  
	  main_problem12
	  
	  
}