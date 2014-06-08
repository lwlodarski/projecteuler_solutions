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
		  def problem12(required:Int): Long = {
		    def divisors(no:Long, cur:Long) : Int = if (cur>no/2) 2 else if (no%cur==0) divisors(no, cur+1) +1 else divisors(no, cur+1)
		    
		    def triangle(cur:Long, next:Long) : Long = 
		      {
		    		val d = divisors(cur, 2)
		    		println("triangle no "+cur + " has " +d + " divisors")
		    		if (d > required) cur else triangle(cur+next, next+1)
		      }
		    
		    triangle(1, 2)
		  }
	    
		  val startTime = System.currentTimeMillis()
		  println( "Triangle number with over 500 divisors equals " + problem12(500) )
		  val endTime = System.currentTimeMillis()
	      val dur = endTime - startTime
	      println(dur + " msecs")
	  }
	  
	  main_problem12
	  
	  
}