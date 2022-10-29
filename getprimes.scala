object main {

def main(args: Array[String]) = {
    
    def getPrimes (n: Int): List[Int] = {

        def isPrime(k: Int): Boolean = {
            (2 until k).forall(x => k % x != 0)
        }
        
        def acc(number: Int, list: List[Int]): List[Int] = { number match 
            case 0 => list
            case _ => if (isPrime(number)) then acc(number -1, list :+ number) else acc(number -1, list)
        }

        lazy val acc2: List[Int] = (1 to n).toList.filter(isPrime(_))

        acc(n, List()).reverse
        //acc2
    }

    if (args.isEmpty) then print(getPrimes(100)) else print(getPrimes(args(0).toInt))
    }
}