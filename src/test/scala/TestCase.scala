import org.scalatest.FlatSpec

import co.gyeongmin.problem.Problems._

class TestCase extends FlatSpec {
  "P01" should "be 8" in {
    import P01._
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

  "P02" should "be 5" in {
    import P02._
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  }

  "P03" should "be 2" in {
    import P03._
    assert(nth(2, List(1, 1, 2, 3, 4, 8)) == 2)
  }

  "P04" should "be 6" in {
    import P04._
    assert(length(List(1, 1, 2, 3, 4, 8)) == 6)
  }

  "P05" should "be reverse" in {
    import P05._
    assert(
      reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1)
    )
  }

  "P06" should "be true" in {
    import P06._
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
  }

  "P07" should "flatten list" in {
    import P07._
    assert(
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8)
    )
  }

  "P08" should "compress" in {
    import P08._
    assert(
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  "P09" should "packing" in {
    import P09._
    assert(
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    )
  }

  "P10" should "encode things" in {
    import P10._
    assert(
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    )
  }

  "P11" should "equal to " in {
    import P11._
    assert(
      encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    )
  }

  "P12" should "decode things" in {
    import P12._
    assert (
      decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
        == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    )
  }

  "P13" should "encode direct" in {
    import P13._
    assert (
      encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    )
  }

  "P14" should "duplicate everything" in {
    import P14._

    assert (
      duplicate(List('a, 'b, 'c, 'c, 'd))
        == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    )
  }

  "P15" should "duplicate n times everything" in {
    import P15._

    assert (
      duplicateN(3, List('a, 'b, 'c, 'c, 'd))
        == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    )
  }

  "P16" should "drop every nth element" in {
    import P16._

    assert (
      drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    )
  }

  "P17" should "split into two parts" in {
    import P17._

    assert (
      split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
  }

  "P18" should "extract slice" in {
    import P18._
    assert (
      slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        == List('d, 'e, 'f, 'g)
    )
  }

  "P19" should "rotate list" in {
    import P19._
    assert(
      rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
        &&
        rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
          == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    )
  }

  "P20" should "remove kth element" in {
    import P20._
    assert(
      removeAt(1, List('a, 'b, 'c, 'd))
        == (List('a, 'c, 'd),'b)
    )
  }

  "P21" should "insert At" in {
    import P21._
    assert(
      insertAt('new, 1, List('a, 'b, 'c, 'd))
        == List('a, 'new, 'b, 'c, 'd)
    )
  }

  "P22" should "range" in {
    import P22._
    assert(
      range(4, 9) == List(4, 5, 6, 7, 8, 9)
    )
  }

  "P23" should "random select" in {
    import P23._
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    assert(
      randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).length == 3
    )
  }

  "P24" should "show lotto well" in {
    import P24._
    assert(lotto(6, 49).forall(x => x >= 1 && x <= 49))
    assert(lotto(6, 49).length == 6)
    assert(lotto(6, 49).distinct.sorted == lotto(6, 49).sorted)
  }

  "P25" should "random permute" in {
    import P25._
    val testInput = List('a, 'b, 'c, 'd, 'e, 'f)
    val rp = randomPermute(testInput)
    println(rp)
    assert(rp != testInput)
  }

  "P26" should "make combinations" in {
    import P26._

    println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
    assert(
      combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)).length == 20
    )
  }

  "P27, P28" should "group 3 items" in {
    import P27._
    import P28._

    val testSet = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")

    assert(group3(testSet) diff group(List(2, 3, 4), testSet) isEmpty)
  }

  "P29, P30" should "sort items well" in {
    import P29._
    import P30._

    val testSet = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))

    val lsortResult = lsort(testSet)
    val lsortFreqResult = lsortFreq(testSet)

    assert((lsortResult zip lsortResult.tail).forall {
      case (left, right) => left.length <= right.length
    })

    assert((lsortFreqResult zip lsortFreqResult.tail).forall {
      case (left, right) => left.length >= right.length
    })
  }

  def isPrime(n: Int): Boolean = {
    !(2 to Math.sqrt(n).toInt).exists(n % _ == 0)
  }

  def primes(n: Int): Stream[Int] = if (isPrime(n)) n #:: primes(n + 1) else primes(n + 1)
  val infPrimes = primes(2)

  "P31" should s"show primes ${infPrimes.take(10).toList.mkString(", ")} to be true" in {
    import P31._

    assert(infPrimes.take(10).forall(_.isPrime))
  }

  "P32" should "show gcd(36, 63) == 9" in {
    import P32._

    assert(gcd(36, 63) == 9)
  }

  "P33" should "35 is coprime to 64" in {
    import P33._

    35.isCoprimeTo(64)
  }

  "P34" should "shows totient" in {
    import P34._

    assert(10.totient == 4)
  }

  "P35" should "show prime factors" in {
    import P35._

    assert(315.primeFactors == List(3, 3, 5, 7))
  }

  "P36" should "primeFactorMultiplicity" in {
    import P36._
    val result = 315.primeFactorMultiplicity
    val answer = Map(3 -> 2, 5 -> 1, 7 -> 1)
    assert(result.keys.forall(key => result(key) == answer(key)))
  }

  "P37, P38" should "Show totient number" in {

    val x = {
      import P37._
      10090.totient
    }

    val y = {
      import P34._
      10090.totient
    }

    println(x, y)

    println(x == y)

  }



  "P39" should "show range" in {
    import P39._
    assert(listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  "P40" should "show goldbachity" in {
    import P40._
    assert(28.goldbach == (5, 23))
  }

  "P41" should "show goldbach compositions well" in {
    import P41._

    printGoldbachList(9 to 20)
    printGoldbachList(1 to 2000, 50)
  }

  "P46" should "show truth table" in {
    import Logics._
    import P46._

    table2((a, b) => and(a, or(a, b)))
  }

  "P49" should "gray code" in {
    import P49._

    assert(gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  "P50" should "show huffman code" in {
    import P50._
    val inputs = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    val output = List(("a", "0"), ("b", "101"), ("c","100"), ("d","111"), ("e","1101"), ("f", "1100"))
    assert(huffman(inputs).sortBy(_._1) == output.sortBy(_._1))
  }

  "P70" should "show" in assert(
    MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
    == "afg^^c^bd^e^^^"
  )
}
