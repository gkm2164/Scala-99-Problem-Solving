import org.scalatest.FlatSpec

import co.gyeongmin.problem.Problems._

class TestCase extends FlatSpec {
  import P01._
  "P01" should "be 8" in assert(last(List(1, 1, 2, 3, 5, 8)) == 8)

  import P02._
  "P02" should "be 5" in assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)

  import P03._
  "P03" should "be 2" in assert(nth(2, List(1, 1, 2, 3, 4, 8)) == 2)

  import P04._
  "P04" should "be 6" in assert(length(List(1, 1, 2, 3, 4, 8)) == 6)

  "P05" should "be reverse" in assert(
    (reverse(List(1, 1, 2, 3, 5, 8)) zip List(8, 5, 3, 2, 1, 1))
      .forall(p => p._1 == p._2))

  "P06" should "be true" in assert(isPalindrome(List(1, 2, 3, 2, 1)))

  "P07" should "flatten list" in assert(
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8)
  )

  "P08" should "compress" in assert(
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))

  "P09" should "packing" in assert(
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  )

  "P10" should "encode things" in assert(
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  )

  "P11" should "equal to " in assert(
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  )

  "P12" should "decode things" in assert (
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  )

  "P13" should "encode direct" in assert (
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  )

  "P14" should "duplicate everything" in assert (
    duplicate(List('a, 'b, 'c, 'c, 'd))
      == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  )

  "P15" should "duplicate n times everything" in assert (
    duplicateN(3, List('a, 'b, 'c, 'c, 'd))
      == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  )

  "P16" should "drop every nth element" in assert (
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  )

  "P17" should "split into two parts" in assert (
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  )

  "P18" should "extract slice" in assert (
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == List('d, 'e, 'f, 'g)
  )

  "P19" should "rotate list" in assert(
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      &&
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  )

  "P20" should "remove kth element" in assert(
    removeAt(1, List('a, 'b, 'c, 'd))
      == (List('a, 'c, 'd),'b)
  )

  "P21" should "insert At" in assert(
    insertAt('new, 1, List('a, 'b, 'c, 'd))
      == List('a, 'new, 'b, 'c, 'd)
  )

  "P22" should "range" in assert(
    range(4, 9) == List(4, 5, 6, 7, 8, 9)
  )

  "P23" should "random select" in {
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    assert(
      randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).length == 3
    )
  }

  "P24" should "show lotto well" in {
    assert(lotto(6, 49).forall(x => x >= 1 && x <= 49))
    assert(lotto(6, 49).length == 6)
    assert(lotto(6, 49).distinct.sorted == lotto(6, 49).sorted)
  }

  "P25" should "random permute" in {
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

  "P70" should "show" in assert(
    MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
    == "afg^^c^bd^e^^^"
  )
}
