package week1

object Week1 {
  import util.Random;
  def main(args: Array[String]) {
    // println(Last(List(1, 1, 2, 3, 5, 8)))
    // println(penultimate(List(1, 1, 2, 3, 5, 8)))
    // println(nth(2, List(1, 1, 2, 3, 5, 8)))
    println(length(List(1, 1, 2, 3, 5, 8)))
    // println(reverse(List(1, 1, 2, 3, 5, 8)))
    //  println(isPalindrome(List(1, 2, 3, 2, 1)))
    // println( flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    //println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    // println(encode2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //println(duplicate(List('a, 'b, 'c, 'c, 'd)))
    // println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
    // println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    // println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    // println(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    // println(removeAt(1, List('a, 'b, 'c, 'd)))
    //  println(insertAt('new, 1, List('a, 'b, 'c, 'd)))
    //println(range(4, 9))
    //println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    // println(lotto(6, 49))
    // println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
  }

  def Last(list: List[Int]): Int = {
    if (list.length == 1) list.head
    else Last(list.tail)
  }

  def penultimate(list: List[Int]): Int = {
    if (list.length == 2) {
      list.head
    } else penultimate(list.tail)
  }

  def nth(n: Int, list: List[Int]): Int = {
    def traverse(list: List[Int], count: Int): Int = {
      if (count == n) list.head
      else
        traverse(list.tail, count + 1)
    }
    traverse(list, 0)
  }

  def length(list: List[Int]): Int = {
    def traverse(count: Int, list: List[Int]): Int = {
      try {
        traverse(count + 1, list.tail)
      }
      catch{
        case e: Exception=> count
      }
    }
    traverse(0, list)
  }

  def reverse[A](list: List[A]): List[A] = {

    def traverse[A](list: List[A], stack: List[A]): List[A] = {
      if (list.length == 0) {
        stack
      } else {
        traverse(list.tail, list.head :: stack)
      }
    }
    traverse(list, List())
  }

  def isPalindrome(list: List[Int]): Boolean = (list == reverse(list))

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  def compress(list: List[Symbol]): List[Symbol] = {

    def destruct(result: List[Symbol], char: Symbol, list: List[Symbol]): List[Symbol] = {
      if (list.length == 0) result
      else if (list.head == char)
        destruct(result, char, list.tail)
      else
        destruct(result ::: List(list.head), list.head, list.tail)
    }
    destruct(List(), '_, list)
  }

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[A](ls: List[A]): List[(Int, A)] = {
    def compress(char: A, count: Int, result: List[(Int, A)], current: List[A]): List[(Int, A)] = {
      if (current.length == 0) (count, char) :: result
      else if (current.head == char)
        compress(char, count + 1, result, current.tail)
      else
        compress(current.head, 1, (count, char) :: result, current.tail)
    }

    reverse(compress(ls.head, 0, List(), ls))
  }

  def encode2[A](ls: List[A]): List[Any] = {
    encode(ls) map {
      t => if (t._1 == 1) t._2 else t
    }
  }

  def duplicate[A](ls: List[A]): List[A] = {
    def copy[A](ls: List[A], result: List[A]): List[A] = {
      if (ls.length == 0) result
      else
        copy(ls.tail, ls.head :: ls.head :: result)
    }
    reverse(copy(ls, List()))
  }

  def duplicateN[A](N: Int, ls: List[A]): List[A] = {

    def copy[A](current: Int, ls: List[A], result: List[A]): List[A] = {
      if (ls.length == 0) result
      else {
        if (current < N - 1) {
          copy(current + 1, ls, ls.head :: result)
        } else
          copy(0, ls.tail, ls.head :: result)
      }
    }
    reverse(copy(0, ls, List()))
  }

  def drop[A](N: Int, ls: List[A]): List[A] = {
    def copy[A](current: Int, ls: List[A], result: List[A]): List[A] = {
      if (ls.length == 0) result
      else {
        if (current < N - 1) {
          copy(current + 1, ls.tail, ls.head :: result)
        } else
          copy(0, ls.tail, result)
      }
    }
    reverse(copy(0, ls, List()))
  }
  def split[A](N: Int, ls: List[A]): List[Any] = {
    def copy[A](current: Int, ls: List[A], result: List[A], result2: List[A]): List[Any] = {
      if (ls.length == 0) List(reverse(result), reverse(result2))
      else {
        if (current < N) {
          copy(current + 1, ls.tail, ls.head :: result, result2)
        } else
          copy(current + 1, ls.tail, result, ls.head :: result2)
      }
    }
    copy(0, ls, List(), List())
  }

  def slice[A](N: Int, Z: Int, ls: List[A]): List[Any] = {
    def cut[A](current: Int, ls: List[A], result: List[A]): List[A] = {
      if (current == Z) result
      else {
        if (current < N)
          cut(current + 1, ls.tail, result)
        else
          cut(current + 1, ls.tail, ls.head :: result)
      }
    }
    reverse(cut(0, ls, List()))
  }
  def rotate[A](N: Int, ls: List[A]): List[Any] = {
    val n = if (N == 0) 0 else N % ls.length
    if (n < 0) rotate(n + ls.length, ls)
    else
      (ls drop n) ::: (ls take n)
  }

  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post) => (pre ::: post, e)
    case (pre, Nil) => throw new NoSuchElementException
  }

  def insertAt[A](add: A, N: Int, ls: List[A]): List[Any] = {
    if (ls.length == 0) ls
    def add2[A](count: Int, ls: List[A], result: List[A]): List[Any] = {
      if (ls.length == 0) reverse(result)
      else {
        if (count == N) {
          add2(count + 1, ls.tail, ls.head :: add :: result)
        } else
          add2(count + 1, ls.tail, ls.head :: result)
      }
    }
    add2(0, ls, List())
  }
  def range(N: Int, Z: Int) = {
    def construct(n: List[Int]): List[Int] = {
      if (n.length == (Z - N + 1)) n
      else
        construct((N + n.length) :: n)
    }
    reverse(construct(List()))
  }
  def randomSelect[A](N: Int, ls: List[A]) = {
    val rs = new Random();
    val original = ls.length
    def destroy(count: Int, ls: List[A], result: List[A]): List[A] = {
      if (count == N) result
      else {
        val (list, removed) = removeAt(rs.nextInt(ls.length), ls)
        destroy(count + 1, list, removed :: result)
      }
    }
    destroy(0, ls, List())
  }

  def lotto(num: Int, range: Int) = {
    randomSelect(num, List.range(1, range + 1))
  }
  def randomPermute[A](ls: List[A]) = {
    randomSelect(ls.length, ls)
  }
  /*
  def combinations[A](n: Int, ls: List[A]): List[Any] = {
    def concat[A](count: Int, ls: List[A], result: List[Any], incomplete: List[A]): List[Any] = {
      if (ls.length == 0) result
      else {
    	  if(count != n){
    	    
    	    concat(count+1, ls.tail, result,ls.head::incomplete)
    	  }
      }
    }
    ls
  }*/
}