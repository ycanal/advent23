import scala.io.Source
import scala.annotation.tailrec

@tailrec
def findFirst(in: List[Char]): (Char, List[Char]) = in match {
  case Nil => throw Exception("No number found")
  case c :: tail if c >= '0' && c <= '9' => (c, c::tail)
  case 'o' :: 'n':: 'e' :: tail => ('1', in.tail)
  case 't' :: 'w':: 'o' :: tail => ('2', in.tail)
  case 't' :: 'h':: 'r' :: 'e' :: 'e' :: tail => ('3', in.tail)
  case 'f' :: 'o':: 'u' :: 'r' :: tail => ('4', in.tail)
  case 'f' :: 'i':: 'v' :: 'e' :: tail => ('5', in.tail)
  case 's' :: 'i':: 'x' :: tail => ('6', in.tail)
  case 's' :: 'e':: 'v' :: 'e' :: 'n' :: tail => ('7', in.tail)
  case 'e' :: 'i':: 'g' :: 'h' :: 't' :: tail => ('8', in.tail)
  case 'n' :: 'i':: 'n' :: 'e' :: tail => ('9', in.tail)
  case _ :: tail => findFirst(tail)
}

@tailrec
def findLast(acc: Char, in: List[Char]): Char = in match {
  case Nil => acc
  case c :: tail if c >= '0' && c <= '9' => findLast(c, tail)
  case 'o' :: 'n':: 'e' :: tail => findLast('1', in.tail)
  case 't' :: 'w':: 'o' :: tail => findLast('2', in.tail)
  case 't' :: 'h':: 'r' :: 'e' :: 'e' :: tail => findLast('3', in.tail)
  case 'f' :: 'o':: 'u' :: 'r' :: tail => findLast('4', in.tail)
  case 'f' :: 'i':: 'v' :: 'e' :: tail => findLast('5', in.tail)
  case 's' :: 'i':: 'x' :: tail => findLast('6', in.tail)
  case 's' :: 'e':: 'v' :: 'e' :: 'n' :: tail => findLast('7', in.tail)
  case 'e' :: 'i':: 'g' :: 'h' :: 't' :: tail => findLast('8', in.tail)
  case 'n' :: 'i':: 'n' :: 'e' :: tail => findLast('9', in.tail)
  case _ :: tail => findLast(acc, tail)
}

@main def day1(input: String) = {
  val result = Source.fromFile(input).getLines.map { (line: String) => 
    val (f, rest) = findFirst(line.toList)
    val l = findLast(f, rest)
    (f::l::Nil).mkString.toInt
  }.sum

  println(result)
}
