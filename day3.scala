import scala.io.Source
import scala.annotation.tailrec

case class Gear(n: Int, line: Int, startColumn: Int, endColumn: Int) {
  def isSymbol(s: Char): Boolean = s != '.'
  def rangeContainsChar(s: Array[Char]): Boolean = {
    val slice = s.slice(Math.max(0, startColumn - 1), Math.min(s.length, endColumn + 2))
    slice.exists(isSymbol)
  }
  def isTouching(schematic: Array[Array[Char]]): Boolean = {
    (if (line <= 0) { false } else { rangeContainsChar(schematic(line - 1)) }) ||
    (if (startColumn <= 0) { false } else { isSymbol(schematic(line)(startColumn - 1)) }) ||
    (if (endColumn >= schematic(line).length - 1) { false } else { isSymbol(schematic(line)(endColumn + 1)) }) ||
    (if (line >= schematic.length - 1) { false } else { rangeContainsChar(schematic(line + 1)) })
  }
}

def gearsForLine(line: Int, input: Array[Char]): List[Gear] = {
  def newGear(acc: List[Char], line: Int, column: Int): Gear = Gear(acc.reverse.mkString.toInt, line, column - acc.length, column - 1)

  @tailrec def rec(column: Int, in: List[Char], acc: List[Char], result: List[Gear]): List[Gear] = (in, acc) match {
    case (Nil, Nil) => result
    case (Nil, l) => (newGear(l, line, column)::result)
    case (c::tail, l) if c >= '0' && c <= '9' => rec(column+1, tail, c::l, result)
    case (_::tail, Nil) => rec(column+1, tail, Nil, result)
    case (_::tail, l) => rec(column+1, tail, Nil, newGear(l, line, column)::result)
  }

  rec(0, input.toList, Nil, Nil).reverse
}

@main def day3(input: String) = {
  val schematic: Array[Array[Char]] = Source.fromFile(input).getLines.map {
    (line: String) => line.toList.toArray
  }.toArray

  val pieces: List[Gear] = schematic.toList.zipWithIndex.flatMap { case (l, i) => gearsForLine(i, l) }
  val touching: List[Gear] = pieces.filter(_.isTouching(schematic))

  val res1 = touching.map(_.n).sum

  println(s"part1 = $res1")
}
