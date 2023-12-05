import scala.io.Source
import scala.annotation.tailrec

val bag = Map(
  "red" -> 12,
  "green" -> 13,
  "blue" -> 14,
)

case class Game(id: Int, turns: List[Map[String, Int]]) {
  def isTurnPossible(in: Map[String, Int]): Boolean = in.keys.forall(k => in(k) <= bag(k))
  def isPossible: Boolean = turns.forall(isTurnPossible)

  @tailrec private def max(in: List[Int], m: Int = 1): Int = in match {
    case Nil => m
    case n::tail if n > m => max(tail, n)
    case _::tail => max(tail, m)
  }
  @tailrec private def product(in: List[Int], acc: Int = 1): Int = in match {
    case Nil => acc
    case n::tail => product(tail, n * acc)
  }

  def power: Int = product(
      bag.keys.toList.map(k => max(turns.flatMap(_.get(k))))
  ) 
}

@main def day2(input: String) = {
  val game = "^Game ([0-9]+): (.*)$".r

  val games = Source.fromFile(input).getLines.map {
    case game(id, turns) => Game(
      id.toInt,
      turns.split(";").toList.map(turn =>
        turn.split(",").map(_.trim.split(" ").toList).map {
          case n :: color :: Nil => (color, n.toInt)
          case _ => throw new IllegalArgumentException(s"could not parse turn: $turn")
        }.toMap
        )
      )
    case line => throw new IllegalArgumentException(s"could not parse game: $line")
  }.toList

  println(s"part1: ${games.filter(_.isPossible).map(_.id).sum}")
  println(s"part2: ${games.map(_.power).sum}")
}
