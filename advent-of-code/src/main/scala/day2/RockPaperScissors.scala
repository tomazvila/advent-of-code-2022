package RockPaperScissors

import scala.io.Source

sealed trait Draw:
    val value: Int
case object Rock extends Draw:
    val value: Int = 1
case object Paper extends Draw:
    val value: Int = 2
case object Scissors extends Draw:
    val value: Int = 3


sealed trait Result
case object Win extends Result
case object Lose extends Result
case object Tie extends Result

object Result:
    def parse(str: String): Option[Result] =
        str match
            case "X" => Some(Lose)
            case "Y" => Some(Tie)
            case "Z" => Some(Win)
            case _: String => None

object Draw:
    def parse(str: String): Option[Draw] =
        str match
            case "A" => Some(Rock)
            case "B" => Some(Paper)
            case "C" => Some(Scissors)
            case "X" => Some(Rock)
            case "Y" => Some(Paper)
            case "Z" => Some(Scissors)
            case _: String => None

    def compare(a: Draw, b: Draw): Result =
        (a, b) match
            case (Rock, Scissors) => Win
            case (Rock, Paper) => Lose
            case (Scissors, Rock) => Lose
            case (Scissors, Paper) => Win
            case (Paper, Rock) => Win
            case (Paper, Scissors) => Lose
            case (_, _) => Tie

case class Play2(opponent: Draw, you: Result)

object Play2:
    def parse(str: String): Option[Play2] =
        str.split(" ") match
            case Array(opp, you) =>
                val draw = (Draw.parse(opp), Result.parse(you))
                draw match
                    case (Some(a), Some(b)) => Some(Play2(a, b))
                    case _ => None
            case _ => None
    
    def getScore(play: Play2): Option[Int] =
        val draws = Vector(Rock, Paper, Scissors)
        val myDraw = draws.find(d => Draw.compare(d, play.opponent) == play.you)
        myDraw.map(d => Play.getScore(Play(play.opponent, d)))

case class Play(opponent: Draw, you: Draw)
object Play:
    def parse(str: String): Option[Play] =
        str.split(" ") match
            case Array(opp, you) =>
                val draw = (Draw.parse(opp), Draw.parse(you))
                draw match
                    case (Some(a), Some(b)) => Some(Play(a, b))
                    case _ => None
            case _ => None
 
    def getScore(play: Play): Int =
        val res = Draw.compare(play.you, play.opponent) match
            case Win => 6
            case Tie => 3
            case Lose => 0
        res + play.you.value
    
def getData: Vector[String] =
    Source.fromResource("day2/data").getLines().toVector

def parseData: Vector[Play] =
    getData.map(Play.parse).flatten

def parseData2: Vector[Play2] =
    getData.map(Play2.parse).flatten

def count: Int =
    parseData.map(Play.getScore(_)).sum

def count2: Int =
    parseData2.map(Play2.getScore(_)).flatten.sum