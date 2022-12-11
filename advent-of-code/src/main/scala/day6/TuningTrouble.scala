package TuningTrouble

import scala.io.Source

def getData: String =
    Source.fromResource("day6/data").getLines().toVector.head

def solve(str: String, distinctChars: Int): Option[Int] =
    str.sliding(distinctChars, 1).toVector.zip(LazyList.from(distinctChars)).find { case (code, index) => code.distinct.length == distinctChars }.map(_._2)

@main def hello() =
    val data = getData
    solve(data, 4) match
        case None => println("Failed to solve")
        case Some(value) => println(s"first solve ${value}")
    solve(data, 14) match
        case None => println("Failed to solve")
        case Some(value) => println(s"second solve ${value}")

