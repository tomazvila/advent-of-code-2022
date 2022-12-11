package SupplyStacks

import scala.io.Source
import scala.collection.mutable.Stack

case class Ship(container: Map[Int, CratesStack]):
    def getTopRow: String =
        val one = container.toVector.sortBy(_._1).map((k, v) =>
            v.stack.pop()
        )
        one.toString

object Ship:
    def parse(lines: Vector[String]): Ship =
        val withoutIndexes = lines.toList.init.toVector.reverse
        val indexed = withoutIndexes.map(line =>
            line.zip((-1) to line.length)
            .filter(_._2 % 4 == 0)
            .filter(_._1 != ' ')
            .map { case (c, index) => (c, index / 4 + 1) }
        )
        val stacks = indexed.foldLeft[Map[Int, Stack[Char]]](Map.empty)((container, line) =>
            val crates = line.foldLeft[Map[Int, Char]](Map.empty)((acc, crate) =>
                crate match
                    case (c, index) => acc + (index -> c)
            )
            crates.foldLeft[Map[Int, Stack[Char]]](container)((acc, crate) =>
                acc.get(crate._1) match
                    case None => acc + (crate._1 -> Stack(crate._2))
                    case Some(value) => acc + (crate._1 -> value.push(crate._2))
            )
        )
        val cratesStack = stacks.map((k, v) => (k -> CratesStack(v)))
        Ship(cratesStack)
        

case class CratesStack(stack: Stack[Char])

case class Command(amount: Int, from: Int, to: Int)

// move 1 from 2 to 1
object Command:
    def parse(lines: Vector[String]): Vector[Command] = 
        val pattern = "[^\\d]*([0-9]+)[^\\d]*([0-9]+)[^\\d]*([0-9]+)".r        
        val commands = lines.flatMap(line =>
            line match
                case pattern(a, b ,c) => 
                    val cmd = Command(a.toInt, b.toInt, c.toInt)
                    Some(cmd)
                case _ => None
        )
        commands
    // left is top, right is bottomk
    def move(ship: Ship, command: Command): Ship =
        val container = ship.container
        val moved = (1 to command.amount).foldLeft(container)((acc, _) =>
            val afterMoving = acc.get(command.from) match
                case None => acc
                case Some(value) => {
                    val poped = value.stack.pop()
                    acc.get(command.to) match
                        case None => acc + (command.to -> CratesStack(Stack(poped)))
                        case Some(value) => acc + (command.to -> CratesStack(value.stack.push(poped)))
                }
            afterMoving
        )
        Ship(moved)
    def moveAll(ship: Ship, command: Command): Ship =
        val container = ship.container
        val moved = container.get(command.from) match
            case None => container
            case Some(value) =>
                val removed = value.stack.take(command.amount)
                val remaining = value.stack.drop(command.amount)
                val added = container.get(command.to) match
                    case None => Map(command.to -> CratesStack(removed))
                    case Some(value) => Map(command.to -> CratesStack(removed ++ value.stack))
                container ++ added ++ Map(command.from -> CratesStack(remaining))
        Ship(moved)



def getData: Vector[String] =
    Source.fromResource("day5/data").getLines().toVector

def solveFirst: String =
    val (stacks, commands) = getData.span(!_.isEmpty)
    val ship = Ship.parse(stacks)
    val cmds = Command.parse(commands)
    cmds.foldLeft(ship)((s, command) => Command.move(s, command)).getTopRow

def solveSecond: String =
    val (stacks, commands) = getData.span(!_.isEmpty)
    val ship = Ship.parse(stacks)
    val cmds = Command.parse(commands)
    cmds.foldLeft(ship)((s, command) => Command.moveAll(s, command)).getTopRow

@main def hello() =
    val solution = solveFirst
    val second = solveSecond
    println(solution)
