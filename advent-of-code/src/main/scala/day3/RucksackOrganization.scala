package RucksackOrganization

import scala.io.Source

case class Item(priority: Int)
object Item:
    val priorities = ((('a' to 'z') zip (LazyList.from(1))) ++ (('A' to 'Z') zip (LazyList.from(27)))).toMap

    def parse(letter: Char): Option[Item] =
        priorities.get(letter).map(Item(_))

case class Compartment(items: Vector[Item])

object Compartment:
    def parse(str: String): Compartment =
        val items = str.flatMap(Item.parse(_)).toVector
        Compartment(items)

case class RucksackGroup(
    group: (Rucksack, Rucksack, Rucksack)
)
object RucksackGroup:
    def parse(rucksacks: Vector[Rucksack]): Vector[RucksackGroup] =
        rucksacks.grouped(3).toVector.filter(_.length == 3).flatMap {
            case Vector(a, b, c) => Some(RucksackGroup(a, b, c))
            case _ => None
        }
    
    def findBadges(rucksackGroup: RucksackGroup): Int =
        val seta = (rucksackGroup.group._1.a.items ++ rucksackGroup.group._1.b.items).toSet.toVector
        val setb = (rucksackGroup.group._2.a.items ++ rucksackGroup.group._2.b.items).toSet.toVector
        val setc = (rucksackGroup.group._3.a.items ++ rucksackGroup.group._3.b.items).toSet.toVector
        seta.intersect(setb).intersect(setc).map(_.priority).sum

case class Rucksack(a: Compartment, b: Compartment)

object Rucksack:
    def parse(str: String): Option[Rucksack] =
        if (str.length % 2 != 0)
            None
        else
            val (a, b) = str.splitAt(str.length / 2)
            val x = Compartment.parse(a)
            val y = Compartment.parse(b)
            Some(Rucksack(x, y))
    
    def findTheSame(rucksack: Rucksack): Int =
        val aset = rucksack.a.items.toSet
        val bset = rucksack.b.items.toSet
        aset.filter(aitem => bset.contains(aitem)).map(_.priority).sum

def getData: Vector[String] =
    Source.fromResource("day3/data").getLines().toVector

def solvePartOne: Int =
    getData.flatMap(Rucksack.parse).map(Rucksack.findTheSame).sum

def solvePartTwo: Int =
    val rucksacs = getData.flatMap(Rucksack.parse)
    RucksackGroup.parse(rucksacs).map(RucksackGroup.findBadges).sum
