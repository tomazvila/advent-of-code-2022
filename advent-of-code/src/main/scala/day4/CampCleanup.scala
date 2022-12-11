package CampCleanup

import scala.io.Source

case class CleanupRange(r: Vector[Int])
object CleanupRange:
    def parse(str: String): Option[CleanupRange] =
        str.split("-") match
            case Array(lower, upper) =>
                val l = lower.toIntOption
                val u = upper.toIntOption
                (l, u) match
                    case (Some(l), Some(u)) => 
                        val r = (l to u).toVector
                        Some(CleanupRange(r))
                    case _ =>
                        None
            case _ =>
                None

case class Elf(cr: CleanupRange)
object Elf:
    def parse(str: String): Option[Elf] =
        CleanupRange.parse(str).map(Elf(_))

case class ElfPair(a: Elf, b: Elf)
object ElfPair:
    def parse(str: String): Option[ElfPair] =
        str.split(",") match
            case Array(a, b) => 
                (Elf.parse(a), Elf.parse(b)) match
                    case (Some(aa), Some(bb)) => Some(ElfPair(aa, bb))
                    case _ => None                
            case _ => None
    def fullyOverlaps(ep: ElfPair): Boolean =
        val ab = ep.a.cr.r.containsSlice(ep.b.cr.r)
        val bd = ep.b.cr.r.containsSlice(ep.a.cr.r)
        (ab || bd)

    def partialyOverlaps(ep: ElfPair): Boolean =
        ep.a.cr.r.intersect(ep.b.cr.r).length > 0

def getData: Vector[String] =
    Source.fromResource("day4/data").getLines().toVector

def solveFirst: Int =
    getData.flatMap(ElfPair.parse).filter(ElfPair.fullyOverlaps).length

def solveSecond: Int =
    getData.flatMap(ElfPair.parse).filter(ElfPair.partialyOverlaps).length
