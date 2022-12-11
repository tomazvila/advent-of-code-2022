package CalorieCounting

import scala.io.Source

case class Elf(items: Vector[Int])

object Elf:
    def emptyElf: Elf = Elf(Vector.empty)
/*List.unfold[Vector[Int], Vector[Int]](vec) { state: Vector[Int] =>
    if (state.nonEmpty) {
      val stripped = state.dropWhile(_ == 0)
      val (x, y: Vector[Int]) = stripped.span(_ > 0)
      x match {
        case _ +: _ => Some((x, y))
        case _ => None
      }
    } else {
      None
    }
  }*/
    // def parseData2(data: Vector[String]): Vector[Vector[Option[Int]]] =
    //     val a = data.map(x =>
    //         if (x.nonEmpty) 
    //             x.toIntOption
    //         else
    //             None
    //     )
    //     List.unfold[Vector[Option[Int]], Vector[Option[Int]]](a) { (state: Vector[Option[Int]]) => {
    //             if (state.nonEmpty)
    //                 val (x, y) = state.dropWhile(_.isEmpty).span(_.isDefined)
    //                 x match
    //                     case _ +: _ => Some((x, y))
    //                     case _ => None
    //             else
    //                 None
    //         }
    //     }
        
    def parseData(data: Vector[String]): Vector[Elf] =
        val (elfs, _) = data.map(x =>
            if (x.nonEmpty) 
                x.toIntOption
            else
                None
        ).foldLeft[(Vector[Elf], Elf)]((Vector.empty, Elf.emptyElf))((acc, opt) =>
            acc match
                case (elfs, curElf) => 
                    opt match
                        case Some(item) => (elfs, Elf(curElf.items :+ item))                 
                        case None       => (elfs :+ curElf, Elf.emptyElf)
        )
        elfs
    
    def mostCalories(elfs: Vector[Elf]): Int =
        elfs.map(_.items).map(_.sum).max

    def topThree(elfs: Vector[Elf]): Int =
        val sums = elfs.map(_.items).map(_.sum)
        val topThree = sums.sortWith(_ > _)
        topThree.take(3).sum

def getData: Vector[String] =
    Source.fromResource("day1/data").getLines().toVector

def solve: Int =
    val data = getData
    val parsed = Elf.parseData(data)
    val answer = Elf.mostCalories(parsed)
    answer

def solveForThree: Int =
    val data = getData
    val parsed = Elf.parseData(data)
    val answer = Elf.topThree(parsed)
    answer