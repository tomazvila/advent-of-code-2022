package NoSpaceLeftOnDevice

import scala.io.Source

trait Command

case class FSCrumb(
    name: String,        // name of parent folder
    lefts: List[FSItem], // items that come before the file that we are focusing on (inside zipper)
    rights: List[FSItem] // items that come after the file we are focusing on (inside zipper)
)

trait Zipper

case class ListZipper (
    ls: List[FSItem],
    rs: List[FSItem]
) extends Zipper

case class FSZipper (
    item: FSItem,
    crumbs: List[FSCrumb]
) extends Zipper
object FSZipper:
    def nameIs(name: String, item: FSItem): Boolean = item match
        case Directory(dname, _, _) => dname == name
        case _ => false

    def fsNewFile(newItem: FSItem, zipper: FSZipper): FSZipper = zipper match
        case FSZipper(item, crumbs) => item match
            case Directory(folderName, items, _) => FSZipper(Directory(folderName, (newItem +: items), 0), crumbs)
            case _ => throw new Exception("Can't add item to File")

    def goUp(zipper: FSZipper): FSZipper = zipper match
        case FSZipper(item, crumbs) => crumbs match
            case head :: bs => FSZipper(Directory(head.name, (head.lefts :+ item) ++ head.rights), bs)
            case Nil => throw new Exception("We can't go up")
    
    def goTop(zipper: FSZipper): FSZipper = zipper match
        case FSZipper(item, crumbs) => crumbs match
            case _ :: _ => goTop(goUp(zipper))
            case Nil => zipper

    def fsTo(name: String, zipper: FSZipper): FSZipper = zipper match
        case FSZipper(item, crumbs) => item match
            case Directory(folderName, items, _) =>
                val (ls, a) = items.span(!nameIs(name, _))
                a match
                    case head :: rs => FSZipper(head, (FSCrumb(folderName, ls, rs)) +: crumbs)
                    case Nil => throw new Exception(s"We can't go to $name")
            case _ => throw new Exception("Provided zipper does not have directory as an item inside")
 
/*
- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)
*/

    def setDirectoriesSizes(zipper: FSZipper): FSZipper = zipper match
        case FSZipper(item, crumbs) => item match
            case upper@Directory(_, items, _) =>
                val (newZipper, _) = items.foldRight[(FSZipper, ListZipper)](zipper, ListZipper(List.empty, items.tail))((item, acc) => {
                    val (acc1, newItem) = item match {
                        case target@Directory(_, _, _) =>
                            val zipperForSetDirSizes = FSZipper(
                                target,
                                FSCrumb(
                                    upper.name,
                                    acc._2.ls,
                                    acc._2.rs
                                ) +: crumbs
                            )
                            val zipperWithDirSize = setDirectoriesSizes(zipperForSetDirSizes)
                            val before: Zipper = acc._1
                            val zip = FSZipper(
                                Directory(
                                    upper.name,
                                    (acc._2.rs :+ zipperWithDirSize.item) ++ acc._2.ls,
                                    acc._1.item.size + zipperWithDirSize.item.size
                                ),
                                crumbs
                            )
                            (zip, zipperWithDirSize.item)
                        case File(_, fsize) =>
                            val zip = FSZipper(
                                Directory(
                                    upper.name,
                                    (acc._2.ls :+ item) ++ acc._2.rs,
                                    acc._1.item.size + fsize
                                ),
                                crumbs
                            )
                            val newItem = item
                            (zip, newItem)
                    }
                    val acc2 = acc._2.rs match {
                        case _ :: t => ListZipper(acc._2.ls :+ newItem, t)
                        case Nil => ListZipper(acc._2.ls :+ newItem, List.empty)
                    }
                    (acc1, acc2)
                })
                newZipper
            case File(_, _) => throw new Exception("Can't calculate directory size for File")

    def countAtMost(zipper: FSZipper, atMost: Int): Int = zipper match
        case FSZipper(item, crumbs) => item match
            case dir@Directory(_, _, _) =>
                val sum = dir.fsItems.foldRight(0)((item, acc) => item match
                    case Directory(_, _, _) =>
                        countAtMost(FSZipper(item, List.empty), atMost) + acc
                    case File(_, _) => acc
                )
                val thisDirRet = if (dir.size <= atMost) dir.size else 0
                sum + thisDirRet
            case File(_, _) => 0

    def findSmallestDeletableSize(zipper: FSZipper, minToDel: Int): Int = zipper match
        case FSZipper(item, crumbs) => item match
            case dir@Directory(_, _, _) =>
                dir.fsItems.foldRight(dir.size)((item, acc) => item match
                    case Directory(_, _, size) =>
                        val best = findSmallestDeletableSize(FSZipper(item, List.empty), minToDel)
                        if (best < minToDel)
                            acc
                        else if ((best - minToDel) < (acc - minToDel))
                            best
                        else
                            acc
                    case File(_, _) => acc
                )
            case File(_, _) => throw new Exception("Can't calculate smalest deletable size for File")
    
trait FSItem:
    val size: Int
case class File(name: String, size: Int = 0) extends Command with FSItem
object File:
    val regex = "(\\d+) (\\S+)".r
case class Directory(name: String, fsItems: List[FSItem], size: Int = 0) extends Command with FSItem
object Directory:
    val regex = "dir (\\S*)".r

case class Ls() extends Command
object Ls:
    val regex = "\\$ ls".r
trait Cd extends Command
case class Up() extends Cd
object Up:
    val regex = "\\$ cd \\.\\.".r
case class Down(direction: String) extends Cd
object Down:
    val regex = "\\$ cd (\\S+)".r

object Command:
    def getCommand(str: String): Command =
        str match
            case Up.regex() => Up()
            case Down.regex(dir) => Down(dir)
            case Ls.regex() => Ls()
            case File.regex(size, name) => File(name, size.toInt)
            case Directory.regex(dir) => Directory(dir, List.empty)

case class Parser(val curDir: Directory):
    def parse(lines: Vector[String]): FSZipper =
        val withoutRoot = lines.drop(1)
        var zipper = FSZipper(Directory("/", List.empty), List.empty)
        withoutRoot.foldLeft(zipper)((zipper, line) =>
            val cmd = Command.getCommand(line)
            val ret = cmd match
                case Up() =>
                    FSZipper.goUp(zipper)
                case Down(dir) => 
                    FSZipper.fsTo(dir, zipper)
                case Ls() =>
                    zipper
                case Directory(name, _, _) =>
                    FSZipper.fsNewFile(Directory(name, List.empty), zipper)
                case File(name, size) =>
                    FSZipper.fsNewFile(File(name, size), zipper)
            ret
        )

def getData: Vector[String] =
    Source.fromResource("day7/data").getLines().toVector

@main def solve() =
    val data = getData
    val parser = Parser(curDir = Directory("/", List.empty))
    val res = parser.parse(data)
    val root = FSZipper.goTop(res)
    val withSizes = FSZipper.setDirectoriesSizes(root)
    val solution1 = FSZipper.countAtMost(withSizes, 100000)
    val freeSpace = 70000000 - withSizes.item.size
    val targetFree = 30000000
    val minimumToDelete = targetFree - freeSpace // 208860
    val best = FSZipper.findSmallestDeletableSize(withSizes, minimumToDelete)
    println("end of program")
