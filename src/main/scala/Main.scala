enum Tree[+A]:
  case Empty
  case Branch(left: Tree[A], right: Tree[A], key: A)
  case Leaf(a: A)

object Tree:
  case class BSTree private[Tree] (l: Option[BSTree], r: Option[BSTree], v: Option[Int])

  def size[A](tree: Tree[A]): Int = {
    tree match
      case Empty => 0
      case Leaf(a) => 1
      case Branch(left, right, v) => size(left) + size(right) + 1
  }

  private def makeBstFromList(xs: List[Int], start: Int, end: Int): Option[BSTree] = {
    if (start > end){
      None
    }
    else{
      val middle = (start + end) / 2
      Option(BSTree(makeBstFromList(xs, start, middle - 1), makeBstFromList(xs, middle + 1, end), Option(xs(middle))))
    }
  }

  def toList(subtree: Tree[Int], buffer:List[Int]): List[Int] ={
    subtree match
      case Empty => List()
      case Leaf(value) => buffer :+ value
      case Branch(left, right, key) => {
        key +: List.concat(toList(left, buffer), toList(right, buffer))
      }
  }

  def fromList(xs:List[Int], start: Int, end: Int): Tree[Int] = {
    if (start > end){
      Empty
    }
    else if(start == end){
      Leaf(xs(start))
    }
    else{
      val middle = (start + end) / 2
      Branch(fromList(xs, start, middle - 1), fromList(xs, middle + 1, end), xs(middle))
    }
  }

  def makeBst(tree: Tree[Int]): BSTree = {
    val resList:List[Int] = toList(tree, List()).sortWith(_ < _)
    makeBstFromList(resList, 0, resList.size - 1).get
  }

  def BstToList(bstTree: Option[BSTree], buffer: List[Int]): List[Int] = {
    bstTree match
      case None => List()
      case Some(l, r, v) => {
        List.concat(BstToList(l, buffer):+v.get, BstToList(r, buffer))
      }
  }

  def sort(xs: List[Int]): List[Int]= {
    if(xs.isEmpty){
      xs
    }
    else {
      val tree = fromList(xs, 0, xs.size - 1)
      val bst_tree = makeBst(tree)
      BstToList(Option(bst_tree), List())
    }
  }
