
class MySuite extends munit.FunSuite {
  test("sort list") {
    var new_list = List(3, 2, 8, 9, -1, 0, 10, -1)
    val obtained = Tree.sort(new_list)
    val expected = List(-1, -1, 0, 2, 3, 8, 9, 10)
    assertEquals(obtained, expected)
  }
  test("sort empty list") {
    var new_list = List()
    val obtained = Tree.sort(new_list)
    val expected = List()
    assertEquals(obtained, expected)
  }
  test("size of tree") {
    var tree: Tree[Int] = Tree.Branch(Tree.Branch(Tree.Leaf(5), Tree.Leaf(4), 3), Tree.Leaf(1), 2)
    val obtained = Tree.size(tree)
    val expected = 5
    assertEquals(obtained, expected)
  }
  test("size of empty tree") {
    var tree: Tree[Int] = Tree.Empty
    val obtained = Tree.size(tree)
    val expected = 0
    assertEquals(obtained, expected)
  }
  test("list to tree") {
    var new_list = List(3, 2, 8, 9, -1, 0, 10)
    val obtained = Tree.fromList(new_list)
    val expected = Tree.Branch(Tree.Branch(Tree.Leaf(3),Tree.Leaf(8),2),Tree.Branch(Tree.Leaf(-1),Tree.Leaf(10),0),9)
    assertEquals(obtained, expected)
  }
  test("empty list to tree") {
    var new_list = List()
    val obtained = Tree.fromList(new_list)
    val expected = Tree.Empty
    assertEquals(obtained, expected)
  }
  test("tree to list") {
    var tree: Tree[Int] = Tree.Branch(Tree.Branch(Tree.Leaf(5), Tree.Leaf(4), 3), Tree.Leaf(1), 2)
    val obtained = Tree.toList(tree,List())
    val expected = List(2, 3, 5, 4, 1)
    assertEquals(obtained, expected)
  }
  test("empty tree to list") {
    var tree: Tree[Int] = Tree.Empty
    val obtained = Tree.toList(tree,List())
    val expected = List()
    assertEquals(obtained, expected)
  }
  
}
