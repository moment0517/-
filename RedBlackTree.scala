/**
  * Created by leijun on 23/03/2017.
  */
class RedBlackTree[T: Ordering] extends BinarySearchTree[T] {

  case class NodeC(
                    override val color: String,
                    override val left: Node[T],
                    override val key: T,
                    override val right: Node[T]
                  ) extends Node[T]

  def balance(t: Node[T]): Node[T] = t match {
    case NodeC("B", NodeC("R", NodeC("R", a, x, b), y, c), z, d) =>
      NodeC("R", NodeC("B", a, x, b), y, NodeC("B", c, z, d))
    case NodeC("B", NodeC("R", a, x, NodeC("R", b, y, c)), z, d) =>
      NodeC("R", NodeC("B", a, x, b), y, NodeC("B", c, z, d))
    case NodeC("B", a, x, NodeC("R", b, y, NodeC("R", c, z, d))) =>
      NodeC("R", NodeC("B", a, x, b), y, NodeC("B", c, z, d))
    case NodeC("B", a, x, NodeC("R", NodeC("R", b, y, c), z, d)) =>
      NodeC("R", NodeC("B", a, x, b), y, NodeC("B", c, z, d))
    case _ => t
  }

  def ins(t: Node[T], k: T): Node[T] = {
    if (t == null)
      NodeC("R", null, k, null)
    else if (implicitly(Ordering[T]).lt(k, t.key))
      balance(NodeC(t.color, ins(t.left, k), t.key, t.right))
    else
      balance(NodeC(t.color, t.left, t.key, ins(t.right, k)))
  }

  override def insert(t: Node[T], k: T): Node[T] = makeBlack(ins(t, k))

  def makeBlack(t: Node[T]): Node[T] = NodeC("B", t.left, t.key, t.right)

}


object RedBlackTree extends App {
  val rltree = new RedBlackTree[Int]
  val nodes = rltree.fromList(List(1, 2, 3, 4, 5, 6, 7, 8))
  rltree.depict(nodes)
//  println(nodes.right.key)
}