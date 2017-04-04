/**
  * Created by leijun on 21/03/2017.
  */
abstract class Node[T] {
  val left: Node[T]
  val key: T
  val right: Node[T]
  val parent: Node[T] = null
  val count: Int = 1
  val color: String = null
}

import scala.util.Random.shuffle

class BinarySearchTree[T: Ordering] {


  case class NodeA(val left: Node[T], val key: T, val right: Node[T])
    extends Node[T]

  def implicitly[T](implicit t: T) = t

  def insert(t: Node[T], k: T): Node[T] = {
    if (t == null)
      NodeA(null, k, null)
    else if (implicitly[Ordering[T]].lt(t.key, k))
      NodeA(insert(t.left, k), t.key, t.right)
    else
      NodeA(t.left, t.key, insert(t.right, k))
  }

  def map(f: T => T, t: Node[T]): Node[T] = {
    if (t == null)
      null
    else
      NodeA(map(f, t.left), f(t.key), map(f, t.right))
  }

  def toList(t: Node[T]): List[T] = {
    if (t == null)
      Nil
    else
      toList(t.left) ::: List(t.key) ::: toList(t.right)
  }

  def fromList(xs: List[T]): Node[T] = {
    shuffle(xs).foldLeft(null: Node[T])(insert(_, _))
  }

  def sort(xs: List[T]): List[T] = toList(fromList(xs))

  def lookUp(t: Node[T], k: T): Node[T] = {
    if (t == null)
      null
    else if (implicitly[Ordering[T]].eq(t.key, k))
      t
    else if (implicitly[Ordering[T]].lt(t.key, k))
      lookUp(t.left, k)
    else
      lookUp(t.right, k)
  }

  def max(t: Node[T]): Node[T] = {
    if (t == null)
      null
    else if (t.right == null)
      t
    else max(t.right)
  }

  def min(t: Node[T]): Node[T] = {
    if (t == null)
      null
    else if (t.left == null)
      t
    else max(t.left)
  }

  def succ(t: Node[T]): Node[T] = {
    if (t.right != null)
      min(t.right)
    else {
      var x = t
      var p = t.parent
      while (p != null && x == p.right) {
        x = p
        p = p.parent
      }
      p
    }
  }

  def pred(t: Node[T]): Node[T] = {
    if (t.left != null)
      max(t.left)
    else {
      var x = t
      var p = t.parent
      while (p != null && x == p.left) {
        x = p
        p = p.parent
      }
      p
    }
  }

  def delete(t: Node[T], k: T): Node[T] = {
    if (t == null)
      null
    else if (implicitly[Ordering[T]].lt(k, t.key))
      NodeA(delete(t.left, k), t.key, t.right)
    else if (implicitly[Ordering[T]].gt(k, t.key))
      NodeA(t.left, t.key, delete(t.right, k))
    else if (implicitly[Ordering[T]].eq(k, t.key) && t.left == null) {
      t.right
    }
    else if (implicitly[Ordering[T]].eq(k, t.key) && t.right == null) {
      t.left
    }
    else {
      val r_min = min(t.right)
      NodeA(t.left, r_min.key, delete(t.right, r_min.key))
    }
  }

  private def pointPosition(t: Node[T], startPoint: (Int, Int)): List[(Int, Int, T)] =
    t match {
      case null => List()
      case _ =>
        List((startPoint._1, startPoint._2, t.key)) :::
          pointPosition(t.left, (startPoint._1 - 1, startPoint._2 - 1)) :::
          pointPosition(t.right, (startPoint._1 + 1, startPoint._2 - 1))
    }

  def depict(t: Node[T]): Unit = {
    val points = pointPosition(t, (0, 0))
    val sortedPoints = points.sorted
    var flag = 0
    val tmp = ""
    println("=============================")
    /*for (point <- sortedPoints) {
      if (point._1 == flag)
        print(" " * point._2 + point._3.toString)
      else {
        flag = point._1
        println()
        print(" " * point._2 + point._3.toString)
      }
    }*/
    sortedPoints.foreach(println)
    println()
    println("=============================")
  }

}

