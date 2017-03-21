def minfree(A: Int*): Int = {
  def search(A: Array[Int], start: Int, end: Int): Int =
    A match {
      case Array() => start
      case _ => {
        val m = (start + end) / 2
        val (a1 , a2) = A.partition(_ <= m)
        if (m - start + 1 == a1.length)
          search(a2, m + 1, end)
        else
          search(a1, start, m)
      }
  }

  search(A.toArray, 0, A.length - 1)
}
