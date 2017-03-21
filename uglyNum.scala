import scala.collection.mutable.Queue

def take(n: Int): BigInt = {

  val q2 = Queue(BigInt(2))
  val q3 = Queue(BigInt(3))
  val q5 = Queue(BigInt(5))

  def get_number(
                  n: Int,
                  x: BigInt,
                  q2: Queue[BigInt],
                  q3: Queue[BigInt],
                  q5: Queue[BigInt]
                ): BigInt = {
    if (n == 1)
      x
    else {
      val xNew = q2.front min q3.front min q5.front
      xNew match {
        case q2.front => {
          q2.dequeue()
          q2.enqueue(2 * xNew)
          q3.enqueue(3 * xNew)
          q5.enqueue(5 * xNew)
        }
        case q3.front => {
          q3.dequeue()
          q3.enqueue(3 * xNew)
          q5.enqueue(5 * xNew)
        }
        case q5.front => {
          q5.dequeue()
          q5.enqueue(5 * xNew)
        }
      }
      get_number(n - 1, xNew, q2, q3, q5)
    }
  }

  get_number(n, BigInt(1), q2, q3, q5)

}
