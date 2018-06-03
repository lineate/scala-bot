package abc

import com.lineate.xonix.mind.model._

import scala.collection.mutable

object SBot extends Bot {

  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))
  var m = 0
  var n = 0
  var id = 0

  override def getName: String = "Hello, I'm scala bot!"

  override def move(gs: GameState): Move = {
    m = gs.field.length
    n = gs.field.head.length
    id = gs.botId
    val me = findMe(gs)
    Move.STOP
  }

  def findMe(gs: GameState): Seq[Point] = {
    val tail = Cell.tail(id)
    val me = mutable.Buffer(gs.head)
    // current point
    var cp = me.headOption
    while (cp.isDefined) {
      // seek for lower letter around until not found
      val p = cp.get
      val po = neigh.map { it ⇒
        val i = (p.getRow + it._1).bound(0, m - 1)
        val j = (p.getCol + it._2).bound(0, n - 1)
        Point.of(i, j)
      }.find { it ⇒
        !me.contains(it) && gs.field(it.getRow)(it.getCol) == tail
      }
      po.foreach { nt ⇒
        me.insert(0, nt)
      }
      cp = po
    }
    me
  }

  implicit class RichInt(it: Int) {
    def bound(l: Int, u: Int): Int = {
      if (it < l)  l
      else if (it > u) u
      else it
    }
  }

}
