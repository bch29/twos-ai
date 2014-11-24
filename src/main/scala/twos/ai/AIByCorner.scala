package twos.ai

import twos.game._

abstract class AIByCorner(depth: Int, maxChecks: Int = 4)
  extends TwosAI(depth, maxChecks) {

  // The weightings placed on each goodness metric, respectively.
  // The higher the value, the more the algorithm takes the
  // associated metric into consideration.
  def NeighbourWeight: Float
  def ClosenessWeight: Float
  def FullnessWeight: Float

  def getNextFromCorner(corner: Int, where: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val (i, j) = where
    corner match {
      case 0 => ((i + 1, j), (i, j + 1))
      case 1 => ((i - 1, j), (i, j + 1))
      case 2 => ((i - 1, j), (i, j - 1))
      case 3 => ((i + 1, j), (i, j - 1))
    }
  }

  def closestNeighbour2(state: GameState, i: Int): Float = {
    val d = state.closestNeighbour(i)
    if (d == -1) 0 else d
  }

  def getNeighbourScore(state: GameState, where: (Int, Int), corner: Int): Float = {
    val (w1, w2) = getNextFromCorner(corner, where)
    val p = state.getSquare(where)
    val p1 = if (state.squareExists(w1)) state.getSquare(w1) else p
    val p2 = if (state.squareExists(w2)) state.getSquare(w2) else p
    val mod = p * (p1 + p2)
    val a = p1 - p
    val b = p2 - p
    (if (a < 0) a * (-1) else a * mod) + (if (b < 0) b * (-1) else b * mod)
  }

  def getStateBadness(state: GameState): Float = {
    val byNeighbours =
      if (NeighbourWeight != 0)
        (for {
          i <- 0 until state.size
          j <- 0 until state.size
        } yield getNeighbourScore(state, (i, j), 2)).sum
      else 0
    val byFull = (state.size * state.size) - state.empties.size
    val nonEmpties = state.nonEmpties
    val byCloseness = (nonEmpties map (closestNeighbour2(state, _))).sum / nonEmpties.size.toFloat

    byNeighbours * NeighbourWeight + byCloseness * ClosenessWeight + byFull * FullnessWeight
  }
}