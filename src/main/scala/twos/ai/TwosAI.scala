package twos.ai

import twos.game._

abstract class TwosAI(depth: Int, maxChecks: Int = 4) {

  def getStateBadness(state: GameState): Float

  /**
   * Returns all states the board could be in after a random new square
   * appears. The first part of the tuple contains the states in which a
   * two has appeared, and the second part contains the states in which
   * a four has appeared.
   */
  def getAllRandoms(state: GameState): (Seq[GameState], Seq[GameState]) = {
    val empties = state.empties
    val nEmpties = empties.size
    if (nEmpties > 0) {
      val every = math.max(nEmpties / maxChecks, 1)
      val zipped = for {
        j <- state.empties.indices
        if j % every == 0
        i = state.empties(j)
      } yield (state.setSquare(i, 1), state.setSquare(i, 2))
      zipped.unzip
    } else (Nil, Nil)
  }

  /**
   * Returns the average goodness from the given state when moving in
   * the given direction, a number of moves in advanced specified by
   * depth. The last argument specifies whether to consider fours
   * appearing in the averaging or not. Doing so slows down the AI
   * by a great deal.
   */
  def getAverageBadness(state: GameState, move: Int, depth: Int, considerFours: Boolean = false): Float = {
    val moved = state.shift(move)
    if (moved == state) Float.MaxValue
    else {
      val (twos, fours) = getAllRandoms(moved)
      def getSum(l: Seq[GameState]): Float = {
        if (depth == 0)
          (l map getStateBadness).sum
        else {
          // .par takes advantage of Scala's built-in parallel container processing
          val moves = (0 until 4).par map (m => l.par.map(t => getAverageBadness(t, m, depth - 1)).sum)
          moves.min
        }
      }
      val twosSum = getSum(twos)
      if (considerFours) {
        val foursSum = getSum(fours)
        twosSum / twos.size + foursSum / fours.size
      } else
        twosSum / twos.size
    }
  }

  /**
    * Returns the best, on average, move from a given state.
    */
  def getBestMove(state: GameState): Int = {
    val moves = 0 until 4
    val moveBadnesses = for {
      move <- moves
      goodness = getAverageBadness(state, move, depth)
      if goodness != -1
    } yield goodness
    moves minBy (i => moveBadnesses(i))
  }

  def advance(state: GameState) = state.move(getBestMove(state))
}