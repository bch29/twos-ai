package twos.game

import scala.util.Random

object TwosGame {
  implicit val rand: Random = new Random

  def randTo(to: Int) = rand.nextInt().abs % to

  def randFromSeq[T](seq: Seq[T]) = seq(randTo(seq.size))

  def newGame(size: Int): GameState = {
    val state = GameState((0 until size * size).toVector map (_ => 0), size, 0)
    state.setRandom.setRandom
  }
}

// nFours is the number of fours that have randomly appeared
// (as opposed to twos) and is necessary to calculate score
// based on board state.
case class GameState(grid: Vector[Int], size: Int, nFours: Int) {
  @inline
  def p2s(pair: (Int, Int)) = pair._1 + pair._2 * size
  @inline
  def s2p(single: Int) = (single % size, single / size)

  // Helper function to get a grid position from a coordinate pair
  @inline
  def gridAt(where: (Int, Int)) = grid(p2s(where))
  
  /**
   * Helper function for shiftRowLeft.
   */
  def firstNonZero(row: List[Int], acc: Int): Int = {
    if (row.isEmpty) -1
    else if (row.head == 0) firstNonZero(row.tail, acc + 1)
    else acc
  }

  /**
   * Shifts one row to the left
   * @param row the row to shift.
   * @return the shifted row.
   */
  def shiftRowLeft(row: List[Int]): List[Int] = {
    // A row can only be shifted if it contains more than one element.
    if (row.size > 1) {
      // The first non zero element should be moved all the way to the left.
      val i = firstNonZero(row, 0)
      // If there are no non-zero elements, nothing can be shifted.
      if (i > -1) {
        // If there is an element matching the one at position i with only zeroes between, merge them.
        val j0 = firstNonZero(row drop (i + 1), 0)
        val j = if (j0 > -1) j0 + i + 1 else j0
        val ri = row(i)
        // Continue operating on the rest of the row.
        if (j > -1 && ri == row(j))
          (ri + 1) :: shiftRowLeft(row.updated(i, 0).updated(j, 0).tail)
        else
          ri :: shiftRowLeft(row.updated(i, 0).tail)
      } else row
    } else row
  }

  /**
   * These patterns help to turn a 2D vector into a list of rows or columns
   */
  val patternsH =
    0 until size map (i => i * size until (i * size + size))

  val patternsV =
    0 until size map (i => i until (i + size * size) by size)

  /**
    * Returns the result of shifting the grid in the given direction.
    */
  def shift(dir: Int): GameState = {
    // is the shift along the horizontal axis?
    val isH = dir == 0 || dir == 2
    // is the shift in the positive coordinate direction?
    val isP = dir == 0 || dir == 1

    val patterns = if (isH) patternsH else patternsV

    /*
     * This is a function that takes a column and returns the corresponding row or column
     * such that calling shiftRowLeft on it will shift it in the direction corresponding
     * to dir
     */
    val rowMapper1: Int => List[Int] = x => patterns(x).map(grid(_)).toList
    val rowMapper: Int => List[Int] =
      if (isP) x => rowMapper1(x).reverse
      else rowMapper1

    val rowsShifted =
      (0 until size).map(rowMapper)
        .map(shiftRowLeft)

    /*
     * Rearranges rows back into vertical columns.
     */
    def rearrange(vec: Vector[Int]): Vector[Int] = {
      val pattern = patterns.flatten.toVector
      vec.indices.toVector map (i => vec(pattern(i)))
    }

    val result1 = if (isP) rowsShifted map (_.reverse) else rowsShifted
    val result2 = result1.flatten.toVector
    val result = if (isH) result2 else rearrange(result2)

    GameState(result, size, nFours)
  }

  /**
    * Returns the state after being shifted in the given direction
    * and placing a random new square.
    */
  def move(dir: Int): GameState = shift(dir).setRandom

  lazy val empties: Seq[Int] = for {
    i <- grid.indices
    if grid(i) == 0
  } yield i

  lazy val nonEmpties: Seq[Int] = for {
    i <- grid.indices
    if grid(i) > 0
  } yield i

  @inline
  def setSquare(i: Int, to: Int): GameState = GameState(grid.updated(i, to), size, nFours)
  @inline
  def setSquare(where: (Int, Int), to: Int): GameState = setSquare(p2s(where), to)

  @inline
  def getSquare(i: Int): Int = grid(i)
  @inline
  def getSquare(where: (Int, Int)): Int = grid(p2s(where))

  def addFour = GameState(grid, size, nFours + 1)

  /**
    * Returns true if the given point is within the bounds
    * of the board.
    */
  def squareExists(where: (Int, Int)): Boolean = {
    val (i, j) = where
    i >= 0 && j >= 0 && i < size && j < size
  }

  /**
    * Creates a new square in a random location with a 10% chance
    * of it being a 4 and a 90% chance of it being a 2.
    */
  def setRandom =
    if (empties.isEmpty) this
    else {
      if (TwosGame.randTo(10) > 0)
        setSquare(TwosGame.randFromSeq(empties), 1)
      else
        setSquare(TwosGame.randFromSeq(empties), 2).addFour
    }

  def closestNeighbour(where: (Int, Int)): Int = {
    val (i, j) = where
    val neighbourCoords = List((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
    val neighbourDiffs = for {
      (x, y) <- neighbourCoords
      if x >= 0 && x < size && y >= 0 && y < size && getSquare(x, y) != 0
    } yield (getSquare(x, y) - getSquare(where)).abs
    if (neighbourDiffs.isEmpty) -1 else neighbourDiffs.min
  }

  def closestNeighbour(i: Int): Int = closestNeighbour(s2p(i))

  def canMove = empties.size > 0 || (grid.indices exists (i => closestNeighbour(i) == 0))

  def getScore: Int = {
    val tileScores = nonEmpties map (x => Math.pow(2, grid(x)) * (grid(x) - 1))
    tileScores.sum.toInt - nFours * 2
  }
}