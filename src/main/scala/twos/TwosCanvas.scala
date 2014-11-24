package twos

import java.awt.{ Color, Font }
import scala.swing._
import scala.Vector
import twos.game._
import twos.ai._
import scala.swing.event.Key
import scala.swing.event.KeyReleased

abstract class TwosCanvas extends Panel {
  val that = this

  def AIDepth: Int
  def AIMaxChecks: Int
  
  def AINeighbourWeight: Float
  def AIClosenessWeight: Float
  def AIFullnessWeight: Float

  def SquareSize: Int
  def InternalMargin: Int
  def ExternalMargin: Int
  def GridSize: Int

  def BackgroundColorHex: Int
  def SquareColorsHex: Vector[Int]

  def FontSizes: Vector[Int]
  def TextColor1Hex: Int
  def TextColor2Hex: Int

  // Values that depend on abstract methods need to be inside
  // this object because otherwise the abstract methods all
  // return null at initialisation.
  object Dat {
    val TextColor1 = new Color(TextColor1Hex)
    val TextColor2 = new Color(TextColor2Hex)

    val BackgroundColor = new Color(BackgroundColorHex)

    // Storing the results of the calculation for speed
    val NumDigits = (0 until 20).toVector map (n => math.log10(math.pow(2, n)).floor.toInt + 1)

    val TotalMargin = 2 * ExternalMargin + (GridSize - 1) * InternalMargin
    val DisplayWidth = SquareSize * GridSize + TotalMargin + 16
    val DisplayHeight = SquareSize * GridSize + TotalMargin + 32
    val SquareColors = SquareColorsHex map (new Color(_))
    val Fonts = FontSizes map (new Font("Arial", Font.BOLD, _))

    val rand = new scala.util.Random()
    val ai = new AIByCorner(AIDepth, AIMaxChecks) {
      val NeighbourWeight = AINeighbourWeight
      val ClosenessWeight = AIClosenessWeight
      val FullnessWeight = AIFullnessWeight
    }

    var state: GameState = TwosGame.newGame(GridSize)
    var gameOver: Boolean = false
  }

  // allows the use of fields within Dat without explicitly
  // doing Dat.
  import Dat._
  
  def getDisplayDimension = new Dimension(DisplayWidth, DisplayHeight)

  def updateState(newState: GameState): Unit = {
    if (state == newState)
      gameOver = true
    else
      state = newState

    repaint()
  }

  def init() = {
    // Run the AI loop in a separate thread to maintain
    // GUI responsiveness
    new Thread() {
      override def run() = {
        while (true) {
          while (!gameOver) {
            updateState(ai.advance(state))
          }

          val restart = Dialog.showConfirmation(that, "Score: " + state.getScore + ". Restart?", "Game over!")
          if (restart != Dialog.Result.Yes && restart != Dialog.Result.Ok)
            System.exit(0)
          else {
            gameOver = false
            state = TwosGame.newGame(GridSize)
          }
        }
      }
    }.start()
  }

  def drawGrid(g: Graphics2D) = {
    // Go through the entire grid
    for (i <- 0 until GridSize; j <- 0 until GridSize) {
      val x = ExternalMargin + (InternalMargin + SquareSize) * i
      val y = ExternalMargin + (InternalMargin + SquareSize) * j
      val k = state.getSquare(i, j)

      // Draw the square itself
      g.setColor(SquareColors(k))
      g.fillRect(x, y, SquareSize, SquareSize)

      // 0 indicates an empty square
      if (k > 0) {
        // Draw the number in the square
        val s = math.pow(2, k).toInt.toString
        val f = Fonts(NumDigits(k))
        g.setColor(if (k <= 2) TextColor1 else TextColor2)
        g.setFont(f)
        // Centre the numbers within the squares
        val bounds = f.getStringBounds(s, g.getFontRenderContext)
        g.drawString(
          s,
          x + (SquareSize - bounds.getWidth).toFloat / 2,
          y + (SquareSize + bounds.getHeight).toFloat / 2.2f)
      }
    }
  }

  override def paintComponent(g: Graphics2D) {
    g.setColor(BackgroundColor)
    // Fill in the background
    g.fillRect(0, 0, size.width, size.height)

    drawGrid(g)
  }

}