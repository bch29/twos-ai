package twos

import java.awt.{ Color, Font }

import scala.swing._
import scala.Vector

import twos.game._
import twos.ai._

object TwosForm extends scala.swing.SimpleSwingApplication {
  
  trait CanvasProperties {
    val AIDepth = 8
    val AIMaxChecks = 1
    
    val AINeighbourWeight = 5f
    val AIClosenessWeight = 3f
    val AIFullnessWeight = 3f
    
    val SquareSize = 100
    val InternalMargin = 10
    val ExternalMargin = 20
    val GridSize = 4

    val BackgroundColorHex = 0xbbada0
    val SquareColorsHex = Vector(
      0xccc0b4, 0xeee4da, 0xede0c8, 0xf2b179, 0xec8d54, 0xf67c5f, 0xff6633, 0xf3d86b,
      0xf1d04b, 0xe4c02a, 0xe2ba13, 0xecc400, 0x5fda93, 0x4e4439, 0x4e4439, 0x4e4439,
      0x4e4439, 0x4e4439, 0x4e4439, 0x4e4439, 0x4e4439, 0x4e4439, 0x4e4439, 0x4e4439)
      
    val TextColor1Hex = 0x776e65
    val TextColor2Hex = 0xffffff
      
    val FontSizes = Vector(40, 48, 48, 42, 32, 26, 20, 14, 8, 8, 8, 8, 8, 8, 8)
  }

  def top: Frame = new MainFrame {
    val canv = new TwosCanvas with CanvasProperties
    
    preferredSize = canv.getDisplayDimension

    contents = canv
    canv.init()
  }

}
