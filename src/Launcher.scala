/**
  * This exercise is about converting a normal picture into an ascii picture
  */
import java.awt.image.BufferedImage
import java.io.{File, PrintWriter}
import javax.imageio.{ImageIO}

/**
  * Created by David on 22/03/2017.
  */
object Launcher {
  def main(args: Array[String]): Unit = {
    // Verify command-line arguments
    if (!areGoodCommandLineArguments(args)) {
      println("Syntax : scalimg <image_path>")
      System.exit(0)
    }

    // Get the input image provided
    var inputImage = retrieveImage(args(0)).get

    // Store some datas for convenience
    var cols = inputImage.getWidth
    var rows = inputImage.getHeight
    var xFactor = if (args.length > 1) retrievePrecision(args(1)) else 1
    var yFactor = xFactor

    // Declare a new matrix to store the ascii picture
    var asciiImage = Array.ofDim[Character](rows / yFactor, cols / xFactor)

    for {
      i <- 0 until rows / yFactor
      j <- 0 until cols / xFactor
    } asciiImage(i)(j) = grayScaleToAscii(averageOf(inputImage.getRGB(j, i)))

    storeAsciiImage(asciiImage, args(0) + ".txt")

    System.exit(0)
  }

  /**
    * Check whether the command used to start this programm is good or not
    */
  def areGoodCommandLineArguments(args: Array[String]): Boolean =
    args.length == 1

  /**
    * Returns the gray scale associated to a given color
    * */
  def grayScaleOf(color: Int): Int = {
    var mix = averageOf(color)
    (mix << 16) + (mix << 8) + mix
  }

  /**
    * Returns the average mix of the three (r,g,b) components of a given color
    */
  def averageOf(color: Int): Int = {
    var r = (color >> 16) & 0xff
    var g = (color >> 8) & 0xff
    var b = color & 0xff
    (r + g + b) / 3
  }

  /**
    * Returns the ascii character associated to a given gray shade
    */
  def grayScaleToAscii(g: Int): Character = {
    lazy val asciiRamp: String = " .,:;ox%#@"
    if (g >= 230.0)
      ' '
    else if (g >= 200.0)
      '.'
    else if (g >= 180.0)
      '*'
    else if (g >= 160.0)
      ':'
    else if (g >= 130.0)
      'o'
    else if (g >= 100.0)
      '&'
    else if (g >= 70.0)
      '8'
    else if (g >= 50.0)
      '#'
    else
      '@'
  }

  /**
    * Retrieve the image from the command-line argument and return it
    */
  def retrieveImage(p: String): Option[BufferedImage] = {
    var picture: BufferedImage = null
    try {
      picture = ImageIO.read(new File(p))
    } catch {
      case ex: Exception => {
        println("File not found or File corrupted : " + p)
        System.exit(0)
      }
    }
    Some(picture)
  }

  /**
    * Retrieve the precision which has to be used in order to render the ascii image and returns the scale factor
    */
  def retrievePrecision(p: String): Int = {
    try if ((p toInt) > 0) 100 / (p toInt) else 1
    catch {
      case ex: Exception =>
        1
    }
  }

  /**
    * Store the ascii image created
    */
  def storeAsciiImage(asciiPicture: Array[Array[Character]],
                      filepath: String): Unit = {
    new PrintWriter(new File(filepath))
      .write(asciiPicture.map(_.mkString(" ")).mkString("\n"))
  }
}