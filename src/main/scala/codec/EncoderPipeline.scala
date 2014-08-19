import java.nio.ByteBuffer

import scala.annotation.tailrec

/**
 * # Encoder/Decoder - the codec!
 * This class contains code responsible for converting input data to error-correction-coded colors
 * suitable for printing on a page.
 */
object EncoderPipeline {
  /**
   * ## Converting Bytes to Colors
   * We need to convert bytes into colors for arrangement in an image.
   *
   * The way to do this conversion is by trying to read bytes in from the provided sequence
   * in chunks of three at a time: then we concatenate them and mask out
   * eight consecutive groups of three bits each, which are converted to colors.
   *
   * Since these colors are intended to be printed, they will conceptually be a combination of
   * the printer ink colors: cyan, magenta, and yellow --
   * although in practice an absence of all three is simply implemented as white,
   * and the presence of all three is substituted with a module of black.
   *
   * A read of three bytes provides bits to eight colors A-H as follows:
   *     01100110 01101111 01101111
   *     AAABBBCC CDDDEEEF FFGGGHHH
   *     cmy
   * e.g. the fourth most significant bit in the second byte controls the yellow channel of color D.
   *
   * The exact shade output will vary by printer, but the convention for color naming used here is as follows:
   * CMY | color
   * ----|----
   * 000 | white
   * 100 | cyan
   * 010 | magenta
   * 110 | violet
   * 001 | yellow
   * 101 | green
   * 011 | orange
   * 111 | black
   *
   * For the example above, the colors are [orange yellow cyan violet black green green black]
   *
   * N.B. note the similarity to base 64 encoding: each module contains 3 bits of information,
   * so two modules represent a single character in base 64.
   *
   * Implementation notes: ByteBuffer.wrap is used to concatenate the bits in the three bytes
   * into an integer, which is then shifted and masked to retrieve the appropriate bits.
   * These are used to construct a Color object.
   *
   * The colors will be concatenated to the remainder of the stream using reduceLeft.
   * Since this will attach the colors in reverse order, the colors themselves
   * are generated in reverse order as well.
   */
  def bytesToColors(bytes: Seq[Byte]): Stream[Color] = {
    bytes match {
      // read exactly 3 bytes from byte sequence
      case b1 +: b2 +: b3 +: tail => {
        val bits = ByteBuffer.wrap(Array[Byte](0, b1, b2, b3)).getInt // concatenate three bytes

        // mask out eight groups of bits to convert into colors
        (for (i <- 0 until 8) yield {
          // extract the three bits comprising this color
          // start with the rightmost group of three bits -- foldLeft will attach them in reverse order of generation
          val colorcode = (bits >> (3 * i)) & 0x00000007

          // create the color
          new Color(cyan = (colorcode & 0x4) != 0,
            magenta = (colorcode & 0x2) != 0,
            yellow = (colorcode & 0x1) != 0)
        })
        .foldLeft[Stream[Color]](Stream())( // streaming the eight colors produced here
          (stream, next) => next #:: stream) #::: bytesToColors(tail)
      }

      // unable to read 3 bytes -- convert remaining bytes and attach termination sequence
      case _ => terminateColorStream(bytes)
    }
  }

  /**
   * ## Color Stream Termination
   * To allow unambiguous identification of the end of the data sequence, we'll attach
   * a termination symbol to the end of the input byte stream.
   *
   * The termination symbol consists of:
   * 1. a single black module and
   * 2. a module whose color indicates the number of padding bits
   * followed by as many white modules as is required to fill out the rectangle.
   * The input will be considered to end immediately before the last fully black module in the rectangle.
   *
   * A few possibilities arise involving the number of bytes N and the number of padding bits:
   * 1. We're missing one byte: so the bit assignment to the colors now looks like
   *     01100110 01101111 [00]
   *     AAABBBCC CDDDEEEF  FF
   *
   * The excess bits required to round out a color are called the 'padding bits' and are rendered as zero.
   *
   * There are two here, so the appropriate termination symbol is `111 011`,
   * that is, [black orange].
   *
   * 2. We're missing two bytes: so the bit assignment is
   *     01100110 [0]
   *     AAABBBCC  C
   *
   * There's one padding bit, giving a termination symbol of `111 001`, [black yellow].
   *
   * 3. There are no bytes left. No padding is needed, and the termination symbol is `111 000`: [black white].
   */
  private def terminateColorStream(bytes: Seq[Byte]): Stream[Color] = {
    // get concatenated bit sequence with padding bit count
    val (bits, padding) = bytes match {
      case b1 +: b2 +: Seq() => (ByteBuffer.wrap(Array[Byte](0, 0, b1, b2)).getInt << 2, 2) // two padding bits
      case b1 +: Seq() => {
        (ByteBuffer.wrap(Array[Byte](0, 0, 0, b1)).getInt << 1, 1)
      } // one padding bit
      case _ => (0, 0) // no padding bits -- if the sequence is nonempty, assume usage was to cut a stream short
    }

    // generate colors from bit sequence
    // if there are two bytes so two padding bits, there will be six colors
    // one byte with one padding bit gives three colors
    // and no bytes/no padding bits gives none; an immediate termination sequence
    (for (i <- 0 until (padding * 3)) yield {
      // extract the three bits comprising this color
      val colorcode = (bits >> (3 * i)) & 0x00000007

      new Color(cyan = (colorcode & 0x4) != 0,
        magenta = (colorcode & 0x2) != 0,
        yellow = (colorcode & 0x1) != 0)
    }).foldLeft[Stream[Color]](Stream())((stream, next) => next #:: stream) #:::
      Stream(new Color(true, true, true), // termination sequence black module
        new Color(false, padding >= 2, padding >= 1)) // indicator for appropriate number of padding bits
  }

  /**
   * ## Color Stream to Image
   * Method wraps a color stream into an image, completely defeating the point of saving memory by using streams.
   *
   * The actual image returned has width and height increased by 3 pixels each (@ 100 dpi, this is 0.03 in/<1 mm)
   * due to addition of finder and timing patterns. The finder pattern is a solid one-pixel border surrounding the
   * image, which enables image-recognition software to determine the boundaries of the contents. The timing pattern
   * is an alternating strip of black and white pixels (beginning with black at the far upper left hand corner)
   * extending across the top and left of the image, which allows imaging software to determine the number and
   * placement of pixels in the image.
   *
   * It's up to the caller to determine that the amount of data in the stream will fit in the given width/height.
   */
  // TODO switch to use ArrayBuilder or some equivalent... no need to be fucking around with lists and such this way
  // TODO how do you handle having excess colors in the sequence?
  def colorsToImage(colors: Seq[Color], width: Int, height: Int): ColorPanel = {
    import Color._ // get color definitions

    // method used to generate timing patterns:
    // generates alternating colors with increasing index
    def alternate(index: Int): Color = if (index % 2 == 0) BLACK else WHITE

    // the image begins and ends with the finder pattern
    // which forms a solid top and bottom black bar to the image
    val finderPattern = (for (i <- 0 until width + 4) yield BLACK).toArray

    // the second row of the image contains a timer pattern
    // delimited by black pixels from the finder pattern on the left and right
    val timerPattern = (BLACK :: (for (i <- 0 until width + 2) yield alternate(i)).toList ::: BLACK :: List()).toArray

    /**
     * Splits the input color stream into rows by width and rows remaining.
     */
    @tailrec
    def splitRows(colors: Seq[Color], width: Int, rowsLeft: Int, rowAccumulator: List[Seq[Color]]): List[Seq[Color]] = {
      import Color._

      if (rowsLeft < 1) rowAccumulator // TODO what if there are colors remaining? return that for generation of another image? exception out??
      else {
        // split remaining color sequence
        val (row, remainder) = colors.splitAt(width)

        // row postprocessing: if it is not long enough, pad with white pixels
        val length = row.length
        val paddedRow = row ++: (if (length < width) (for (i <- 0 until width - length) yield BLACK).toList else List())

        splitRows(remainder, width, rowsLeft - 1, paddedRow :: rowAccumulator)
      }
    }

    // the colors, cut up into rows, are used to form the intermediate rows of the image, consisting of:
    val imageData = splitRows(colors, width, height, List()).zipWithIndex
      .map({case (split, index) =>
      (BLACK ::                 // 1 black pixel (forming the left border of the finder pattern),
        alternate(index + 1) :: // 1 pixel (from the timer pattern) that may be black or white, depending on the current row
        split.toList :::        // the pixels of color data,
        BLACK :: List())        // and a terminal black pixel
    .toArray})                  // this list of pixels is converted into an array as a row

    // now we form the image
    val colordata = (
      finderPattern :: // the first row is the finder pattern
      timerPattern ::  // then a row of timer pattern
      imageData :::    // then the color data, which is a block of rows
      finderPattern :: // this is followed by the final row of the finder pattern
      List()).toArray  // and now convert this list of arrays to an array of arrays

    // insert the color data into the color panel and we are done
    new ColorPanel(width, height, colordata)
  }
}

/**
 * This case class stores a three-bit CMY color for the color stream.
 */
case class Color(cyan: Boolean, magenta: Boolean, yellow: Boolean)

/**
 * We'll store color definitions here for convenience.
 */
object Color {
  val BLACK = new Color(false, false, false)
  val WHITE = new Color(true, true, true)
}

/**
 * Need a class to store sized image data.
 */
class ColorPanel(val width: Int, val height: Int, pixels: Array[Array[Color]]) {
  /**
   * Gets the pixel at a specified row/column in the panel.
   */
  def apply(r: Int, c: Int): Color = {
    // bounds checking with extensive error messages
    if (r < 0) throw new IndexOutOfBoundsException("row index out of bounds: index is " + r + ", need a non-negative row index")
    if (c < 0) throw new IndexOutOfBoundsException("col index out of bounds: index is " + c + ", need a non-negative col index")
    if (r >= height) throw new IndexOutOfBoundsException("row index out of bounds: index is " + r + ", but height is " + height)
    if (c >= width) throw new IndexOutOfBoundsException("col index out of bounds: index is " + c + ", but width is " + width)

    // retrieve row: pixel data might not have correct row count, in which case an empty
    // row is returned: column pixel retrieval will note no data and return a blank pixel
    val row = if (r < pixels.length) pixels(r)
    else Array[Color]()

    // there is a possibility that input data is non-uniform row length (this should not be so!)
    // if it is, we should just pass out blank pixels for missing data
    if (c < row.length) row(c)
    else new Color(false, false, false)
  }
}