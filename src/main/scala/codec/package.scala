import java.nio.ByteBuffer

/**
 * # Encoder/Decoder - the codec!
 * This class contains code responsible for converting input data to error-correction-coded colors
 * suitable for printing on a page.
 */
package object codec {
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
   * The colors will be concatenated to the remainder of the stream using foldLeft.
   * Since this will attach the colors in reverse order, the colors themselves
   * are generated in reverse order as well.
   */
  def byteSeqToColorStream(bytes: Seq[Byte]): Stream[Color] = {
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
          new Color(cyan = (colorcode & 0x8) != 0,
            magenta = (colorcode & 0x4) != 0,
            yellow = (colorcode & 0x1) != 0)
        })
        .foldLeft[() => Stream[Color]](() => byteSeqToColorStream(tail))( // streaming the eight colors produced here:
          (stream, next) => () => next #:: stream())() // stream held as function to enforce lazy evaluation
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
   * 1. a single black module,
   * 2. a module whose color indicates the number of padding bits, and
   * 3. as many white modules as is required to fill out the rectangle
   * The input will be considered to end immediately before the last fully black module in the rectangle.
   *
   * A few possibilities arise involving the number of bytes N and the number of padding bits:
   *
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
      case b1 +: b2 +: Seq() => (((b1 << 8) | b2) << 2, 2) // two padding bits
      case b1 +: Seq() => (b1 << 1, 1) // one padding bit
      case _ => (0, 0) // no padding bits -- if the sequence is nonempty, assume usage was to cut a stream short
    }

    // generate colors from bit sequence
    // if there are two bytes so two padding bits, there will be six colors
    // one byte with one padding bit gives three colors
    // and no bytes/no padding bits gives none; an immediate termination sequence
    (for (i <- 0 until (padding * 3)) yield {
      // extract the three bits comprising this color
      val colorcode = (bits >> (3 * i)) & 0x00000007

      new Color(cyan = (colorcode & 0x8) != 0,
        magenta = (colorcode & 0x4) != 0,
        yellow = (colorcode & 0x1) != 0)
    }).foldLeft[Stream[Color]](
      Stream(new Color(true, true, true), // termination sequence black module
        new Color(false, padding >= 2, padding >= 1), // indicator for appropriate number of padding bits
        new Color(false, false, false)))((stream, next) => next #:: stream)
  }

  /**
   * This case class stores a three-bit CMY color for the color stream.
   */
  case class Color(cyan: Boolean, magenta: Boolean, yellow: Boolean)

  // TODO: wrap color stream into an image

  // TODO: image generator class; automatically adds finding and timing patterns

  // HOWTO: is there a way to get low-level printer control -- to avoid dpi moire problems?
}