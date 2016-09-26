package markov

/**
 * `Prefix` represents a prefix in the markov chain.
 *
 * In this implementation a prefix consists of two words. The
 * `first` word precedes the `second` word in the order it appears in
 * the text that is read in from an originating file or in the order
 * generated in the output.
 *
 * @param first  the first word
 * @param second the second word
 */
class Prefix(val first: String, val second: String) {

  /**
   * `shiftIn` shifts a new `word` into this prefix to produce a new
   * prefix. For example,
   *
   * {{{
   * val p = new Prefix("hello", "world")
   * val n = p.shiftIn("peace")
   * }}}
   *
   * `n` in the above code would result in `Prefix("world", "peace")`.
   *
   * @param word the word to shift in
   * @return a new `Prefix` instance with `word` in second and second
   *         in first.
   */
  def shiftIn(word: String): Prefix = new Prefix(second, word)

  /**
   * `equals` compares this `Prefix` to the object `o`.
   *
   * We need to override this to provide natural equality between two
   * prefixes. Naturally, prefix A and B are equivalent if A.first is
   * equivalent to B.first and A.second is equivalent to
   * B.second. This is required (in combination with hashCode) to
   * allow a `Prefix` to be used as a key in a `Map` collection.
   *
   * @param o other object
   * @return `true` if they are equivalent; `false` otherwise
   */
  override def equals(o: Any) = {
    if (o.isInstanceOf[Prefix]) {
      val other = o.asInstanceOf[Prefix]
      other.first == first && other.second == second
    } else {
      false
    }
  }

  /**
   * `hashCode` generates a unique integer for a `Prefix`.
   * We need to override this method so we can use a `Prefix` as a
   * key in a `Map` collection.
   *
   * @return the hashcode for a prefix
   */
  override def hashCode: Int = s"$first $second".hashCode

  /** Returns a string representation of a prefix. */
  override def toString: String = s"[$first, $second]"
}

/**
 * The `Prefix` object provides definitions for all prefixes.
 */
object Prefix {
  /**
   * `empty` returns the empty prefix.
   * The empty prefix is a prefix where the first and second word are
   * newlines. It is used as the starting prefix for building a
   * `Chain` as well as generating output text from a `Chain`.
   *
   * @return the empty prefix
   */
  def empty: Prefix = new Prefix("\n", "\n")
}

/**
 * `Suffix` represents a suffix word.
 * Yes, this is a very simple class - however, it makes it more clear
 * in the implementation to differentiate between arbitrary strings
 * and suffix words.
 *
 * @param word the word representing the suffix
 */
class Suffix(val word: String) {
  override def toString: String = word

  /**
   * Compares this suffix to another object for equality.
   */
  override def equals(o: Any) = {
    if (o.isInstanceOf[Suffix]) {
      val other = o.asInstanceOf[Suffix]
      other.word == word
    } else {
      false
    }
  }
}

/**
 * `ChainAlgorithm` is an algorithm used to compute a markov chain.
 *
 * We use this trait to simply define a new type called `Chain` that
 * is used in sub-traits to make definitions easier to read.
 */
trait ChainAlgorithm {
  /**
   * `Chain` is a type alias for `Map[Prefix, Vector[Suffix]]`.
   *
   * A `Chain` is a mapping from a prefix to a vector of suffixes. It
   * is the primary data structure that is used during the markov
   * chain algorithm to associate prefix words to a list of suffix
   * words that follow the prefix in the input text.
   *
   * Note: this implementation requires immutable collections. `Map`
   * and `Vector` are both immutable collections. You are not allowed
   * to use mutable collections in your implementation.
   */
  type Chain = scala.collection.immutable.HashMap[Prefix, Vector[Suffix]]
}

/**
 * `Builder` represents the markov chain's "building" algorithm.
 *
 * Although we are providing only a single implementation in this
 * assignment, we are structuring the application such that we could
 * provide multiple different implementations. In an object-oriented
 * fashion we can do this through composition.
 */
trait Builder extends ChainAlgorithm {
  /** `build` transforms an array of files into a `Chain` object.
    * @param files an array of files to read in to the chain
    * @return the markov chain
    */
  def build(files: Array[String]): Chain
}

/**
 * `Generator` represents the markov chain's "generator" algorithm.
 *
 * Although we are providing only a single implementation in this
 * assignment, we are structuring the application such that we could
 * provide multiple different implementations. In an object-oriented
 * fashion we can do this through composition.
 */
trait Generator extends ChainAlgorithm {
  /**
   * `generate` transforms a markov chain into an output string.
   *
   * The output string is text generated from the prefix and suffixes
   * contained in the markov chain. We provide it the number of
   * `words` to generate and a `Randomizer` object that is used to
   * randomly pick suffixes.
   *
   * @param chain the markov chain
   * @param words the number of words to generate
   * @param rand  the randomizer
   * @return the generated paragraph as a string
   */
  def generate(chain: Chain, words: Int, rand: Randomizer): String
}

/** `Randomizer` provides a `random` function.
  *
  * This is used in the `Generator.generate` implementation to
  * randomly select a suffix. We implement this as a trait so we can
  * use a deterministic implementation that is easy to test in the
  * Unit tests (e.g., always returns 0).
  */
trait Randomizer {
  /**
   * `random` returns a random integer.
   * The random integer is between 0 (inclusive) and n (exclusive).
   */
  def random(n: Int): Int
}

/**
 * `Markov` represents the markov chain algorithm.
 *
 * The full algorithm requires a `Builder` and a `Generator`. That is
 * why this class is abstract. It does provide an `apply` method that
 * invokes the builder and generator functionality properly.
 */
abstract class Markov {
  // To be implemented in sub-classes:
  val builder  : Builder
  val generator: Generator

  /**
   * `apply` invokes the markov chain algorithm.
   *
   * @param files  an array of file names
   * @param words  the number of words to generate
   * @param rand   the randomizer to use during generation
   */
  def apply(files: Array[String], words: Int, rand: Randomizer): String =
    generator.generate(builder.build(files), words, rand)
}
