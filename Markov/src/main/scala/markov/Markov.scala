package markov

import scala.collection.immutable.HashMap
import scala.util.Random
import scala.io.Source

/**
 * `MyBuilder` extends the `Builder` trait.
 * See `Builder` for additional information.
 */
object MyBuilder extends Builder {

  /* The implementation of this Builder provides several overloaded
   * `build` methods. Each method decomposes the problem into smaller
   * and smaller pieces. Each piece can easily be solved by relying on
   * the solution to the smaller parts. These methods do not use
   * loops, rather, they rely solely on recursion.
   *
   * It turns out that recursion provides a natural way to work with
   * immutable data structures. Hopefully by the end of this
   * assignment you will see clearly why this is the case.
   */

  /**
   * This is the abstract method we are required to implement (as
   * indicated in the `Builder` trait). The implementation for this
   * one is simple - so we provide it for you. We simply delegate the
   * responsibility of building a `Chain` to a different `build`
   * method that accepts an array of file names and a new Chain as
   * its arguments.
   */
  def build(files: Array[String]): Chain =
    buildWithChain(files, new Chain)

  /**
   * The `buildWithChain` method uses recursion to solve the problem.
   * The base case is simple: if the array of files is empty, we are done
   * and return the `chain` that was passed to it. The recursive step
   * is equally simple: we call this method on the tail of the
   * array of files (the rest of them) and let tje `buildSingleFile`
   * method solve the problem of building the chain from each file
   * file in the list. When that build method completes it returns
   * the resulting `Chain` from that file (`files.head`).
   */
  private[markov] def buildWithChain(files: Array[String], chain: Chain): Chain =
    if (files.isEmpty) chain
    else buildWithChain(files.tail, buildSingleFile(files.head, chain))

  /**
   * The `buildSingleFile` method performs the necessary step of
   * reading in the given file. We use the `Source.fromFile` method
   * to read in the file and then the `getLines` method from that to
   * get an `Iterator[String]` which is an iterator over the lines in the
   * file. With this job out of the way we defer yet again to another
   * build method that accepts an iterator, a prefix, and a chain.
   *
   * Because we do not yet have a prefix we use the *empty* prefix to
   * start the process off. We pass the `chain` through to this call.
   */
  private[markov] def buildSingleFile(file: String, chain: Chain): Chain =
    buildWithLines(Source.fromFile(file).getLines(), Prefix.empty, chain)

  /**
   * The `buildWithLines` method is where some work happens. This
   * method receives an iterator over the lines of a file. It also
   * receives a prefix and the current chain. Note, that this method
   * also returns a chain. Here are the steps to make this work:
   *
   * (1) This is a recursive function, so we need a base case. In
   * this function the base case is when we no longer have any
   * lines to process. When there are no more lines, we simply
   * return the chain.
   *
   * (2) The recursive step is not too hard since we can rely on a
   * `buildWithWords` method that handles individual words. First,
   * we split the next line into words. We need not concern ourselves
   * with punctuation as it often works out fine in the resulting
   * text - we can simply split on the single space string " ".
   * We can then use the resulting array of words (strings) as
   * an argument to the `buildWithWords` method responsible for
   * words, of course, passing the prefix and chain along as well.
   *
   * (3) That call will return a tuple (Prefix, Chain). This is the
   * new prefix and chain that results from adding new words to
   * the chain. You can simply pass the lines and the new prefix
   * and chain in a recursive call to this build function.
   *
   * The basic idea is that we are using recursion instead of a loop
   * to iterate over the lines from a file. We do the work of
   * splitting the line into words, but let a different function take
   * care of processing the words and producing a new chain.
   */
  private[markov] def buildWithLines(lines: Iterator[String],
                                     prefix: Prefix,
                                     chain: Chain): Chain =
  // TODO: implement this one!
  {
    if(lines.isEmpty)
      chain
    else{
      val words = splitWords(lines.next())
      val tuple = buildWithWords(words, prefix, chain)
      buildWithLines(lines, tuple._1, tuple._2)
    }
  }

  /**
   * The `buildWithWords` method works on adding new prefixes to
   * the chain by recursing over the given words. Here are the
   * steps you must take:
   *
   * (1) First, identify the base case. What could it be? Think about
   * this for a moment - it should be clear. When you reach the base
   * case you simply return a tuple containing the prefix and the
   * chain.
   *
   * (2) The recursive step is simple, albeit subtle. We first create
   * a new `Suffix` from the first word in the array of words. Next,
   * we need to add that suffix to the chain. Remember, a `Chain` is
   * a map from `Prefix` to a `Vector[Suffix]`. It is completely
   * reasonable for a `Prefix` to be associated with multiple
   * `Suffix`s. So, first identify if the prefix already has a
   * mapping - if it does then you can add the new suffix to the
   * vector of existing suffixes. If it does not, you can simply
   * create a new vector, add the new suffix. Lastly, map the current
   * prefix to the new vector of suffixes into the chain.
   *
   * (3) Lastly, make the recursive call to this build function
   * passing to it the rest of the words, a new prefix that
   * results from calling its `shiftIn` method with the first
   * word of words, and the extended chain.
   */
  private[markov] def buildWithWords(words: Array[String],
                                     prefix: Prefix,
                                     chain: Chain): (Prefix, Chain) =
  // TODO: implement this one!
  {
    if(words.length==0)
      (prefix, chain)
    else{
      val suffix = new Suffix(getFirstWord(words))
      val otherchain = addToChain(chain, prefix, suffix)
      buildWithWords(getRestOfWords(words), prefix.shiftIn(getFirstWord(words)), otherchain)
    }

  }

  /**
   * Splits the line of words (as a String) into an array of words.
   *
   * You should split on spaces " " - no need to worry about punctuation!
   *
   * @param line  the line to split
   * @return an array of words
   */
  private[markov] def splitWords(line: String): Array[String] =
  // TODO: implement this one!
    line split (" ")


  /**
   * Returns the first word from the array.
   *
   * Thus:
   *
   * getFirstWord(Array("this", "is", "fun")) => "this"
   *
   * @param words an array of words
   * @return the first word
   */
  private[markov] def getFirstWord(words: Array[String]): String =
  // TODO: implement this one!
    words(0)

  /**
   * Returns the rest of the words from the array of words.
   *
   * Thus:
   *
   * getRestOfWords(Array("this", "is", "fun")) => Array("is", "fun")
   *
   * @param words the array of words
   * @return the rest of the words
   */
  private[markov] def getRestOfWords(words: Array[String]): Array[String] =
  // TODO: implement this one!
    words drop (1)

  /**
   * Returns a new vector of suffixes with the given suffix added.
   *
   * Thus:
   *
   * addSuffix(Vector(Suffix("hello")), Suffix("the"))
   * => Vector(Suffix("hello"), Suffix("the"))
   *
   * @param suffixes  the vector of suffixes
   * @param suffix    the new suffix to add
   * @return a new vector of suffixes with `suffix` added
   */
  private[markov] def addSuffix(suffixes: Vector[Suffix], suffix: Suffix): Vector[Suffix] =
  // TODO: implement this one!
    suffixes :+ suffix

  /**
   * Returns a new `Chain` with the additional mapping from prefix to suffix.
   *
   * Thus:
   *
   * addToChain(Map(Prefix("the", "dog") -> Vector(Suffix("man"))),
   * Prefix("the", "dog"), Suffix("friend")))
   * =>
   *
   * Map(Prefix("the", "dog") -> Vector(Suffix("man"), Suffix("friend")))
   *
   * NOTE: addSuffix is a useful helper method to use here.
   *
   * @param chain     the markov chain
   * @param prefix    the prefix
   * @param suffix    the new suffix
   * @return a new chain with the new prefix to suffixes mapping
   */
  private[markov] def addToChain(chain: Chain, prefix: Prefix, suffix: Suffix): Chain = {
    // TODO: implement this one!

    if (chain.contains(prefix))
      chain + (prefix -> addSuffix(chain(prefix), suffix))
    else {
      val newChain = HashMap(prefix -> addSuffix(Vector[Suffix](), suffix))
      chain ++: newChain
    }

  }

}

  /**
   * `MyGenerator` extends the `Generator` trait.
   * See `Generator` for additional information.
   */
  object MyGenerator extends Generator {

    /**
     * The `generate` function simply calls the `generateAll`method
     * with a prefix as the last parameter. We pass in the empty prefix
     * as it will be the starting point for generating text.
     *
     * We give you this one!
     */
    def generate(chain: Chain,
                 words: Int,
                 rand: Randomizer): String =
      generateAll(chain, words, rand, Prefix.empty)

    /**
     * This `generateAll` function is the work horse. It is a recursive
     * function that generates the provided number of `words` from the
     * given chain starting with the given prefix. The algorithm uses
     * the given `Randomizer` to randomly select suffixes associated
     * with a given prefix. Here are the steps:
     *
     * (1) Identify the base case - we stop when words is 0. As we are
     * required to return a string we simply return the empty
     * string "".
     *
     * (2) Next, we get the vector of suffixes from the chain using the
     * current prefix. We use the length of the vector of suffixes
     * to choose a random suffix from the vector of suffixes. If
     * the suffix word is "\n" we stop generating text (we reached
     * the end of a prefix) and return the empty string "";
     * otherwise we make the recursive call to this function giving
     * it the chain, words - 1, rand, and a new prefix by shifting
     * in its suffix word. We then append the suffix word, a space,
     * and the return value of the recursive call. We return the
     * resulting string.
     */
    private[markov] def generateAll(chain: Chain,
                                    words: Int,
                                    rand: Randomizer, prefix: Prefix): String = {
      // TODO: implement this one!

      if(words == 0) ""
      else{
        val suffixes = chain(prefix)
        val suffixWord = getRandomSuffix(suffixes, rand)
        if(isSuffixNewline(suffixWord)) ""
        else{
         val cool= generateAll(chain, words-1, rand, prefix.shiftIn(suffixWord.toString))
         val resultString = suffixWord.toString + " " + cool
          resultString
        }
      }

    }
    /**
     * `getSuffixes` returns the suffixes associated with the given prefix.
     *
     * @param chain   the markov chain
     * @param prefix  the prefix clause
     * @return the vector of suffixes
     */
    private[markov] def getSuffixes(chain: Chain, prefix: Prefix): Vector[Suffix] =
    // TODO: implement this one!
      chain(prefix)

    /**
     * Returns the length of the suffixes.
     *
     * @param suffixes  the vector of suffixes
     * @return the length of the vector of suffixes
     */
    private[markov] def getSuffixesLength(suffixes: Vector[Suffix]): Int =
    // TODO: implement this one!
      suffixes length

    /**
     * Returns a random suffix.
     *
     * Note: this function should use `getSuffixesLength` to get the
     * length of the suffixes vector to feed into the randomizer.
     *
     * @param suffixes  the vector of suffixes
     * @param rand      the randomizer
     * @return a random suffix
     */
    private[markov] def getRandomSuffix(suffixes: Vector[Suffix], rand: Randomizer): Suffix = {
      // TODO: implement this one!
      suffixes(rand.random(getSuffixesLength(suffixes)))
    }
    /**
     * Returns true if this suffix is a newline ("\n"); false otherwise.
     *
     * @param suffix  the suffix
     * @return true if this suffix is a newline; false otherwise
     */
    private[markov] def isSuffixNewline(suffix: Suffix): Boolean = {
      // TODO: implement this one!
      val newLine = new Suffix("\n")
      suffix == newLine
    }
  }

  /**
   * The implementation of a Randomizer.
   * We give this to you!
   */
  object MyRandomizer extends Randomizer {
    def random(n: Int) = Random.nextInt(n)
  }

  /**
   * This instantiates a new Markov object using composition of our
   * Builder and Generator implementations. We also provide a main
   * method that you can use to run this as an application. From the
   * activator console you would do this (Mac/Linux):
   *
   * ./activator
   * > run-main markov.MyMarkov books/dracula.txt books/oz.txt
   *
   * Or, on Windows:
   *
   * activator.bat
   * > run-main markov.MyMarkov books\dracula.txt books\oz.txt
   */
  object MyMarkov extends Markov {
    val builder = MyBuilder
    val generator = MyGenerator

    def main(args: Array[String]) = {
      if (args.isEmpty)
        println("expected file names, got nothing")
      else
        println(MyMarkov(args, 200, MyRandomizer))
    }
  }


