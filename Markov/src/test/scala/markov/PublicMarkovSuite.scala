package markov

import java.io.File

import org.scalatest.FunSuite
import org.scalatest.concurrent.Eventually
import support.TreeTraversals._

import scala.util.Random

class PublicMarkovSuite extends FunSuite with ChainAlgorithm with Eventually {

	test("[1!] Your implementation should not have any while loops") {
		eventually {
			val xm = getMethods("src/main/scala/markov/Markov.scala")
			xm.foreach(m => {
				assert(!hasWhileLoop(m), "one of your methods has a while loop")
			})
		}
	}

	test("[1!] Your implementation should not have any foreach loops") {
		eventually {
			val xm = getMethods("src/main/scala/markov/Markov.scala")
			xm.foreach(m => {
				assert(!hasForEach(m), "one of your methods has a foreach loop")
			})
		}
	}

	test("[1!] Your implementation should not have any for comprehensions") {
		eventually {
			val xm = getMethods("src/main/scala/markov/Markov.scala")
			xm.foreach(m => {
				assert(!hasForComp(m), "one of your methods has a for comprehension")
			})
		}
	}

	test("[1!] Your implementation should not have any var variables") {
		eventually {
			val xm = getMethods("src/main/scala/markov/Markov.scala")
			xm.foreach(m => {
				assert(!hasVarDef(m), "one of your methods has a var variable")
			})
		}
	}

	test("[2] Your splitWords function should split a line of words") {
		eventually {
			val line = "this is a sentence, with words!"
			val result = Array("this", "is", "a", "sentence,", "with", "words!")
			assert(MyBuilder.splitWords(line).deep == result.deep)
		}
	}

	test("[2] Your getFirstWord should return the first word") {
		eventually {
			val words = Array("this", "is", "a", "sentence,", "with", "words!")
			assert(MyBuilder.getFirstWord(words) == "this")
		}
	}

	test("[3] Your addToChain should add a new mapping to a populated chain") {
		eventually {
			// Define the starting chain:
			var chain   = new Chain
			val prefix  = new Prefix("the", "dog")
			val suffix1 = new Suffix("pal")
			chain = chain + (prefix -> Vector(suffix1))

			// Define the target chain we want:
			var target = new Chain
			val suffix2 = new Suffix("friend")
			target = target + (prefix -> Vector(suffix1, suffix2))

			// Invoke the addToChain function to get the result:
			val result = MyBuilder.addToChain(chain, prefix, suffix2)

			// Compare:
			assert(target == result, "The resulting chain is not the same as the target")
		}
	}

	test("[2] Your getSuffixesLength should return the length of the suffixes") {
		eventually {
			val suffixes = Vector(new Suffix("this"), new Suffix("is"), new Suffix("a"), new Suffix("test"))
			assert(MyGenerator.getSuffixesLength(suffixes) == 4, "The length is incorrect")
		}
	}

	test("[1] Your isSuffixNewLine should identify suffixes that are newlines") {
		eventually {
			val s = new Suffix("\n")
			assert(MyGenerator.isSuffixNewline(s), "should be a newline suffix")
		}
	}

	test("[4] Your buildWithWords should populate a chain with correct size") {
		eventually {
			val (_, c) = MyBuilder.buildWithWords(Array("this", "is", "a", "sentence"), Prefix.empty, new Chain)
			assert(c.size == 4, "the size should be 4")
		}
	}

	test("[5] Your buildWithLines should populate a chain with correct size") {
		eventually {
			val lines  = Array("this is a sentence")
			val iter   = lines.iterator
			val prefix = Prefix.empty
			val chain  = new Chain
			val result = MyBuilder.buildWithLines(iter, prefix, chain)
			assert(result.size == 4, "the size should be 4")
		}
	}

	test("[10] Your apply should generate the correct text") {
		object TestRandomizer extends Randomizer {
			Random.setSeed(0)
			def random(n: Int) = Random.nextInt(n)
		}
		eventually {
			val book = "books" + File.separator + "oz.txt"
			val output = MyMarkov(Array(book), 200, TestRandomizer)
			val target = """   At once the great Head, made out of sight, with all the rust was removed and it seemed that at last he was so awkward that Dorothy woke from her ruby throne to give the beast was a Ball of Fire it became quite wet, and he stood stupidly in one of the Scarecrow found a little girl a good-bye kiss, and no one could climb down, and he dashed up the leg sulkily and led her through many of the dark, so she never took them off had she wished, but of course they took pains not to speak of it, and Oz was a great chattering and flapping of wings, as the tiger had said, and its body covered with a carefully painted face.  I have played Wizard for so many years she has of the outer wall, and they got along quite well at first, for they had generously saved him from curiosity, he is a great chattering and laughing, and the Cowardly Lion.     They now turned and flew away with the green girl good-bye, and they had begun to look into the green glow became brighter and brighter, """
			assert(output == target, "your text did not match ours")
		}
	}

}