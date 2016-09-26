package cs220.evaluator

import cs220.regex._
import org.scalatest.FunSuite

class RegExTestSuite extends FunSuite {

  test("An empty regular expression should match empty") {
    val S  = Factory.re
    var re = S.empty
    assert((re matches "") == true)
  }

  test ("An empty regular expression should not match a character") {
    val S  = Factory.re
    val re = S.empty
    assert((re matches "c") == false)
  }

  test ("A character regular expression should match a character") {
    val S  = Factory.re
    val re = S.char('p')
    assert((re matches "p") == true)
  }

  test ("A character regular expression should not match a non-matching character") {
    val S  = Factory.re
    val re = S.char('p')
    assert((re matches "j") == false)
  }

  test ("An empty string regular expression should match ''") {
    val S  = Factory.re
    val re = S.str("")
    assert((re matches "") == true)
  }

  test ("An empty string regular expression should not match string 'p'") {
    val S  = Factory.re
    val re = S.str("")
    assert((re matches "p") == false)
  }

  test ("A string regular expression should match a same string") {
    val S  = Factory.re
    val re = S.str("abcabc")
    assert((re matches "abcabc") == true)
  }

  test ("A regular expression /abcabc/ should not match 'abcab'") {
    val S  = Factory.re
    val re = S.str("abcabc")
    assert((re matches "abcab") == false)
  }

  test ("A regular expression /abcabc/ should not match 'bcabc'") {
    val S  = Factory.re
    val re = S.str("abcabc")
    assert((re matches "bcabc") == false)
  }

  test ("A sequence regular expression /abc/ should match 'abc'") {
    val S  = Factory.re
    val re = S.seq(S.char('a'), S.seq(S.char('b'), S.seq(S.char('c'), S.empty)))
    assert((re matches "abc") == true)
  }

  test ("A regular expression /a|b/ should match 'a'") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "a") == true)
  }

  test ("A regular expression /a|b/ should match 'b'") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "b") == true)
  }

  test ("A regular expression /a|b/ should not match 'c'") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "c") == false)
  }

  test ("A regular expression /a|b/ should not match 'ab'") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "ab") == false)
  }

  test ("A regular expression /a|b/ should not match 'ba'") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "ba") == false)
  }

  test ("A regular expression /a|b/ should not match 'aa'") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "aa") == false)
  }

  test ("A regular expression /a|b/ should not match 'bb'") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "bb") == false)
  }

  test ("A regular expression /a|b/ should not match empty") {
    val S  = Factory.re
    val re = S.alt(S.char('a'), S.char('b'))
    assert((re matches "") == false)
  }

  test("A regular expression /a*/ should match ''") {
    val S  = Factory.re
    val re = S.closure(S.str("a"))
    assert((re matches "") == true)
  }

  test("A regular expression /a*/ should match 'a'") {
    val S  = Factory.re
    val re = S.closure(S.str("a"))
    assert((re matches "a") == true)
  }

  test("A regular expression /a*/ should match 'aa'") {
    val S  = Factory.re
    val re = S.closure(S.str("a"))
    assert((re matches "aa") == true)
  }

  test("A regular expression /a*/ should match 'aaaaaaaaaaaaaaaa'") {
    val S  = Factory.re
    val re = S.closure(S.str("a"))
    assert((re matches "aaaaaaaaaaaaaaaa") == true)
  }

  test("A regular expression /a*/ should not match 'b'") {
    val S  = Factory.re
    val re = S.closure(S.str("a"))
    assert((re matches "b") == false)
  }

  test("A regular expression /a*/ should not match 'bbbb'") {
    val S  = Factory.re
    val re = S.closure(S.str("a"))
    assert((re matches "bbbb") == false)
  }

  test("A regular expression /(a|b)*/ should match empty character") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "") == true)
  }

  test("A regular expression /(a|b)*/ should match empty string") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.str("a"), S.str("b")))
    assert((re matches "") == true)
  }

  test("A regular expression /(a|b)*/ should match 'a'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "a") == true)
  }

  test("A regular expression /(a|b)*/ should match 'b'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "b") == true)
  }

  test("A regular expression /(a|b)*/ should match 'ab'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "ab") == true)
  }

  test("A regular expression /(a|b)*/ should match 'ba'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "ba") == true)
  }

  test("A regular expression /(a|b)*/ should match 'baab'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "baab") == true)
  }

  test("A regular expression /(a|b)*/ should match 'abba'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "abba") == true)
  }

  test("A regular expression /(a|b)*/ should match 'ababa'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "ababa") == true)
  }

  test("A regular expression /(a|b)*/ should match 'aaaaaaaa'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "aaaaaaaa") == true)
  }

  test("A regular expression /(a|b)*/ should match 'bbbbbbbb'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "bbbbbbbb") == true)
  }

  test("A regular expression /(a|b)*/ should match 'bbbbbbbbaaaaaaaa'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "bbbbbbbbaaaaaaaa") == true)
  }

  test("A regular expression /(a|b)*/ should match 'aaaaaaaabbbbbbbb'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.char('a'), S.char('b')))
    assert((re matches "aaaaaaaabbbbbbbb") == true)
  }

  test("A regular expression /(abc)*/ should match ''") {
    val S  = Factory.re
    val re = S.closure(S.str("abc"))
    assert((re matches "") == true)
  }

  test("A regular expression /(abc)*/ should match 'abc'") {
    val S  = Factory.re
    val re = S.closure(S.str("abc"))
    assert((re matches "abc") == true)
  }

  test("A regular expression /(abc)*/ should not match 'abca'") {
    val S  = Factory.re
    val re = S.closure(S.str("abc"))
    assert((re matches "abca") == false)
  }

  test("A regular expression /(abc)*/ should not match 'abcbc'") {
    val S  = Factory.re
    val re = S.closure(S.str("abc"))
    assert((re matches "abcbc") == false)
  }

  test("A regular expression /(abc)*/ should not match 'abcabca'") {
    val S  = Factory.re
    val re = S.closure(S.str("abc"))
    assert((re matches "abcabca") == false)
  }

  test("A regular expression /(abc)*/ should match 'abcabcabcabcabc'") {
    val S  = Factory.re
    val re = S.closure(S.str("abc"))
    assert((re matches "abcabcabcabcabc") == true)
  }

  test("A regular expression /(abc|d)*/ should match ''") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.str("abc"),S.char('d')))
    assert((re matches "") == true)
  }

  test("A regular expression /(abc|d)*/ should match 'd'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.str("abc"),S.char('d')))
    assert((re matches "d") == true)
  }

  test("A regular expression /(abc|d)*/ should match 'dddabcd'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.str("abc"),S.char('d')))
    assert((re matches "dddabcd") == true)
  }

  test("A regular expression /(abc|d)*/ should not match 'dddabcdab'") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.str("abc"),S.char('d')))
    assert((re matches "dddabcdab") == false)
  }

  test("A regular expression /abc|d|foo/ should match 'abc'") {
    val S  = Factory.re
    val re = S.alt(S.str("abc"), S.alt(S.str("d"), S.str("foo")))
    assert((re matches "abc") == true)
  }

  test("A regular expression /abc|d|foo/ should match 'd'") {
    val S  = Factory.re
    val re = S.alt(S.str("abc"), S.alt(S.str("d"), S.str("foo")))
    assert((re matches "d") == true)
  }

  test("A regular expression /abc|d|foo/ should match 'foo'") {
    val S  = Factory.re
    val re = S.alt(S.str("abc"), S.alt(S.str("d"), S.str("foo")))
    assert((re matches "foo") == true)
  }

  test("A regular expression /(a|b)*x(b|c)qg*/ should match xbq") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.alt(S.char('a'), S.char('b'))),
                   S.seq(S.char('x'), S.seq(S.alt(S.char('b'), S.char('c')),
                                            S.seq(S.char('q'),
                                                  S.closure(S.char('g'))))))
    assert((re matches "xbq") == true)
  }

  test("A regular expression /(a|b)*x(b|c)qg*/ should match axbq") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.alt(S.char('a'), S.char('b'))),
                   S.seq(S.char('x'), S.seq(S.alt(S.char('b'), S.char('c')),
                                            S.seq(S.char('q'),
                                                  S.closure(S.char('g'))))))
    assert((re matches "axbq") == true)
  }

  test("A regular expression /(a|b)*x(b|c)qg*/ should match xcq") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.alt(S.char('a'), S.char('b'))),
                   S.seq(S.char('x'), S.seq(S.alt(S.char('b'), S.char('c')),
                                            S.seq(S.char('q'),
                                                  S.closure(S.char('g'))))))
    assert((re matches "xcq") == true)
  }

  test("A regular expression /(a|b)*x(b|c)qg*/ should match aaabbxbqggg") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.alt(S.char('a'), S.char('b'))),
                   S.seq(S.char('x'), S.seq(S.alt(S.char('b'), S.char('c')),
                                            S.seq(S.char('q'),
                                                  S.closure(S.char('g'))))))
    assert((re matches "aaabbxbqggg") == true)
  }

  test("A regular expression /(abc|c*)*/ should match abcabcccc") {
    val S  = Factory.re
    val re = S.closure(S.alt(S.str("abc"), S.closure(S.char('c'))))
    assert((re matches "abcabcccc") == true)
  }

}