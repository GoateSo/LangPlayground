package regexmatch

import munit.FunSuite

class RegmatchSuite extends FunSuite:
  test ("single literal char match") {
    val matcher = new RegexMatcher("a")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("aa"), false)
  }
  test ("concatenation of literal char match") {
    val matcher = new RegexMatcher("ab")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("abaaa"), false)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("concatenation of literal char match with wildcard") {
    val matcher = new RegexMatcher("a.b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("axb"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("alternation of literal chars") {
    val matcher = new RegexMatcher("(a|b)")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("c"), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("alternation of groups of literal chars") {
    val matcher = new RegexMatcher("(abba|baab)")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("abba"), true)
    assertEquals(matcher.matches("baab"), true)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("caac"), false)
    assertEquals(matcher.matches("abbc"), false)
    assertEquals(matcher.matches("abbababb"), false)
  }
  test ("kleene star with literal char") {
    val matcher = new RegexMatcher("a*")
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("aaa"), true)
    assertEquals(matcher.matches("aaaab"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches(""),true)
  
  }
  test ("grouped kleene star") {
    val matcher = new RegexMatcher("(ab)*")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("abab"), true)
    assertEquals(matcher.matches("ababab"), true)
    assertEquals(matcher.matches("ababababa"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("sandwiched kleene star") {
    val matcher =  new RegexMatcher("a.*b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("axb"), true)
    assertEquals(matcher.matches("ajgwofehroebvrxb"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("kleene star for alternative") {
    val matcher = new RegexMatcher("(aa|b)*")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("baa"), true)
    assertEquals(matcher.matches("baabaabbaaaa"), true)
    assertEquals(matcher.matches("baabaabbaaaaa"), false)
    assertEquals(matcher.matches("baabaabbabaaa"), false)
  }
  test ("optional value") {
    val matcher = new RegexMatcher("a?")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("optional value with kleene star") {
    val matcher = new RegexMatcher("a*b?")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("ba"), false)
    assertEquals(matcher.matches("baa"), false)
  }
  test ("sandwiched optional value"){
    val matcher = new RegexMatcher("ba?b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("bb"), true)
    assertEquals(matcher.matches("bab"), true)
    assertEquals(matcher.matches("baab"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("optional value outside alternation ") {
    val matcher = new RegexMatcher("(a|b)?")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("aa"), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("optional value inside alternation") {
    val matcher = new RegexMatcher("(a?|b)")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("aa"), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("kleene plus on value"){
    val matcher = new RegexMatcher("a+")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("aaa"), true)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test ("kleene plus in sandwich") {
    val matcher = new RegexMatcher("a.+b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("axb"), true)
    assertEquals(matcher.matches("ajgwofehroebvrxb"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
  }
  test ("two kleene plus in sandwich") {
    val matcher = new RegexMatcher("a.+a+b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("aab"), false)
    assertEquals(matcher.matches("axab"), true)
    assertEquals(matcher.matches("ajgwofehroebvrxb"), false)
    assertEquals(matcher.matches("ajgwofehroebvrxaaaaaab"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
  }