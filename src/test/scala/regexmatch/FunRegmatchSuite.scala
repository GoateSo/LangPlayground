package regexmatch

import munit.FunSuite

class FunRegmatchSuite extends FunSuite:
  test("single literal char match") {
    val matcher = new FunRegexMatcher("a")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("aa"), false)
  }
  test("concatenation of literal char match") {
    val matcher = new FunRegexMatcher("ab")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("abaaa"), false)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test("concatenation of literal char match with wildcard") {
    val matcher = new FunRegexMatcher("a.b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("axb"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test("alternation of literal chars (2+ cases)") {
    val matcher = new FunRegexMatcher("(a|b)")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("c"), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("ba"), false)
    val mat2 = new FunRegexMatcher("(a|b|c)")
    assertEquals(mat2.matches(""), false)
    assertEquals(mat2.matches("a"), true)
    assertEquals(mat2.matches("b"), true)
    assertEquals(mat2.matches("c"), true)
    assertEquals(mat2.matches("d"), false)
    assertEquals(mat2.matches("ab"), false)
    assertEquals(mat2.matches("ba"), false)
    assertEquals(mat2.matches("ac"), false)
  }
  test("alternation of groups of literal chars (2+ cases)") {
    val matcher = new FunRegexMatcher("(abba|baab)")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("abba"), true)
    assertEquals(matcher.matches("baab"), true)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("caac"), false)
    assertEquals(matcher.matches("abbc"), false)
    assertEquals(matcher.matches("abbababb"), false)
    val mat2 = new FunRegexMatcher("(abba|baab|abab)")
    assertEquals(mat2.matches(""), false)
    assertEquals(mat2.matches("abba"), true)
    assertEquals(mat2.matches("baab"), true)
    assertEquals(mat2.matches("abab"), true)
    assertEquals(mat2.matches("ab"), false)
    assertEquals(mat2.matches("caac"), false)
    assertEquals(mat2.matches("abbc"), false)
    assertEquals(mat2.matches("abbababb"), false)
  }
  test("kleene star with literal char") {
    val matcher = new FunRegexMatcher("a*")
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("aaa"), true)
    assertEquals(matcher.matches("aaaab"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches(""), true)

  }
  test("grouped kleene star") {
    val matcher = new FunRegexMatcher("(ab)*")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("abab"), true)
    assertEquals(matcher.matches("ababab"), true)
    assertEquals(matcher.matches("ababababa"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test("sandwiched kleene star") {
    val matcher = new FunRegexMatcher("a.*b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("axb"), true)
    assertEquals(matcher.matches("ajgwofehroebvrxb"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test("kleene star for alternative (2+ cases)") {
    val matcher = new FunRegexMatcher("(aa|b)*")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("baa"), true)
    assertEquals(matcher.matches("baabaabbaaaa"), true)
    assertEquals(matcher.matches("baabaabbaaaaa"), false)
    assertEquals(matcher.matches("baabaabbabaaa"), false)
    val mat2 = new FunRegexMatcher("(aa|b|c)*")
    assertEquals(mat2.matches(""), true)
    assertEquals(mat2.matches("aa"), true)
    assertEquals(mat2.matches("b"), true)
    assertEquals(mat2.matches("c"), true)
    assertEquals(mat2.matches("aab"), true)
    assertEquals(mat2.matches("baa"), true)
    assertEquals(mat2.matches("baabaabbaacaac"), true)
    assertEquals(mat2.matches("baabaabbaaaaca"), false)
    assertEquals(mat2.matches("baabacabbabaaa"), false)
  }
  test("optional value") {
    val matcher = new FunRegexMatcher("a?")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test("optional value with kleene star") {
    val matcher = new FunRegexMatcher("a*b?")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("ab"), true)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("ba"), false)
    assertEquals(matcher.matches("baa"), false)
  }
  test("sandwiched optional value") {
    val matcher = new FunRegexMatcher("ba?b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("bb"), true)
    assertEquals(matcher.matches("bab"), true)
    assertEquals(matcher.matches("baab"), false)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test("optional value outside alternation ") {
    val matcher = new FunRegexMatcher("(a|b)?")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("aa"), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("ba"), false)
  }
  test("optional value inside alternation") {
    val matcher = new FunRegexMatcher("(a?|b)")
    assertEquals(matcher.matches(""), true)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("b"), true)
    assertEquals(matcher.matches("aa"), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("ba"), false)

  }
  test("kleene plus on value (with partials)") {
    val matcher = new FunRegexMatcher("a+")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("a"), true)
    assertEquals(matcher.matches("aa"), true)
    assertEquals(matcher.matches("aaa"), true)
    assertEquals(matcher.matches("b"), false)
    assertEquals(matcher.matches("ba"), false)
    // partial matches (at start)
    assertEquals(matcher.matches("ab", true), true)
    assertEquals(matcher.matches("aab", true), true)
    assertEquals(matcher.matches("baa", true), false)
    // partial matches (general)
    assertEquals(matcher.finds("ab"), true)
    assertEquals(matcher.finds("aab"), true)
    assertEquals(matcher.finds("baa"), true)
    assertEquals(matcher.finds("bbbb"), false)
  }
  test("kleene plus in sandwich (with partials)") {
    val matcher = new FunRegexMatcher("a.+b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("aab"), true)
    assertEquals(matcher.matches("axb"), true)
    assertEquals(matcher.matches("ajgwofehroebvrxb"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
    // partial matches (at start)
    assertEquals(matcher.matches("axblog", true), true)
    assertEquals(matcher.matches("ajgwofehroebvrxbtrog", true), true)
    assertEquals(matcher.matches("laajgwofehroebvrxb", true), false)
    // partial matches (general)
    assertEquals(matcher.finds("axblog"), true)
    assertEquals(matcher.finds("logaxb"), true)
    assertEquals(matcher.finds("logbxalog"), false)
  }
  test("two kleene plus in sandwich") {
    val matcher = new FunRegexMatcher("a.+a+b")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("ab"), false)
    assertEquals(matcher.matches("aab"), false)
    assertEquals(matcher.matches("axab"), true)
    assertEquals(matcher.matches("ajgwofehroebvrxb"), false)
    assertEquals(matcher.matches("ajgwofehroebvrxaaaaaab"), true)
    assertEquals(matcher.matches("a"), false)
    assertEquals(matcher.matches("b"), false)
  }
  test("mix of all features") {
    val matcher = new FunRegexMatcher("a?(cd|dc)+b*")
    assertEquals(matcher.matches(""), false)
    assertEquals(matcher.matches("cd"), true)
    assertEquals(matcher.matches("dc"), true)
    assertEquals(matcher.matches("cdcd"), true)
    assertEquals(matcher.matches("adcdc"), true)
    assertEquals(matcher.matches("adcdcb"), true)
    assertEquals(matcher.matches("adccdbbbb"), true)
    assertEquals(matcher.matches("adccdbbbba"), false)
    assertEquals(matcher.matches("adccdbbbbc"), false)
    assertEquals(matcher.matches("adcdbbbbc"), false)
    assertEquals(matcher.matches("aadcb"), false)
    // partials (at start)
    assertEquals(matcher.matches("adccdbbbbc", true), true)
    assertEquals(matcher.matches("adccdbbbba", true), true)
    assertEquals(matcher.matches("acdb", true), true)
    assertEquals(matcher.matches("babcdbdcb", true), false)
    // partials (general)
    assertEquals(matcher.finds("xxadccdbbbbcxx"), true)
    assertEquals(matcher.finds("xxadccdbbbba"), true)
    assertEquals(matcher.finds("xxacxdb"), false)
    assertEquals(matcher.finds("xxadccdbbbbcxxadccdbbbbcxx"), true)
  }
  test("error on unmatched Lparam") {
    intercept[AssertionError] {
      val matcher = new FunRegexMatcher("(ab")
    }
  }
  test("error on unmatched Rparam") {
    intercept[Exception] {
      val matcher = new FunRegexMatcher("ab)")
    }
  }
  test("error on unmatched Lbracket") {
    intercept[Exception] {
      val matcher = new FunRegexMatcher("[aa")
    }
  }
  test("error on unmatched Rbracket") {
    intercept[Exception] {
      val matcher = new FunRegexMatcher("ab]")
    }
  }
  test("charsets and char ranges") {
    val mat = new FunRegexMatcher("[abc][xyz]")
    assertEquals(
      List("ax", "ay", "az", "bx", "by", "bz", "cx", "cy", "cz")
        .map(mat.matches(_))
        .forall(identity),
      true
    )
    assertEquals(mat.matches("aa"), false)
    assertEquals(mat.matches("xb"), false)
    val matcher = new FunRegexMatcher("[a-z]+")
    assertEquals(matcher.matches("abc"), true)
    assertEquals(matcher.matches("abc123"), false)
    assertEquals(matcher.matches("ABC"), false)
    val matcher2 = new FunRegexMatcher("[a-zA-Z]+")
    assertEquals(matcher2.matches("abc"), true)
    assertEquals(matcher2.matches("abc123"), false)
    assertEquals(matcher2.matches("ABC"), true)
    val matcher3 = new FunRegexMatcher("[a-zA-Z0-9]+")
    assertEquals(matcher3.matches("abc"), true)
    assertEquals(matcher3.matches("abc123"), true)
    assertEquals(matcher3.matches("ABC"), true)
    assertEquals(matcher3.matches("123^"), false)
    val matcher4 = new FunRegexMatcher("[^abc]+")
    assertEquals(matcher4.matches("abc"), false)
    assertEquals(matcher4.matches("123"), true)
    assertEquals(matcher4.matches("123abc"), false)
    assertEquals(matcher4.matches("knsdofij56132^"), true)
    val matcher5 = new FunRegexMatcher("a+[A-Z]*1?")
    assertEquals(matcher5.matches("a"), true)
    assertEquals(matcher5.matches("a1"), true)
    assertEquals(matcher5.matches("aa1"), true)
    assertEquals(matcher5.matches("aa2"), false)
    assertEquals(matcher5.matches("aaab1"), false)
    assertEquals(matcher5.matches("aKYA1"), true)
    assertEquals(matcher5.matches("aKYa1"), false)
  }
