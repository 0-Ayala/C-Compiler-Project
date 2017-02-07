package csp.ch03

import fastparse.all._

// From SBT: ~run-main csp.ch03.Ex02

object Ex02 {
  def test [X] (p : Parser[X], s : String) : Unit = {
    val result : fastparse.core.Parsed[X, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    // Use ".!" to CAPTURE a value.
    val p01 : Parser[String] = P ("hello".!)
    test (p01, "hello")
    test (p01, "hellox")
    test (p01, "he")
    test (p01, "xhello")
    println ("=" * 80)

    // SEQUENCING gives a pair of values.
    val p02 : Parser[(String, String)] = P ("hello".! ~ "world".!)
    test (p02, "helloworld")
    test (p02, "hello world")
    test (p02, "hello")
    test (p02, "hellowor")
    println ("=" * 80)

    // REPEATED "hello" occurrences can be captured as a single string.
    val p03 : Parser[(String, String)] = P ("hello".rep.! ~ "world".!)
    test (p03, "world")
    test (p03, "helloworld")
    test (p03, "hellohelloworld")
    test (p03, "hellohellohelloworld")
    println ("=" * 80)

    // Alternatively, REPEATED "hello" occurrences can be captured individually and then returned as a Seq (similar to a List).
    val p03b : Parser[(Seq[String], String)] = P ("hello".!.rep ~ "world".!)
    test (p03b, "world")
    test (p03b, "helloworld")
    test (p03b, "hellohelloworld")
    test (p03b, "hellohellohelloworld")
    println ("=" * 80)

    // Capture ("hello" followed by "world") repeated zero or more times.
    val p04 : Parser[Seq[(String,String)]] = P (("hello".! ~ "world".!).rep)
    test (p04, "world")
    test (p04, "helloworld")
    test (p04, "hellohelloworld")
    test (p04, "hellohelloworldworld")
    test (p04, "helloworldhelloworld")
    println ("=" * 80)

    // Alternatively, capture "hello" and "world" as a single string.
    val p04b : Parser[Seq[String]] = P (("hello" ~ "world").!.rep)
    test (p04b, "world")
    test (p04b, "helloworld")
    test (p04b, "hellohelloworld")
    test (p04b, "hellohelloworldworld")
    test (p04b, "helloworldhelloworld")
    println ("=" * 80)

    // Alternatively, capture the entire string.
    val p04c : Parser[String] = P (("hello" ~ "world").rep.!)
    test (p04c, "world")
    test (p04c, "helloworld")
    test (p04c, "hellohelloworld")
    test (p04c, "hellohelloworldworld")
    test (p04c, "helloworldhelloworld")
    println ("=" * 80)

    // Capture (EITHER "hello" or "world") repeated zero or more times.
    val p05 : Parser[Seq[String]] = P (("hello".! | "world".!).rep)
    test (p05, "world")
    test (p05, "helloworld")
    test (p05, "hellohelloworld")
    test (p05, "hellohelloworldworld")
    test (p05, "helloworldhelloworld")
    println ("=" * 80)

    // Capture ("hello" followed by an optional "world") repeated zero or more times.
    val p06 : Parser[Seq[(String, Option[String])]] = P (("hello".! ~ "world".!.?).rep)
    test (p06, "world")
    test (p06, "helloworld")
    test (p06, "hellohelloworld")
    test (p06, "hellohelloworldworld")
    test (p06, "helloworldhelloworld")
    println ("=" * 80)

    // ("hello" followed by "world") repeated zero or more times but must finish at the end of the string.
    // NOTE: compare with p04 above.
    val p07 : Parser[Seq[(String,String)]] = P (("hello".! ~ "world".!).rep ~ End)
    test (p07, "world")
    test (p07, "helloworld")
    test (p07, "hellohelloworld")
    test (p07, "hellohelloworldworld")
    test (p07, "helloworldhelloworld")
    println ("=" * 80)

    val digitsAsText : Parser[String] = P (CharIn ('0' to '9').rep (1).!)
    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)

    val twoDigits : Parser[Int] = P (CharIn ('0' to '9').rep (exactly=2).!).map (s => s.toInt)

    val month : Parser[String] = P (("Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec").!)
    val day : Parser[Int] = digits
    val date : Parser[(String,Int)] = month ~ " " ~ day
    val time : Parser[(Int,Int,Int)] = twoDigits ~ ":" ~ twoDigits ~ ":" ~ twoDigits
    val hostname : Parser[String] = P (CharIn ('a' to 'z').rep (1).!)
    val process : Parser[(String,Int)] = P ("sshd".! ~ "[" ~ digits ~ "]")
    val message : Parser[String] = P (AnyChar.rep.!)
    val app01 : Parser[(String,Int,(Int,Int,Int),String,(String,Int),String)] = P (date ~ " " ~ time ~ " " ~ hostname ~ " " ~ process ~ ":" ~ message ~ End)
    test (app01, "Jan 29 07:07:02 reed sshd[15279]: User root from 95-24-31-141.broadband.corbina.ru not allowed because not listed in AllowUsers")
    println ("=" * 80)
  }
}
