package csp.ch03

import fastparse.all._

// From SBT: ~run-main csp.ch03.Ex01

object Ex01 {
  def test (p : Parser[Unit], s : String) : Unit = {
    val result : fastparse.core.Parsed[Unit, Char, String] = p.parse (s) 
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

    // Just "hello".
    val p01 : Parser[Unit] = P ("hello")
    test (p01, "hello")
    test (p01, "hellox")
    test (p01, "he")
    test (p01, "xhello")
    println ("=" * 80)

    // "hello" followed by "world" (~ is called SEQUENCING).
    val p02 : Parser[Unit] = P ("hello" ~ "world")
    test (p02, "helloworld")
    test (p02, "hello world")
    test (p02, "hello")
    test (p02, "hellowor")
    println ("=" * 80)

    // "hello" repeated zero or more times, followed by exactly one "world".
    val p03 : Parser[Unit] = P ("hello".rep ~ "world")
    test (p03, "world")
    test (p03, "helloworld")
    test (p03, "hellohelloworld")
    test (p03, "hellohellohelloworld")
    println ("=" * 80)

    // ("hello" followed by "world") repeated zero or more times.
    // NOTE: Look at index carefully!!!
    val p04 : Parser[Unit] = P (("hello" ~ "world").rep)
    test (p04, "world")
    test (p04, "helloworld")
    test (p04, "hellohelloworld")
    test (p04, "hellohelloworldworld")
    test (p04, "helloworldhelloworld")
    println ("=" * 80)

    // (either "hello" or "world") repeated zero or more times.
    // NOTE: Look at index carefully!!!
    val p05 : Parser[Unit] = P (("hello" | "world").rep)
    test (p05, "world")
    test (p05, "helloworld")
    test (p05, "hellohelloworld")
    test (p05, "hellohelloworldworld")
    test (p05, "helloworldhelloworld")
    println ("=" * 80)

    // ("hello" followed by an optional "world") repeated zero or more times.
    // NOTE: Look at index carefully!!!
    val p06 : Parser[Unit] = P (("hello" ~ "world".?).rep)
    test (p06, "world")
    test (p06, "helloworld")
    test (p06, "hellohelloworld")
    test (p06, "hellohelloworldworld")
    test (p06, "helloworldhelloworld")
    println ("=" * 80)

    // ("hello" followed by "world") repeated zero or more times but must finish at the end of the string.
    // NOTE: compare with p04 above.
    val p07 : Parser[Unit] = P (("hello" ~ "world").rep ~ End)
    test (p07, "world")
    test (p07, "helloworld")
    test (p07, "hellohelloworld")
    test (p07, "hellohelloworldworld")
    test (p07, "helloworldhelloworld")
    println ("=" * 80)

    val digits : Parser[Unit] = P (CharIn ('0' to '9').rep (1))
    val twoDigits : Parser[Unit] = P (CharIn ('0' to '9').rep (exactly=2))

    val month : Parser[Unit] = P ("Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec")
    val day : Parser[Unit] = digits
    val date : Parser[Unit] = month ~ " " ~ day
    val time : Parser[Unit] = twoDigits ~ ":" ~ twoDigits ~ ":" ~ twoDigits
    val hostname : Parser[Unit] = P (CharIn ('a' to 'z').rep (1))
    val process : Parser[Unit] = P ("sshd" ~ "[" ~ digits ~ "]")
    val message : Parser[Unit] = P (AnyChar.rep)
    val app01 : Parser[Unit] = P (date ~ " " ~ time ~ " " ~ hostname ~ " " ~ process ~ ":" ~ message ~ End)
    test (app01, "Jan 29 07:07:02 reed sshd[15279]: User root from 95-24-31-141.broadband.corbina.ru not allowed because not listed in AllowUsers")
    println ("=" * 80)
  }
}
