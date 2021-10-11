import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeComparator, LocalDate}

val typos = Set("20210", "20201", "20121", "22021", "20221")

def parseDate(s: String, f: String => Error): Either[Error, LocalDate] =
  val formatter = DateTimeFormat.forPattern("yyyyMMdd")
  try
    val allDigit = s.forall(_.isDigit)
    val R = "(\\d+)".r
    val splitted =
      s.split("[^\\d]").filter(_.nonEmpty) match
        case Array("2021" | "21", R(m), R(d)) =>
          Some(f"2021${m.toInt}%02d${d.toInt}%02d")
        case _ => None
    val ds = s.filter(_.isDigit)
    val zsplitted =
      val opt = ds.split("0").filter(_.nonEmpty).toList match
        case "2" :: "21" :: m :: d :: Nil       => Some((m, d))
        case "2" :: "2" :: "1" :: m :: d :: Nil => Some((m, d))
        case _                                  => None
      opt.map((m, d) => f"2021${m.toInt}%02d${d.toInt}%02d")
    val Md = "(\\d+)월 *(\\d+)일.*".r
    val kor = s match {
      case Md(m, d) => Some(f"2021${m.toInt}%02d${d.toInt}%02d")
      case _        => None
    }

    val ns =
      if (allDigit && s.length == 8) s
      else if (allDigit && s.length == 6 && s.startsWith("21")) s"20$s"
      else if (splitted.nonEmpty) splitted.get
      else if (ds.length == 8) ds
      else if (zsplitted.nonEmpty) zsplitted.get
      else if (kor.nonEmpty) kor.get
      else if (ds.length == 9 && typos(ds.substring(0, 5)))
        s"2021${s.substring(5)}"
      else ds.substring(0, 8)
    val d = formatter.parseLocalDate(ns)
    assert(formatter.print(d) == ns)
    Right(d.withYear(2021))
  catch _ => Left(f(s))

def checkOrder(
    d1: LocalDate,
    d2: LocalDate,
    f: (LocalDate, LocalDate) => Error,
    strict: Boolean = false
): Either[Error, Unit] =
  if (if (strict) d1 < d2 else d1 <= d2) Right(()) else Left(f(d1, d2))

extension (d1: LocalDate)
  def compare(d2: LocalDate): Int =
    val comparator = DateTimeComparator.getDateOnlyInstance
    comparator.compare(d1.toDateTimeAtStartOfDay, d2.toDateTimeAtStartOfDay)

  def <(d2: LocalDate): Boolean = (d1 compare d2) < 0
  def <=(d2: LocalDate): Boolean = (d1 compare d2) <= 0
  def >(d2: LocalDate): Boolean = (d1 compare d2) > 0
  def >=(d2: LocalDate): Boolean = (d1 compare d2) >= 0
