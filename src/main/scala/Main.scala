import Vaccine.{Unvaccinated, Ongoing, Completed}

import org.apache.poi.ss.usermodel.{WorkbookFactory, Row, Cell, CellType}
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeComparator, LocalDate}

import java.io.File
import File.separator as SEP

import scala.jdk.CollectionConverters._

lazy val unreachable = sys.error("unreachable")

@main def main(fn1: String, fn2: String, fn3: String): Unit =
  def rows(fn: String) = getRows(resolveHome(fn)).tail.map(getStrings)
  val people = (rows(fn1) ++ rows(fn2) ++ rows(fn3)).map(Person.apply)

  println(people.length)
  println()

  for
    p <- people;
    e <- p.vaccine.swap.toOption
  yield println(s"${p.name}: $e")
  println()

  val dups = people.groupBy(_.nums).filter { case (_, v) => v.length > 1 }
  for
    (_, v) <- dups;
    p <- v
  yield println(s"${p.name}: ${p.vaccine}")

def resolveHome(fn: String): String =
  if (fn.startsWith(s"~$SEP"))
    fn.replaceFirst("~", System.getProperty("user.home"))
  else
    fn

def getRows(name: String): List[Row] =
  val workbook = WorkbookFactory.create(new File(name))
  val res = workbook.getSheetAt(0).iterator.asScala.toList
  workbook.close()
  res

def getStrings(row: Row): List[String] =
  row.iterator.asScala.toList.map(getString)

def getString(c: Cell): String =
  if (c == null) ""
  else
    val s = c.getCellType match
      case CellType.NUMERIC => c.getNumericCellValue.toLong.toString
      case CellType.FORMULA => c.getCellFormula
      case _                => c.getStringCellValue
    s.trim.filterNot(_.isControl)

val formatter = DateTimeFormat.forPattern("yyyyMMdd")
val comparator = DateTimeComparator.getDateOnlyInstance
val today = LocalDate.now

extension (d1: LocalDate)
  def compare(d2: LocalDate): Int =
    comparator.compare(d1.toDateTimeAtStartOfDay, d2.toDateTimeAtStartOfDay)
  def <(d2: LocalDate): Boolean = (d1 compare d2) < 0
  def <=(d2: LocalDate): Boolean = (d1 compare d2) <= 0
  def >(d2: LocalDate): Boolean = (d1 compare d2) > 0
  def >=(d2: LocalDate): Boolean = (d1 compare d2) >= 0

def parseDate(s: String, f: String => Error): Either[Error, LocalDate] =
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
      opt.map { case (m, d) => f"2021${m.toInt}%02d${d.toInt}%02d" }

    val ns =
      if (allDigit && s.length == 8) s
      else if (allDigit && s.length == 6 && s.startsWith("21")) s"20$s"
      else if (splitted.nonEmpty) splitted.get
      else if (ds.length == 8) ds
      else if (zsplitted.nonEmpty) zsplitted.get
      else if (
        ds.length == 9 && (ds.startsWith("20210") || ds.startsWith("20201"))
      ) s"2021${s.substring(5)}"
      else ds
    val d = formatter.parseLocalDate(ns)
    assert(formatter.print(d) == ns)
    Right(d.withYear(2021))
  catch case _: Exception => Left(f(s))

case class Person(
    pid: String,
    name: String,
    en: String,
    sn: String,
    deptCode: String,
    dept: String,
    phone: String,
    email: String,
    vaccine: Either[Error, Vaccine]
):
  def nums = (en, sn)

object Person:
  def checkOrder(
      d1: LocalDate,
      d2: LocalDate,
      f: (LocalDate, LocalDate) => Error
  ): Either[Error, Unit] =
    if (d1 < d2) Right(()) else Left(f(d1, d2))

  def apply(row: List[String]): Person =
    val pid :: name :: en :: sn :: deptCode :: dept :: phone :: email :: tl =
      row
    val vaccine = tl match
      case s :: Nil => UnvaccinatedReason.parse(s).map(Unvaccinated.apply)
      case s1 :: _ :: s2 :: Nil =>
        for
          d1 <- parseDate(s1, Error.OngoingFirstDate.apply);
          d2 <- parseDate(s2, Error.OngoingSecondDate.apply);
          _ <- checkOrder(d1, d2, Error.OngoingOrder.apply)
        yield Ongoing(d1, d2)
      case s1 :: _ :: s2 :: _ :: Nil =>
        def _d2 =
          if (s2.toLowerCase.filter(_.isLetter).contains("na")) Right(None)
          else parseDate(s2, Error.CompletedSecondDate.apply).map(Some.apply)
        for
          d1 <- parseDate(s1, Error.CompletedFirstDate.apply);
          d2 <- _d2;
          _ <- d2
            .map(checkOrder(d1, _, Error.CompletedOrder.apply))
            .getOrElse(Right(()))
        yield Completed(d1, d2)
      case _ => unreachable
    Person(pid, name, en, sn, deptCode, dept, phone, email, vaccine)

enum Vaccine:
  case Unvaccinated(reason: UnvaccinatedReason)
  case Ongoing(fst: LocalDate, snd: LocalDate)
  case Completed(fst: LocalDate, snd: Option[LocalDate])

enum UnvaccinatedReason:
  case NoPlan
  case Planning
  case Scheduled(date: LocalDate)

object UnvaccinatedReason:
  def parse(s: String): Either[Error, UnvaccinatedReason] = s.charAt(0) match
    case '1' => Right(NoPlan)
    case '2' => Right(Planning)
    case '3' =>
      if (s.length == 11)
        parseDate(s.substring(2, s.length - 1), Error.ScheduledDate.apply)
          .map(Scheduled.apply)
      else Left(Error.ScheduledDate(s))
    case _ => unreachable

enum Error:
  case ScheduledDate(s: String)
  case OngoingFirstDate(s: String)
  case OngoingSecondDate(s: String)
  case OngoingOrder(d1: LocalDate, d2: LocalDate)
  case CompletedFirstDate(s: String)
  case CompletedSecondDate(s: String)
  case CompletedOrder(d1: LocalDate, d2: LocalDate)
