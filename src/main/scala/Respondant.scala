import Vaccine.{Unvaccinated, Ongoing, Completed}

import org.joda.time.LocalDate

case class Respondant(
    pid: String,
    name: String,
    en: String,
    sn: String,
    deptCode: String,
    dept: String,
    phone: String,
    email: String
):
  def nums = (en, sn)

  override def equals(obj: Any): Boolean = obj match
    case that: Respondant => this.nums == that.nums
    case _                => false

  override def hashCode: Int = nums.##

object Respondant:
  val today = LocalDate.now

  def parseOngoing(s1: String, s2: String): Either[Error, Vaccine] =
    for
      d1 <- parseDate(s1, Error.OngoingFirstDate.apply)
      d2 <- parseDate(s2, Error.OngoingSecondDate.apply)
      _ <- checkOrder(d1, d2, Error.OngoingOrder.apply, true)
      _ <- checkOrder(d1, today, Error.OngoingFuture.apply)
    yield Ongoing(d1, d2)

  def parseCompleted(s1: String, s2: String): Either[Error, Vaccine] =
    val d1 = parseDate(s1, Error.CompletedFirstDate.apply)
    val s2f = s2.toLowerCase.filter(_.isLetter)
    if (s2f.contains("na") || s2f.contains("얀센"))
      for
        d1 <- d1
        _ <- checkOrder(d1, today, Error.CompletedFuture.apply)
      yield Completed(d1, None)
    else
      for
        d1 <- d1
        d2 <- parseDate(s2, Error.CompletedSecondDate.apply)
        _ <- checkOrder(d1, d2, Error.CompletedOrder.apply, true)
        _ <- checkOrder(d2, today, Error.CompletedFuture.apply)
      yield Completed(d1, Some(d2))

  def parse(
      row: List[String]
  ): (Respondant, Either[Error, Vaccine], List[String]) =
    val pid :: name :: en :: sn :: deptCode :: dept :: phone :: email :: tl =
      row
    val p = Respondant(pid, name, en, sn, deptCode, dept, phone, email)
    val v = tl match
      case s :: Nil => Right(Unvaccinated(UnvaccinatedReason.parse(s)))
      case s1 :: _ :: s2 :: Nil      => parseOngoing(s1, s2)
      case s1 :: _ :: s2 :: _ :: Nil => parseCompleted(s1, s2)
      case _                         => unreachable
    (p, v, tl)

  def parse2(
      row: List[String]
  ): (Respondant, Either[Error, Vaccine], List[String]) =
    val name :: en :: sn :: email :: tl = row
    val p = Respondant("", name, en, sn, "", "", "", email)
    val v = tl match
      case "" :: "" :: "" :: "" :: "" :: "" :: "" :: s :: _ =>
        Right(Unvaccinated(UnvaccinatedReason.parse(s)))
      case "" :: "" :: "" :: s1 :: _ :: s2 :: _ => parseCompleted(s1, s2)
      case s1 :: _ :: s2 :: _                   => parseOngoing(s1, s2)
      case l                                    => println(p); unreachable
    (p, v, tl.filter(_.nonEmpty))

given Ordering[Respondant] with
  def compare(x: Respondant, y: Respondant): Int =
    implicitly[Ordering[(String, String)]].compare((x.sn, x.en), (y.sn, y.en))
