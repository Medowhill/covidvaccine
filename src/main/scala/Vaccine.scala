import org.joda.time.LocalDate

enum Vaccine:
  case Unvaccinated(reason: UnvaccinatedReason)
  case Ongoing(fst: LocalDate, snd: LocalDate)
  case Completed(fst: LocalDate, snd: Option[LocalDate])

  def unvaccinated: Boolean = isInstanceOf[Unvaccinated]
  def ongoing: Boolean = isInstanceOf[Ongoing]
  def completed: Boolean = isInstanceOf[Completed]

enum UnvaccinatedReason:
  case NoPlan
  case Planning
  case Scheduled

object UnvaccinatedReason:
  def parse(s: String): UnvaccinatedReason = s.charAt(0) match
    case '1' => NoPlan
    case '2' => Planning
    case '3' => Scheduled
    case _   => unreachable

enum Error:
  case OngoingFirstDate(s: String)
  case OngoingSecondDate(s: String)
  case OngoingOrder(d1: LocalDate, d2: LocalDate)
  case OngoingFuture(d1: LocalDate, d2: LocalDate)
  case CompletedFirstDate(s: String)
  case CompletedSecondDate(s: String)
  case CompletedOrder(d1: LocalDate, d2: LocalDate)
  case CompletedFuture(d1: LocalDate, d2: LocalDate)

  override def toString: String = this match
    case OngoingFirstDate(s)     => s"1차 접종일 '$s' 처리 불가"
    case OngoingSecondDate(s)    => s"2차 예정일 '$s' 처리 불가"
    case OngoingOrder(d1, d2)    => s"1차 접종일 ${d1}이 2차 예정일 ${d2} 이후"
    case OngoingFuture(d1, d2)   => s"1차 접종일 ${d1}이 오늘(${d2}) 이후"
    case CompletedFirstDate(s)   => s"1차 접종일 '$s' 처리 불가"
    case CompletedSecondDate(s)  => s"2차 접종일 '$s' 처리 불가"
    case CompletedOrder(d1, d2)  => s"1차 접종일 ${d1}이 2차 접종일 ${d2} 이후"
    case CompletedFuture(d1, d2) => s"2차 접종일 ${d1}이 오늘(${d2}) 이후"
