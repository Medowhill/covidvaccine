sealed trait Person:
  def name: String
  def number: String
  def dept: String
  def phone: String
  def email: String
  def active: Boolean
  def role: String
  def getData: List[String]

given Ordering[Person] with
  def compare(x: Person, y: Person): Int =
    val a = if (x.isInstanceOf[Employee]) 0 else 1
    val b = if (y.isInstanceOf[Employee]) 0 else 1
    implicitly[Ordering[(Int, String)]].compare((a, x.number), (b, y.number))

case class Student(
    name: String,
    sn: String,
    dept: String,
    phone: String,
    email: String,
    active: Boolean
) extends Person:
  override def equals(obj: Any): Boolean = obj match
    case that: Student => this.sn == that.sn
    case _             => false

  override def hashCode: Int = sn.##

  def number: String = sn

  def role: String =
    val state = if (active) "재학" else "휴학"
    val course = sn.charAt(sn.length - 4) match
      case '0' | '1' | '2' => "학부생"
      case _               => "대학원생"
    s"$course($state)"

  def getData: List[String] = List(name, "", sn, email)

object Student:
  def parse(row: List[String]): Student =
    val _ :: name :: sn :: dept :: phone :: email :: state :: Nil = row
    val active = state match {
      case "재학" => true
      case "휴학" => false
      case _    => unreachable
    }
    Student(name, sn, dept, phone, email, active)

given Ordering[Student] with
  def compare(x: Student, y: Student): Int =
    implicitly[Ordering[String]].compare(x.sn, y.sn)

case class Employee(
    name: String,
    en: String,
    dept: String,
    phone: String,
    email: String,
    active: Boolean
) extends Person:
  override def equals(obj: Any): Boolean = obj match
    case that: Employee => this.en == that.en
    case _              => false

  override def hashCode: Int = en.##

  def number: String = en

  def role: String =
    val state = if (active) "재직" else "휴직"
    s"교직원($state)"

  def getData: List[String] = List(name, en, "", email)

object Employee:
  def parse(row: List[String]): Employee =
    val _ :: _ :: name :: en :: _ :: dept :: phone :: email :: state :: Nil =
      row
    val active = state match {
      case "재직" => true
      case "휴직" => false
      case _    => unreachable
    }
    Employee(name, en, dept, phone, email, active)

given Ordering[Employee] with
  def compare(x: Employee, y: Employee): Int =
    implicitly[Ordering[String]].compare(x.en, y.en)
