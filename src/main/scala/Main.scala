import org.apache.commons.io.FileUtils

import java.io.File
import File.separator as SEP

import scala.jdk.CollectionConverters._
import scala.util.chaining._

lazy val unreachable = sys.error("unreachable")

@main def main(
    fn1: String,
    fn2: String,
    fn3: String,
    fnSt: String,
    stPw: String,
    fnEm: String,
    emPw: String,
    dnEx: String
): Unit =
  val dirEx = File(dnEx)
  require(dirEx.isDirectory)
  val exs = FileUtils
    .listFiles(dirEx, Array("xlsx"), true)
    .asScala
    .filter(!_.getName.startsWith("~"))
    .flatMap(_.getAbsolutePath.pipe(getRows(_, null)))
    .map(getStringsWithColors(_)(1))
    .collect { case (n, c) if c == "FFFFFF00" => n }
    .toSet

  val students = rows(fnSt, stPw).map(Student.parse)
  val employees = rows(fnEm, emPw).map(Employee.parse).filterNot(_.en.pipe(exs))
  val people = (students ++ employees).sorted

  val pevs = (rows(fn3) ++ rows(fn2) ++ rows(fn1))
    .map(Respondant.parse)
    .groupBy(_._1)
    .map((p, l) => p -> (l.map(_._2), l.map(_._3)))
    .toList
    .sortBy(_._1)

  val pvs = pevs.collect { case (p, (Right(v) :: _, _)) => p -> v }
  val pes = pevs.filter(_._2._1.head.isLeft)

  println(s"총 응답자 수: ${pevs.length}명")
  println(s"유효 응답자 수: ${pvs.length}명")
  println(s"잘못된 응답자 수: ${pes.length}명")

  val vMap = pvs.flatMap { (p, v) =>
    (if (p.sn.nonEmpty) List(p.sn -> v) else Nil) ++
      (if (p.en.nonEmpty) List(p.en -> v) else Nil)
  }.toMap
  val eSet = pes.flatMap { (p, _) =>
    (if (p.sn.nonEmpty) List(p.sn) else Nil) ++
      (if (p.en.nonEmpty) List(p.en) else Nil)
  }.toSet
  val depts = people.groupBy(_.dept).toList.sortBy(_._1).map { (d, p) =>
    Group(d, p, vMap, eSet)
  }
  val states = people.groupBy(_.role).toList.sortBy(_._1).map { (r, p) =>
    Group(r, p, vMap, eSet)
  }
  val header =
    List(
      "총 인원",
      "미응답자",
      "미응답자(재직/재학)",
      "잘못된 응답자",
      "유효 응답자",
      "미접종자",
      "부분 완료자",
      "완료자",
      "미접종자 비율",
      "부분 완료자 비율",
      "완료자 비율"
    )

  val errors = pes.map { case (p, (es, is)) =>
    val i2 = is.find(_.length == 3).getOrElse(List("", "", ""))
    val i3 = is.find(_.length == 4).getOrElse(List("", "", "", ""))
    val e = es.collect { case Left(e) => e.toString }.mkString(", ")
    List(p.name, p.en, p.sn, p.email) ++ i2 ++ i3 :+ e
  }

  writeWorkbook("설문조사 응답 현황.xlsx") { wb =>
    def aux(s: String, l: List[Group])(sheet: SheetWrapper) =
      sheet.addHeaderRows(0, l.length + 1)
      sheet.addPercentCols(9, 10, 11)
      sheet.write(s :: header)
      for d <- l do sheet.write(d.name :: addPercent(d.getStat))
      val total = l
        .map(_.getStat)
        .foldLeft(List.fill(8)(0.0))((a, b) => a.zip(b).map(_ + _))
      sheet.write("전체" :: addPercent(total))

    wb.writeSheet("직군별 통계")(aux("직군", states))
    wb.writeSheet("부서별 통계")(aux("부서", depts))
    wb.writeSheet("잘못된 응답") { sheet =>
      (4 to 10).foreach(sheet.setWidth(_, 10))
      sheet.addHeaderRow(0)
      sheet.write(
        "이름",
        "사번",
        "학번",
        "이메일",
        "1차 접종일",
        "1차 로트번호",
        "2차 예정일",
        "1차 접종일",
        "1차 로트번호",
        "2차 접종일",
        "2차 로트번호",
        "상세"
      )
      errors.foreach(sheet.write)
    }
  }

  val (large, small) =
    depts
      .filter(_.notRespondedActive != 0)
      .partition(_.notRespondedActive >= 30)
  for d <- large do
    writeWorkbook(s"${d.name}.xlsx") { wb =>
      wb.writeSheet("미응답자") { sheet =>
        sheet.addHeaderRow(0)
        sheet.write("이름", "사번", "학번", "이메일")
        d.nraList.map(_.getData).foreach(sheet.write)
      }
    }
  writeWorkbook("기타 부서.xlsx") { wb =>
    wb.writeSheet("미응답자") { sheet =>
      sheet.addHeaderRow(0)
      sheet.write("이름", "사번", "학번", "이메일")
      var r = 1
      for d <- small do
        sheet.addHeaderRow(r)
        sheet.write(d.name, "", "", "")
        d.nraList.map(_.getData).foreach(sheet.write)
        r += d.nraList.length + 1
    }
  }

def resolveHome(fn: String): String =
  if (fn.startsWith(s"~$SEP"))
    fn.replaceFirst("~", System.getProperty("user.home"))
  else
    fn

def rows(fn: String, pw: String = null) =
  getRows(resolveHome(fn), pw).tail.map(getStrings)

def addPercent(l: List[Double]): List[String | Double] =
  val List(_, _, _, _, r, u, o, c) = l
  def div(a: Double, b: Double): String | Double =
    if (b == 0) "-" else a / b
  l ++ List(div(u, r), div(o, r), div(c, r))
