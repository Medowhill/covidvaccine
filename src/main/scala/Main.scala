import org.apache.commons.io.FileUtils

import java.io.File
import File.separator as SEP

import scala.jdk.CollectionConverters._
import scala.util.chaining._

import Vaccine.Ongoing

lazy val unreachable = sys.error("unreachable")

@main def main(
    fn1: String,
    fn2: String,
    fn3: String,
    fnSt: String,
    stPw: String,
    fnEm: String,
    emPw: String,
    dnEx: String,
    dnPr: String
): Unit =
  val exs = getXlsxs(dnEx)
    .map(getStringsWithColors(_)(1))
    .collect { case (n, c) if c == "FFFFFF00" => n }
    .toSet

  val _prs = getXlsxs(dnPr)
    .map(getStrings)
    .filter(_.length > 4)
    .filter(_.head != "이름")
    .map(Respondant.parse2)
    .groupBy(_._1)
    .map { case (r, l) =>
      r -> l
        .map { case (_, a, b) => (a, b) }
        .toList
        .sortBy { case (e, _) => if (e.isLeft) 1 else 0 }
        .unzip
    }
  val prs = _prs.collect { case (p, (Right(v) :: _, _)) => p -> v }.toList
  val pres = _prs.filter(_._2._1.head.isLeft)
  val prIds = prs.flatMap((p, v) => List(p.en, p.sn)).toSet.filter(_.nonEmpty)

  val students = rows(fnSt, stPw).map(Student.parse)
  val employees = rows(fnEm, emPw).map(Employee.parse).filterNot(_.en.pipe(exs))
  val people = (students ++ employees).sorted
  val deptMap = people.map(p => p.number -> p.dept).toMap

  val pevs = (rows(fn3) ++ rows(fn2) ++ rows(fn1))
    .map(Respondant.parse)
    .groupBy(_._1)
    .map((p, l) => p -> (l.map(_._2), l.map(_._3)))
    .toList
    .sortBy(_._1)

  val _pvs = pevs.collect { case (p, (Right(v) :: _, _)) => p -> v }
  val _pes = pevs.filter(_._2._1.head.isLeft)

  val pvIds = _pvs.flatMap((p, v) => List(p.en, p.sn)).toSet.filter(_.nonEmpty)

  val pvs = _pvs ++ prs.filter { case (r, _) => !pvIds(r.en) && !pvIds(r.sn) }
  val pes = _pes.filter { case (r, _) => !prIds(r.en) && !prIds(r.sn) } ++ pres

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
    val i3 =
      is.find(_.length >= 4).map(_.take(4)).getOrElse(List("", "", "", ""))
    val e = es.collect { case Left(e) => e.toString }.mkString(", ")
    List(p.name, p.en, p.sn, p.email) ++ i2 ++ i3 :+ e
  }

  writeWorkbook("out/설문조사 응답 현황.xlsx") { wb =>
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
    wb.writeSheet("부분 완료자") { sheet =>
      sheet.addHeaderRow(0)
      sheet.write(
        "이름",
        "사번",
        "학번",
        "이메일"
      )
      pvs
        .sortBy(_._1)
        .collect { case (r, o: Ongoing) => List(r.name, r.en, r.sn, r.email) }
        .foreach(sheet.write)
    }
  }

  val header1 = List("이름", "사번", "학번", "이메일")
  val (large, small) =
    depts
      .filter(d => d.notRespondedActive != 0 || d.ongoing != 0)
      .partition(d => d.notRespondedActive >= 30 || d.ongoing >= 20)
  for d <- large do
    writeWorkbook(s"out/${d.name}.xlsx") { wb =>
      if (d.notRespondedActive != 0)
        wb.writeSheet("미응답자") { sheet =>
          sheet.addHeaderRow(0)
          sheet.write(header1)
          d.nraList.map(_.getData).foreach(sheet.write)
        }
      if (d.ongoing != 0)
        wb.writeSheet("부분 완료자") { sheet =>
          sheet.addHeaderRow(0)
          sheet.write(header1)
          d.ongoingList.map(_.getData).foreach(sheet.write)
        }
    }
  writeWorkbook("out/기타 부서.xlsx") { wb =>
    wb.writeSheet("미응답자") { sheet =>
      sheet.addHeaderRow(0)
      sheet.write(header1)
      var r = 1
      for d <- small do
        if (d.notRespondedActive != 0)
          sheet.addHeaderRow(r)
          sheet.write(d.name, "", "", "")
          d.nraList.map(_.getData).foreach(sheet.write)
          r += d.notRespondedActive.toInt + 1
    }
    wb.writeSheet("부분 완료자") { sheet =>
      sheet.addHeaderRow(0)
      sheet.write(header1)
      var r = 1
      for d <- small do
        if (d.ongoing != 0)
          sheet.addHeaderRow(r)
          sheet.write(d.name, "", "", "")
          d.ongoingList.map(_.getData).foreach(sheet.write)
          r += d.ongoing.toInt + 1
    }
  }

def getXlsxs(dn: String) =
  val dir = File(dn)
  require(dir.isDirectory)
  FileUtils
    .listFiles(dir, Array("xlsx"), true)
    .asScala
    .filter(!_.getName.startsWith("~"))
    .flatMap(_.getAbsolutePath.pipe(getRows(_, null)))

def resolveHome(fn: String): String =
  if (fn.startsWith(s"~$SEP"))
    fn.replaceFirst("~", System.getProperty("user.home"))
  else
    fn

def rows(fn: String, pw: String = null) =
  getRows(resolveHome(fn), pw).tail.map(getStrings).filter(_.nonEmpty)

def addPercent(l: List[Double]): List[String | Double] =
  val List(_, _, _, _, r, u, o, c) = l
  def div(a: Double, b: Double): String | Double =
    if (b == 0) "-" else a / b
  l ++ List(div(u, r), div(o, r), div(c, r))
