import java.io.File.separator as SEP

lazy val unreachable = sys.error("unreachable")

@main def main(
    fn1: String,
    fn2: String,
    fn3: String,
    fnSt: String,
    stPw: String,
    fnEm: String,
    emPw: String
): Unit =
  val students = rows(fnSt, stPw).map(Student.parse)
  val employees = rows(fnEm, emPw).map(Employee.parse)
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
      "완료자"
    )

  val errors = pes.map { case (p, (es, is)) =>
    val i2 = is.find(_.length == 3).getOrElse(List("", "", ""))
    val i3 = is.find(_.length == 4).getOrElse(List("", "", "", ""))
    val e = es.collect { case Left(e) => e.toString }.mkString(", ")
    List(p.name, p.en, p.sn, p.email) ++ i2 ++ i3 :+ e
  }

  writeWorkbook("설문조사 응답 현황.xlsx") { wb =>
    val st = wb.createStyle(0xc0, 0xc0, 0xc0)
    def aux(s: String, l: List[Group])(sheet: SheetWrapper) =
      sheet.setStyle(st)
      sheet.write(s :: header)
      sheet.clearStyle()
      l.map(_.getData).foreach(sheet.write)
      sheet.setStyle(st)
      val a: List[String | Double] = "전체" :: l
        .map(_.getStat)
        .foldLeft(List.fill(8)(0.0))((a, b) => a.zip(b).map(_ + _))
      sheet.write(a)

    wb.writeSheet("직군별 통계")(aux("직군", states))
    wb.writeSheet("부서별 통계")(aux("부서", depts))
    wb.writeSheet("잘못된 응답") { sheet =>
      (4 to 10).foreach(sheet.setWidth(_, 10))
      sheet.setStyle(st)
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
      sheet.clearStyle()
      errors.foreach(sheet.write)
    }
  }

  val (large, small) =
    depts
      .filter(_.notRespondedActive != 0)
      .partition(_.notRespondedActive >= 30)
  for d <- large do
    writeWorkbook(s"${d.name}.xlsx") { wb =>
      val st = wb.createStyle(0xc0, 0xc0, 0xc0)
      wb.writeSheet("미응답자") { sheet =>
        sheet.setStyle(st)
        sheet.write("이름", "사번", "학번", "이메일")
        sheet.clearStyle()
        d.nraList.map(_.getData).foreach(sheet.write)
      }
    }
  writeWorkbook("기타 부서.xlsx") { wb =>
    val st = wb.createStyle(0xc0, 0xc0, 0xc0)
    wb.writeSheet("미응답자") { sheet =>
      sheet.setStyle(st)
      sheet.write("이름", "사번", "학번", "이메일")
      for d <- small do
        sheet.setStyle(st)
        sheet.write(d.name, "", "", "")
        sheet.clearStyle()
        d.nraList.map(_.getData).foreach(sheet.write)
    }
  }

def resolveHome(fn: String): String =
  if (fn.startsWith(s"~$SEP"))
    fn.replaceFirst("~", System.getProperty("user.home"))
  else
    fn

def rows(fn: String, pw: String = null) =
  getRows(resolveHome(fn), pw).tail.map(getStrings)
