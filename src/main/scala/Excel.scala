import org.apache.poi.ss.usermodel.{
  WorkbookFactory,
  Row,
  Sheet,
  Cell,
  CellType,
  CellStyle
}
import org.apache.poi.xssf.usermodel.{
  XSSFWorkbook,
  XSSFColor,
  DefaultIndexedColorMap
}
import org.apache.poi.ss.usermodel.FillPatternType.SOLID_FOREGROUND

import java.awt.Color
import java.io.{File, FileOutputStream}

import scala.collection.mutable.{Map => MMap}
import scala.jdk.CollectionConverters._

def getRows(name: String, password: String = null): List[Row] =
  val workbook = WorkbookFactory.create(File(name), password)
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

def writeWorkbook(path: String)(func: XSSFWorkbook => Unit): Unit =
  val wb = XSSFWorkbook()
  func(wb)
  val out = FileOutputStream(File(path))
  wb.write(out)
  wb.close()
  out.close()

extension (wb: XSSFWorkbook)
  def writeSheet(name: String)(func: SheetWrapper => Unit): Unit =
    val sheet = wb.createSheet(name)
    val wrapper = SheetWrapper(sheet)
    func(wrapper)
    wrapper.computeWidth()

  def createStyle(r: Int, g: Int, b: Int): CellStyle =
    val st = wb.createCellStyle
    val color = XSSFColor(Color(r, g, b), DefaultIndexedColorMap())
    st.setFillPattern(SOLID_FOREGROUND);
    st.setFillForegroundColor(color)
    st

class SheetWrapper(sheet: Sheet):
  private val widthMap = MMap.empty[Int, Int]
  private val map = MMap.empty[Int, Int]
  private var rowNumber = 0
  private var style: Option[CellStyle] = None

  def newRow(): Row =
    val row = sheet.createRow(rowNumber)
    rowNumber += 1
    row

  def write(r: Row, i: Int, v: String | Double): Unit =
    val c = r.createCell(i)
    v match
      case s: String => c.setCellValue(s)
      case d: Double => c.setCellValue(d)
    style.foreach(c.setCellStyle)
    val s = v.toString
    map(i) = map.getOrElse(i, 0) max s.map(c => if (c < 128) 1 else 2).sum

  def write(ss: (String | Double)*): Unit =
    val r = newRow()
    ss.zipWithIndex.foreach((s, i) => write(r, i, s))

  def write(ss: List[String | Double]): Unit =
    write(ss: _*)

  def setWidth(i: Int, w: Int): Unit = widthMap(i) = w

  def setStyle(st: CellStyle): Unit = style = Some(st)

  def clearStyle(): Unit = style = None

  def computeWidth(): Unit =
    for (i, l) <- map do
      sheet.setColumnWidth(i, (widthMap.getOrElse(i, l) * 256 + 256) min 12800)
