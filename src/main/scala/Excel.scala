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
  XSSFCellStyle,
  DefaultIndexedColorMap
}
import org.apache.poi.ss.usermodel.FillPatternType.SOLID_FOREGROUND

import java.awt.Color
import java.io.{File, FileOutputStream}

import scala.collection.mutable.{Map as MMap, Set as MSet}
import scala.jdk.CollectionConverters._

def getRows(name: String, password: String = null): List[Row] =
  val workbook = WorkbookFactory.create(File(name), password)
  val res = workbook.getSheetAt(0).iterator.asScala.toList
  workbook.close()
  res

def getStrings(row: Row): List[String] =
  row.iterator.asScala.toList.map(getString)

def getStringsWithColors(row: Row): List[(String, String)] =
  row.iterator.asScala.toList.map(getStringWithColor)

def getString(c: Cell): String =
  if (c == null) ""
  else
    val s = c.getCellType match
      case CellType.NUMERIC => c.getNumericCellValue.toLong.toString
      case CellType.FORMULA => c.getCellFormula
      case _                => c.getStringCellValue
    s.trim.filterNot(_.isControl)

def getColor(c: Cell): String =
  c.getCellStyle.getFillForegroundColorColor match
    case color: XSSFColor => color.getARGBHex
    case _                => ""

def getStringWithColor(c: Cell): (String, String) =
  (getString(c), getColor(c))

def writeWorkbook(path: String)(func: XSSFWorkbook => Unit): Unit =
  val wb = XSSFWorkbook()
  func(wb)
  val out = FileOutputStream(File(path))
  wb.write(out)
  wb.close()
  out.close()

extension (wb: XSSFWorkbook)
  def writeSheet(name: String)(func: SheetWrapper => Unit): Unit =
    val wrapper = SheetWrapper(wb, name)
    func(wrapper)
    wrapper.computeWidth()

  def setColor(st: XSSFCellStyle, r: Int, g: Int, b: Int): Unit =
    val color = XSSFColor(Color(r, g, b), DefaultIndexedColorMap())
    st.setFillPattern(SOLID_FOREGROUND);
    st.setFillForegroundColor(color)

class SheetWrapper(wb: XSSFWorkbook, name: String):

  private val sheet = wb.createSheet(name)
  private val percent = wb.createDataFormat.getFormat("0.00%")

  private val widthMap = MMap.empty[Int, Int]
  private val map = MMap.empty[Int, Int]
  private val headerRow = MSet.empty[Int]
  private val percentCol = MSet.empty[Int]
  private var rowNumber = 0

  private val defaultStyle = wb.createCellStyle
  private val headerStyle = wb.createCellStyle
  wb.setColor(headerStyle, 0xc0, 0xc0, 0xc0)
  private val percentStyle = wb.createCellStyle
  percentStyle.setDataFormat(percent)
  private val headerPercentStyle = wb.createCellStyle
  wb.setColor(headerPercentStyle, 0xc0, 0xc0, 0xc0)
  headerPercentStyle.setDataFormat(percent)

  def newRow(): Row =
    val row = sheet.createRow(rowNumber)
    rowNumber += 1
    row

  def write(r: Row, i: Int, v: String | Double): Unit =
    val c = r.createCell(i)
    val (percent, s) = v match
      case s: String =>
        c.setCellValue(s)
        (false, s)
      case d: Double =>
        c.setCellValue(d)
        (percentCol(i), f"$d%.3f")
    val style =
      if (headerRow(r.getRowNum))
        if (percent) headerPercentStyle else headerStyle
      else if (percent) percentStyle
      else defaultStyle
    c.setCellStyle(style)
    map(i) = map.getOrElse(i, 0) max s.map(c => if (c < 128) 1 else 2).sum

  def write(ss: (String | Double)*): Unit =
    val r = newRow()
    ss.zipWithIndex.foreach((s, i) => write(r, i, s))

  def write(ss: List[String | Double]): Unit =
    write(ss: _*)

  def setWidth(i: Int, w: Int): Unit = widthMap(i) = w

  def addPercentCol(i: Int): Unit = percentCol += i

  def addPercentCols(i: Int*): Unit = percentCol ++= i

  def addHeaderRow(i: Int): Unit = headerRow += i

  def addHeaderRows(i: Int*): Unit = headerRow ++= i

  def computeWidth(): Unit =
    for (i, l) <- map do
      sheet.setColumnWidth(i, (widthMap.getOrElse(i, l) * 256 + 256) min 12800)
