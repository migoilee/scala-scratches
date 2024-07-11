case class StudentId(id: Int)
case class StaffId(id: Int)
case class Score(s: Double)

trait Printer[A] {
  def getString(a: A): String
}

def show[A](a: A)(implicit printer: Printer[A]): String = printer.getString(a)

object Printer {
  implicit val studentPrinter: Printer[StudentId] = new Printer[StudentId] {
    def getString(a: StudentId): String = s"StudentId: ${a.id}"
  }

  implicit val staffPrinter: Printer[StaffId] = new   Printer[StaffId] {
    def getString(a: StaffId): String = s"StaffId: ${a.id}"
  }

  implicit val scorePrinter: Printer[Score] = new Printer[Score] {
    def getString(a: Score): String = s"Score: ${a.s}%"
}
}

import Printer.studentPrinter
val studentId = new StudentId(1)
val x = show(studentId)