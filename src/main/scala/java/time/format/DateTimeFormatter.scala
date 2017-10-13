package java.time.format

import java.time.temporal.TemporalAccessor
import java.time.{Instant, LocalDate, LocalTime}

import scala.util.control.NonFatal

trait DateTimeFormatter {
  type tempAcc = TemporalAccessor

  def parse(text: CharSequence): tempAcc = {
    try {
      parseImpl(text);
    } catch {
      case NonFatal(e) =>
        throw new DateTimeParseException(s"Could not parse '$text'", text, 0, e)
    }
  }

  protected def parseImpl(text: CharSequence): tempAcc
}

object DateTimeFormatter {

  val ISO_INSTANT = new DateTimeFormatter {
    override protected def parseImpl(text: CharSequence): Instant = {
      val pattern = """^(.+)T(.+)Z$""".r
      val pattern(date, time) = text
      val d = ISO_LOCAL_DATE.parse(date).asInstanceOf[LocalDate]
      val t = ISO_LOCAL_TIME.parse(time).asInstanceOf[LocalTime]

      Instant.ofEpochSecond(
        d.toEpochDay * 24 * 3600,
        t.toNanoOfDay
      )
    }
  }

  val ISO_LOCAL_DATE = new DateTimeFormatter {
    override protected def parseImpl(text: CharSequence): LocalDate = {
      val pattern = """^([-+]?\d{4,})-(\d{2})-(\d{2})$""".r
      val pattern(year, month, day) = text
      LocalDate.of(Integer.parseInt(year),
                   Integer.parseInt(month),
                   Integer.parseInt(day))
    }
  }

  val ISO_LOCAL_TIME = new DateTimeFormatter {
    override protected def parseImpl(text: CharSequence): LocalTime = {
      val pattern = """^(\d{2}):(\d{2})(?::(\d{2})(?:\.(\d{1,9}))?)?$""".r
      val pattern(hour, minute, second, nano) = text

      val seconds = Option(second).map(Integer.parseInt).getOrElse(0)
      val nanos = Option(nano)
        .map(_.padTo(9, "0").mkString)
        .map(Integer.parseInt)
        .getOrElse(0)

      LocalTime.of(Integer.parseInt(hour),
                   Integer.parseInt(minute),
                   seconds,
                   nanos)
    }
  }
}
