package org.scalajs.testsuite.javalib.time.format

import java.time.{Instant, LocalDate, LocalTime}

import org.junit.{Assert, Test}

class DateTimeFormatterTest {
  val dateExamples = Seq(LocalDate.of(2017, 12, 23),
                         LocalDate.of(-10, 12, 23),
                         LocalDate.of(10000, 12, 23),
                         LocalDate.of(200, 1, 1))

  @Test def test_localdateparse(): Unit = {
    for (example <- dateExamples) {
      val date = LocalDate.parse(example.toString)
      Assert.assertEquals(example, date)
    }
  }

  val timeExamples = Seq(LocalTime.of(13, 42),
                         LocalTime.of(13, 42, 23),
                         LocalTime.of(13, 42, 23, 123456789),
                         LocalTime.of(1, 4, 2, 12345))

  @Test def test_localtimeparse(): Unit = {
    for (example <- timeExamples) {
      val time = LocalTime.parse(example.toString)
      Assert.assertEquals(example, time)
    }
  }

  @Test def test_instantparse(): Unit = {
    for {
      de <- dateExamples
      te <- timeExamples
    } {
      val example =
        Instant.ofEpochSecond(
          de.toEpochDay * 24 * 3600,
          te.toNanoOfDay
        )

      val time = Instant.parse(example.toString)
      Assert.assertEquals(example, time)
    }
  }
}
