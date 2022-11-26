package mj3ngr.adventofcode.y2018

import java.io.File
import java.time.LocalDate

data class Guard(val id: Int)

/**
 * Normalized minute value centered around midnight.
 *
 * <p>Normalized is so minute values can be easily compared around midnight, e.g. 23:58 -> -2; 1:02 -> 62
 */
data class NormalizedMinute(private val hour: Int, private val minute: Int) {
  val value: Int = when(hour) {
    in 0..12 -> hour * 60 + minute
    else -> (hour - 24) * 60 + minute
  }
}

/**
 * Normalized date value centered around midnight.  13:00-23:59 appear as the next day.
 *
 * <p>Note: not data class because equals/hashCode will include hour and shouldn't.
 */
data class NormalizedDate(private val year: Int, private val month: Int, private val day: Int, private val hour: Int) {
  val value: LocalDate = if (hour < 12) LocalDate.of(year, month, day) else LocalDate.of(year, month, day).plusDays(1)

  override fun toString() = value.toString()
  override fun equals(other: Any?): Boolean {
    if (other !is NormalizedDate) {
      return false
    }
    return value == other.value
  }

  override fun hashCode(): Int {
    return value.hashCode()
  }
}

enum class EventType { BEGIN, FALL_ASLEEP, WAKE_UP }

data class Event(val guard: Guard, val minute: NormalizedMinute, val date: NormalizedDate, val type: EventType)

fun main(args: Array<String>) {
  val lines = File("data/2018/4.txt").readLines()

  val dateRegex = """\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)]""".toRegex()
  val beginRegex = """Guard #(\d+) begins shift""".toRegex()
  val fallsAsleepRegex = """falls asleep""".toRegex()
  val wakesUpRegex = """wakes up""".toRegex()

  val events: ArrayList<Event> = ArrayList()

  var currentGuard: Guard? = null
  for (line in lines.sorted()) {
    val (year, month, day, hour, rawMinute) = dateRegex.find(line)!!.destructured.toList().map { it.toInt() }
    val date = NormalizedDate(year, month, day, hour)
    val minute = NormalizedMinute(hour, rawMinute)
    val eventType = when {
      beginRegex.containsMatchIn(line) -> {
        currentGuard = Guard(beginRegex.find(line)!!.destructured.toList()[0].toInt())
        EventType.BEGIN
      }
      fallsAsleepRegex.containsMatchIn(line) -> EventType.FALL_ASLEEP
      wakesUpRegex.containsMatchIn(line) -> EventType.WAKE_UP
      else -> throw IllegalStateException("input didn't match any of the regexes")
    }
    events.add(Event(currentGuard!!, minute, date, eventType))
  }

  println("Part 1: ${findSleepiestGuardsSleepiestMinute(events)}")
  val sleepiestGuardMinute = findSleepiestGuardMinute(events)
  println(sleepiestGuardMinute)
  println("Part 2: ${sleepiestGuardMinute.answer}")
}

data class GuardMinute(val guard: Guard, val minute: Int) {
  val answer = guard.id * minute
}

private fun findSleepiestGuardMinute(events: List<Event>): GuardMinute {
  val sleepCounter: HashMap<GuardMinute, Int> = HashMap()
  var currentGuard: Guard? = null
  var fellAsleepMinute: NormalizedMinute? = null
  for (event in events) {
    when(event.type) {
      EventType.BEGIN -> currentGuard = event.guard
      EventType.FALL_ASLEEP -> {
        assert(fellAsleepMinute == null)
        fellAsleepMinute = event.minute
      }
      EventType.WAKE_UP -> {
        for (i in fellAsleepMinute!!.value until event.minute.value) {
          val guardMinute = GuardMinute(currentGuard!!, i)
          sleepCounter[guardMinute] = (sleepCounter[guardMinute] ?: 0) + 1
        }
        fellAsleepMinute = null
      }
    }
  }

  return sleepCounter.entries.sortedByDescending { it.value }.first().key
}

private fun findSleepiestGuardsSleepiestMinute(events: List<Event>): Int {
  val guardSleepTotals: HashMap<Guard, Int> = HashMap()
  val eventsPerDay = events.groupBy { it.date }
  for (eventGroup in eventsPerDay) {
    val eventList = eventGroup.value
    val guard = eventList.first().guard
    if (guardSleepTotals[guard] == null) {
      guardSleepTotals[guard] = 0
    }
    var time = 0
    for (i in 1 until eventList.size step 2) {
      assert(eventList[i].type == EventType.FALL_ASLEEP)
      assert(eventList[i + 1].type == EventType.WAKE_UP)
      time += eventList[i + 1].minute.value - eventList[i].minute.value
    }
    guardSleepTotals[guard] = guardSleepTotals[guard]!! + time
  }

  val sleepiestGuard = guardSleepTotals.toList().sortedByDescending { it.second }.first().first

  val eventsForSleepiestGuard =
    eventsPerDay
      .filter { (it.value.first()).guard == sleepiestGuard }
      .values
      .flatten()
      .filter { it.type == EventType.FALL_ASLEEP || it.type == EventType.WAKE_UP}
  val sleepPerMinute: HashMap<Int, Int> = HashMap()
  for (i in 0 until eventsForSleepiestGuard.size step 2) {
    assert(eventsForSleepiestGuard[i].type == EventType.FALL_ASLEEP)
    assert(eventsForSleepiestGuard[i + 1].type == EventType.WAKE_UP)
    for (j in eventsForSleepiestGuard[i].minute.value until eventsForSleepiestGuard[i + 1].minute.value) {
      if (sleepPerMinute[j] == null) {
        sleepPerMinute[j] = 0
      }
      sleepPerMinute[j] = sleepPerMinute[j]!! + 1
    }
  }
  val sleepiestMinute = sleepPerMinute.toList().sortedByDescending { it.second }.first().first

  println(sleepiestGuard)
  println(sleepiestMinute)
  return sleepiestGuard.id * sleepiestMinute
}

