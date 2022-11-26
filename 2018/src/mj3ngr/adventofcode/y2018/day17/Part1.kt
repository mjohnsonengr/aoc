package mj3ngr.adventofcode.y2018.day17

import java.io.File
import kotlin.system.measureTimeMillis

typealias State = Map<Point, Entity>
fun MatchResult?.getInts() = this!!.destructured.toList().map { it.toInt() }
operator fun State.get(x: Int, y: Int) = this[Point(x, y)]
operator fun State.plus(e: Entity) = this + e.toPair()
fun State.minY() = this.keys.map { it.y }.min()!!
fun State.maxY() = this.keys.map { it.y }.max()!!

data class Point(val x: Int, val y: Int) {
  fun transpose() = Point(y, x)

  fun left() = Point(x-1, y)
  fun right() = Point(x+1, y)
  fun down() = Point(x, y+1)
}

interface MaybeEntity

interface Entity : MaybeEntity {
  val point: Point
  val char: Char

  fun toPair() = Pair(point, this)
  fun left() = point.left()
  fun right() = point.right()
  fun down() = point.down()
}

interface Blocking
interface Flow : Entity

data class Wall(override val point: Point) : Entity, Blocking {
  override val char = '#'
}

data class Source(override val point: Point) : Flow, Entity {
  override val char = '+'
}

data class OpenFlow(override val point: Point) : Flow, Entity {
  override val char = '|'
}

data class Water(override val point: Point) : Entity, Blocking {
  override val char = '~'
}

fun parseInput(lines: List<String>): State =
    lines.map { parseLine(it) }.flatten().associateBy { it.point }

fun parseLine(line: String) = when(line[0]) {
  'x' -> parseWalls(line)
  'y' -> parseWalls(line).transpose()
  else -> throw AssertionError("invalid input")
}

fun parseWalls(line: String): List<Wall> {
  val (x, y1, y2) = """.=(\d+), .=(\d+)..(\d+)""".toRegex().find(line).getInts()
  return (y1..y2).map { Wall(Point(x, it)) }
}

fun List<Wall>.transpose() = this.map { Wall(it.point.transpose()) }

fun printState(state: State) {
  val minX = state.keys.map { it.x }.min()!! - 1
  val maxX = state.keys.map { it.x }.max()!! + 1
  val minY = 0
  val maxY = state.maxY() + 1

  val hundreds = StringBuilder(" ".repeat(5))
  val tens = StringBuilder(" ".repeat(5))
  val ones = StringBuilder(" ".repeat(5))
  for (x in minX..maxX) {
    hundreds.append(x / 100)
    tens.append(x % 100 / 10)
    ones.append(x % 10)
  }
  println(hundreds.toString())
  println(tens.toString())
  println(ones.toString())
  for (y in minY..maxY) {
    print("${y.toString().padStart(3)} ")
    for (x in minX..maxX) {
      print(state[x, y]?.char ?: '.')
    }
    println()
  }
}

fun expandWater(state: State) = expandWater(Source(Point(500, 0)), state)

fun expandWater(point: Point, state: State) =
    expandWater(OpenFlow(point), state)

fun expandWater(flow: Flow, inState: State): State {
  var state = inState + flow
  if (flow.point.y >= state.maxY()) {
    return state
  }

  if (state[flow.down()] !is Entity) {
    state = expandWater(flow.down(), state)
  }

  // Only expand left/right on top of standing water or walls.
  if (state[flow.down()] is Blocking) {
    if (state[flow.right()] !is Entity) {
      state =
          expandWater(flow.right(), state)
    }
    if (state[flow.left()] !is Entity) {
      state = expandWater(flow.left(), state)
    }
  }

  if (isContainedWater(flow, state)) {
    // recursively ensure neighbor flows are also water.
    state = ensureIsWater(flow.right(), state)
    state = ensureIsWater(flow.left(), state)
    return state + Water(flow.point)
  }

  return state
}

fun ensureIsWater(point: Point, state: State): State =
    if (state[point] is OpenFlow) {
      ensureIsWater(
          point.left(), ensureIsWater(point.right(), state + Water(point))
      )
    } else state

fun isContainedWater(flow: Flow, state: State, marked: List<Flow> = listOf()) =
    when {
      state[flow.down()] !is Blocking -> false
      marked.contains(flow) -> true
      else -> isBlocking(state[flow.right()], state, marked + flow) &&
          isBlocking(state[flow.left()], state, marked + flow)
    }

fun isBlocking(entity: Entity?, state: State, marked: List<Flow>): Boolean =
    when (entity) {
      null -> false
      is Blocking -> true
      is Flow -> isContainedWater(entity, state, marked)
      else -> throw AssertionError("invalid state")
    }

fun main(args: Array<String>) {
  val testInput = """
    x=495, y=2..7
    y=7, x=495..501
    x=501, y=3..7
    x=498, y=2..4
    x=506, y=1..2
    x=498, y=10..13
    x=504, y=10..13
    y=13, x=498..504
  """.trimIndent()
  val testState = expandWater(parseInput(testInput.lines()))
  printState(testState)
  print("Part 1: ")
  print(countWaterAndFlowing(testState))
  println(" total water tiles including flow")
  print("Part 2: ")
  print(countWater(testState))
  println(" total standing water")

  val time = measureTimeMillis {
    val state = expandWater(parseInput(File("data/2018/17.txt").readLines()))
    printState(state)
    print("Part 1: ")
    print(countWaterAndFlowing(state))
    println(" total water tiles including flow")
    print("Part 2: ")
    print(countWater(state))
    println(" total standing water")
  }
  // TODO: This took 564 seconds.
  println(time)
}

fun countWaterAndFlowing(state: State) =
  state.values
      .filter { it is Water || it is OpenFlow }
      .filter { it.point.y >= state.minY() }.count()

fun countWater(state: State) =
    state.values
        .filter { it is Water && it.point.y >= state.minY() }.count()
