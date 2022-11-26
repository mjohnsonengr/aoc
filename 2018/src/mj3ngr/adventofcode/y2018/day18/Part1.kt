package mj3ngr.adventofcode.y2018.day18

import java.io.File
import java.io.PrintStream

fun <T> Collection<Entity>.numEntity(klass: Class<T>) = filterIsInstance(klass).count()
fun Collection<Entity>.numLumberYards() = numEntity(LumberYard::class.java)
fun Collection<Entity>.numTrees() = numEntity(Trees::class.java)

data class Point(val x: Int, val y: Int) {
  fun adjacent() = listOf(
      Point(x - 1, y - 1),
      Point(x - 1, y),
      Point(x - 1, y + 1),
      Point(x, y - 1),
      Point(x, y + 1),
      Point(x + 1, y - 1),
      Point(x + 1, y),
      Point(x + 1, y + 1))
}

interface Entity {
  val point: Point
  val char: Char

  fun minute(adjacent: List<Entity>): Entity?
}

data class Trees(override val point: Point) : Entity {
  override val char = '|'

  override fun minute(adjacent: List<Entity>) =
      if (adjacent.numLumberYards() >= 3) LumberYard(point) else this
}

data class LumberYard(override val point: Point) : Entity {
  override val char = '#'

  override fun minute(adjacent: List<Entity>) =
      if (adjacent.numLumberYards() >= 1 && adjacent.numTrees() >= 1) this
      else Open(point)
}

data class Open(override val point: Point) : Entity {
  override val char = '.'

  override fun minute(adjacent: List<Entity>) =
      if (adjacent.numTrees() >= 3) Trees(point) else Open(point)
}

typealias LumberArea = Map<Point, Entity>

operator fun LumberArea.get(x: Int, y: Int) = this[Point(x, y)]
operator fun Map<Point, Entity>.plus(e: Entity) = plus(Pair(e.point, e))
fun LumberArea.maxX() = keys.map { it.x }.max()!!
fun LumberArea.maxY() = keys.map { it.y }.max()!!

fun LumberArea.print(out: PrintStream) {
  out.println("    ${(0..maxX()).map { it / 10 }.joinToString("")}")
  out.println("    ${(0..maxX()).map { it % 10 }.joinToString("")}")
  for (y in 0..maxY()) {
    out.print("${y.toString().padStart(2)}: ")
    for (x in 0..maxX()) {
      out.print(this[x, y]?.char ?: '.')
    }
    out.println()
  }
}

fun LumberArea.value() = values.numTrees() * values.numLumberYards()

fun LumberArea.minute(n: Int): LumberArea = when(n) {
  0 -> this
  else -> minute(n-1).minute()
}

fun LumberArea.minute() =
  (0..maxX()).map { x -> (0..maxY()).map { y -> Point(x, y) } }.flatten()
      .associateBy { it }
      .mapValues { (_, pt) -> pt.adjacent().mapNotNull { this[it] } }
      .mapNotNull { (pt, adjacent) -> this[pt]!!.minute(adjacent) }
      .associateBy { it.point }

fun parseInput(lines: List<String>): LumberArea =
    lines.withIndex()
        .map { parseLine(it) }.flatten().associateBy { it.point }

fun parseLine(l: IndexedValue<String>): List<Entity> =
    l.value.withIndex()
        .map { parseAcre(it.value, Point(it.index, l.index)) }

fun parseAcre(a: Char, point: Point) = when(a) {
  '.' -> Open(point)
  '#' -> LumberYard(point)
  '|' -> Trees(point)
  else -> throw AssertionError("invalid input")
}


fun main(args: Array<String>) {
  val testInput = """
    .#.#...|#.
    .....#|##|
    .|..|...#.
    ..|#.....#
    #.#|||#|#|
    ...#.||...
    .|....|...
    ||...#|.#|
    |.||||..|.
    ...#.|..|.
  """.trimIndent()

//  var map = parseInput(testInput.lines())
//  map.print(System.out)
//  for (i in 1..10000000) {
//    val newMap = map.minute()
//    if (newMap == map) break
//    map = newMap
//    map.print(System.out)
//  }
////  map = map.minute(10)
//  println(map.value())

  var map2 = parseInput(File("data/2018/18.txt").readLines())
  val maps = mutableSetOf<Int>()
//  map2.print(System.out)
  val THOU = 1000
  val MIL = THOU * 1000
  val BIL = MIL * 1000

  for (i in 1..THOU) {
    val newMap = map2.minute()
    if (newMap == map2) break
    val hc = newMap.values.hashCode()
    if (maps.contains(hc)) break
    maps.add(hc)
    map2 = newMap
//    map2.print(System.out)
    println(map2.value())
    // Getting the answer from here:
    // I plotted the values above and found that a wave occurs with an offset
    // of 20 and a period of 28.  I found that 1B - 20 was divisible by 28 as
    // is 1000, so the answer after 1,000 is the same.  The earliest this answer
    // occurred was at iteration 440.
  }
//  map2 = map2.minute(1000000000)
  map2.print(System.out)
  println()
  println(map2.value())
}
