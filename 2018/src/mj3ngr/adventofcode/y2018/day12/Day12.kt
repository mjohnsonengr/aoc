package mj3ngr.adventofcode.y2018.day12

import java.io.File

typealias Notes = HashMap<String, Char>

fun run(filename: String, generations: Long) {
  val lines = File(filename).readLines()
  val initialState = """initial state: ([#.]+)""".toRegex().find(lines[0])!!.destructured.component1()
  val noteRegex = """([#.]{5}) => ([#.])""".toRegex()
  val notes = Notes()
  for (note in lines.drop(2)) {
    val (pattern, nextGen) = noteRegex.find(note)!!.destructured
    notes[pattern] = nextGen[0]
  }

  // A pattern of "....# => #" is the worst case scenario for left growth.
  // This results in growing to the left by two pots per generation.
  // ....#
  // ..#..
  // #....
  var currentState = initialState
  var offset = 0L
//  println(" 0: $currentState")
  for (i in 1..generations) {
    val (newOffset, newState) = nextGeneration(currentState, notes, offset)
    if (currentState == newState) {
      // At this point, just the offset has to change
      val change = newOffset - offset
      offset += (generations - i + 1) * change
      break
    }
    currentState = newState
    offset = newOffset
//    println("${i.toString().padStart(12)}: $currentState, $offset")
  }

  println(calcState(currentState, offset))
}

fun nextGeneration(state: String, notes: Notes, offset: Long): Pair<Long, String> {
  val sb = StringBuilder(state.length)
  for (i in 1..4) {
    sb.append(notes[".".repeat(5-i) + state.take(i)] ?: '.')
  }
  for (i in 2 until state.length-2) {
    sb.append(notes[state.substring(i-2..i+2)] ?: '.')
  }
  for (i in 4 downTo 1) {
    sb.append(notes[state.takeLast(i) + ".".repeat(5-i)] ?: '.')
  }
  val newState = sb.toString()
  val newOffset = offset - 2 + newState.takeWhile { it == '.' }.length
  return Pair(newOffset, newState.trim('.'))
}

fun calcState(state: String, offset: Long): Long {
  var result = 0L
  for ((i, pot) in state.withIndex()) {
    if (pot == '#') {
      result += i + offset
    }
  }
  return result
}

fun main(args: Array<String>) {
  println("Test. Expected 325")
  run("data/2018/12test.txt", 20)
  println("Prod")
  run("data/2018/12.txt", 20)
  println("Test 100. Expected 1374")
  run("data/2018/12test.txt", 100)
  println("Test 1000. Expected 19374")
  run("data/2018/12test.txt", 1000)
  println("Prod 50 billion.")
  run("data/2018/12.txt", 50000000000L)
}
