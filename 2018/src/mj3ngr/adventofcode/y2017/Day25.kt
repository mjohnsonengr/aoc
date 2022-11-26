package mj3ngr.adventofcode.y2017

import java.io.File
import java.lang.IllegalArgumentException

enum class Direction(val increment: Int) {
  LEFT(-1),
  RIGHT(+1)
}

data class StateKey(val key: String)

data class Directive(val newValue: Int, val direction: Direction, val nextState: StateKey) {
  override fun toString() = "write $newValue, move $direction, then state $nextState"
}

data class State(val directives: Map<Int, Directive>)

data class StateMachine(val states: Map<StateKey, State>)

class StateMachineRunner(private val numSteps: Int, val startingState: StateKey, val stateMachine: StateMachine) {
  /** Returns number of 1s in the machine after run is complete. */
  fun run(): Int {
    var currentState = startingState
    var pointer = 0
    val registers: MutableMap<Int, Int> = HashMap()
    for (i in 0 until numSteps) {
      val state = stateMachine.states[currentState]!!
      val d = state.directives[registers[pointer] ?: 0]!!
      registers[pointer] = d.newValue
      currentState = d.nextState
      pointer += d.direction.increment
    }
    return registers.values.count { it == 1 }
  }
}

/** Returns last word in a punctuated sentence. e.g. "Hello World." -> "World" */
fun String.lastWord(): String {
  return this.trim().split(' ').last().dropLastWhile { it in listOf('.', ':') }
}

fun String.toDirection(): Direction = when (toLowerCase()) {
  "right" -> Direction.RIGHT
  "left" -> Direction.LEFT
  else -> throw IllegalArgumentException("invalid direction string")
}

fun String.toStateKey(): StateKey = StateKey(this)

fun parse(lines: List<String>): StateMachineRunner = StateMachineRunner(
  startingState = lines[0].lastWord().toStateKey(),
  numSteps = lines[1].dropLastWhile { it != ' ' }.lastWord().toInt(),
  stateMachine = parseStateMachine(lines)
)

private fun parseStateMachine(lines: List<String>) = StateMachine(
  lines.asSequence().drop(2).chunked(10).map {
    assert(it[0].trim().isEmpty())
    Pair(it[1].lastWord().toStateKey(), parseState(it))
  }.toMap())

private fun parseState(stateLines: List<String>) = State(
  stateLines.asSequence().drop(2).chunked(4).map {
    Pair(
      it[0].lastWord().toInt(),
      Directive(
        newValue = it[1].lastWord().toInt(),
        direction = it[2].lastWord().toDirection(),
        nextState = it[3].lastWord().toStateKey()
      )
    )
  }.toMap())

fun main(args: Array<String>) {
  println(parse(File("data/2017/day25.txt").readLines()).run())
}
