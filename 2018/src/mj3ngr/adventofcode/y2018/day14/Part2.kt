package mj3ngr.adventofcode.y2018.day14

import java.util.*

fun main(args: Array<String>) {
  testPart2("51589", 9)
  testPart2("01245", 5)
  testPart2("92510", 18)
  testPart2("59414", 2018)
  println("Prod")
  println(findSequence("919901"))
}

fun testPart2(sequence: String, expected: Int) {
  println("Test $sequence. Expected $expected")
  println(findSequence(sequence))
}

fun findSequence(sequence: String): Int {
  val debug = false

  val seqSize = sequence.length
  val last = ArrayDeque<Int>()

  val scores = ArrayList<Int>()
  scores.add(3)
  scores.add(7)

  var elf1 = 0
  var elf2 = 1

  printState(scores, elf1, elf2, debug)

  while (true) {
    val recipe1 = scores[elf1]
    val recipe2 = scores[elf2]
    val combined = recipe1 + recipe2
    val newRecipe1 = combined/10
    val newRecipe2 = combined%10
    if (newRecipe1 != 0) {
      scores.add(newRecipe1)
      last.addLast(newRecipe1)
    }

    // Check between recipes added.
    while (last.size > seqSize) {
      last.removeFirst()
    }
    if (last.joinToString("") == sequence) {
      return scores.size - seqSize
    }

    scores.add(newRecipe2)
    last.addLast(newRecipe2)

    while (last.size > seqSize) {
      last.removeFirst()
    }
    if (last.joinToString("") == sequence) {
      return scores.size - seqSize
    }

    elf1 = (elf1 + 1 + recipe1)%scores.size
    elf2 = (elf2 + 1 + recipe2)%scores.size
    printState(scores, elf1, elf2, debug)
  }
}

