package mj3ngr.adventofcode.y2017.day1

import java.io.File

fun main(args: Array<String>) {
  println("Part 1")
  val strings = arrayOf("1122", "1111", "1234", "91212129", File("data/2017/1.txt").readLines()[0])
  for (string in strings) {
    println(sumSequence(string))
  }

  println("Part 2")
  val strings2 = arrayOf("1212", "1221", "123425", "123123", "12131415", File("data/2017/1.txt").readLines()[0])
  for (string in strings2) {
    println(sumAcross(string))
  }
}

fun sumSequence(sequence: String): Int {
  var prev = sequence.last().toString().toInt()
  var sum = 0
  for (digit in sequence) {
    val cur = digit.toString().toInt()
    if (cur == prev) {
      sum += cur
    }
    prev = cur
  }
  return sum
}

fun sumAcross(string: String): Int {
  val ints = string.map { Character.getNumericValue(it) }
  val halfSize = ints.size/2
  var sum = 0
  for ((i, d) in ints.withIndex()) {
    val crossIndex = if (i < halfSize) i + halfSize else i - halfSize
    if (d == ints[crossIndex]) {
      sum += d
    }
  }

  return sum
}