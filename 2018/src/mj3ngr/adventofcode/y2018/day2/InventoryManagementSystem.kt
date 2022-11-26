package mj3ngr.adventofcode.y2018.day2

import java.io.File

fun calculateChecksum(lines: Sequence<String>): Int {

  fun countDuplicates(line: String): Pair<Boolean, Boolean> {
    val counts = line.asSequence().groupBy { it }.map { it.value.size }.toList()
    return Pair(counts.asSequence().any { it == 2 }, counts.asSequence().any { it == 3 })
  }

  var withTwo = 0
  var withThree = 0
  for (line in lines) {
    val (two, three) = countDuplicates(line)
    if (two) withTwo++
    if (three) withThree++
  }
  return withTwo * withThree
}


fun findCommonBoxId(inputLines: Sequence<String>): String {

  /** Returns true if id1 and id2 only have one character in the same position different. */
  fun compareIds(id1: String, id2: String) =
    (id1.asSequence() zip id2.asSequence()).count { it.first != it.second } == 1

  fun getCommonIdChars(id1: String, id2: String) =
    (id1.asSequence() zip id2.asSequence())
      .filter { it.first == it.second }.map { it.first }.joinToString(separator = "")

  val lines = inputLines.toList()
  for (line in lines) {
    for (otherLine in lines) {
      if (compareIds(line, otherLine)) {
        return getCommonIdChars(line, otherLine)
      }
    }
  }

  throw IllegalArgumentException("Couldn't find matching ids")
}

fun main(args: Array<String>) {
//  val lines = sequenceOf("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
//  println(calculateChecksum(lines))

//  val lines = sequenceOf("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
//  println(findCommonBoxId(lines))

  val file = File(args[0])
  file.useLines { println(calculateChecksum(it)) }
  file.useLines { println(findCommonBoxId(it)) }
}
