package mj3ngr.adventofcode.y2018

import java.io.File

data class ReductionResult(val polymer: String, val reductionsMade: Boolean)

fun react(a: Char, b: Char) = when {
  a.toLowerCase() != b.toLowerCase() -> false
  a.isLowerCase() == b.isLowerCase() -> false
  else -> true
}

fun reducePolymer(polymer: String): String {
  fun reduce(polymer: String): ReductionResult {
    var prev = polymer[0]
    for (i in 1 until polymer.length) {
      if (react(polymer[i], prev)) {
        return ReductionResult(polymer.removeRange(i - 1, i + 1), true)
      }
      prev = polymer[i]
    }
    return ReductionResult(polymer, false)
  }

  var currentPolymer = polymer
  while (true) {
    val (newPolymer, reductionsMade) = reduce(currentPolymer)
    if (!reductionsMade) {
      return newPolymer
    }
    currentPolymer = newPolymer
  }
}

fun minimizeReducedPolymer(reducedPolymer: String): Int {
  val newLengths: HashMap<Char, Int> = HashMap()
  for (c in 'a'..'z') {
    newLengths[c] = reducePolymer(reducedPolymer.replace(c.toString(), "", ignoreCase = true)).length
  }
  return newLengths.values.sorted().first()
}

fun main(args: Array<String>) {
  mainTest()
  mainProd()
}

fun mainTest() {
  println("Test")
  run("dabAcCaCBAcCcaDA")
}

fun mainProd() {
  println("Prod:")
  run(File("data/2018/5.txt").readLines()[0])
}

fun run(polymer: String) {
  val reduced = reducePolymer(polymer)
  printValues(reduced.length, minimizeReducedPolymer(reduced))
}

fun printValues(length: Int, newLength: Int) {
  println("Part 1: $length)")
  println("Part 2: $newLength")
}
