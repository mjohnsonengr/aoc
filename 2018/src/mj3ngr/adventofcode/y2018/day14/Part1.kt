package mj3ngr.adventofcode.y2018.day14

fun main(args: Array<String>) {
  val value = 919901
  val scores = calculateScores(value+10)
  test(scores, 5, "0124515891")
  test(scores, 9, "5158916779")
  test(scores, 18, "9251071085")
  test(scores, 2018, "5941429882")
  println("Prod")
  println(getTenScores(scores, 919901))
}

fun test(scores: List<Int>, n: Int, expected: String) {
  println("Test $n. Expected $expected")
  println(getTenScores(scores, n))
}

fun getTenScores(scores: List<Int>, n: Int): String {
  val sb = StringBuilder(10)
  for (i in n until n+10) {
    sb.append(scores[i])
  }
  return sb.toString()
}

fun calculateScores(n: Int): List<Int> {
  val debug = false
  val scores = ArrayList<Int>(n)
  scores.add(3)
  scores.add(7)
  var elf1 = 0
  var elf2 = 1
  printState(scores, elf1, elf2, debug)
  while (scores.size < n) {
    val recipe1 = scores[elf1]
    val recipe2 = scores[elf2]
    val combined = recipe1 + recipe2
    val newRecipe1 = combined/10
    val newRecipe2 = combined%10
    if (newRecipe1 != 0) {
      scores.add(newRecipe1)
    }
    scores.add(newRecipe2)

    elf1 = (elf1 + 1 + recipe1)%scores.size
    elf2 = (elf2 + 1 + recipe2)%scores.size
    printState(scores, elf1, elf2, debug)
  }
  return scores.toList()
}

fun printState(scores: List<Int>, elf1: Int, elf2: Int, debug: Boolean = false) {
  if(debug) {
    for ((i, score) in scores.withIndex()) {
      val prefix = when (i) {
        elf1 -> '('
        elf2 -> '['
        else -> ' '
      }
      val postfix = when (i) {
        elf1 -> ')'
        elf2 -> ']'
        else -> ' '
      }
      print("$prefix$score$postfix")
    }
    println()
  }
}