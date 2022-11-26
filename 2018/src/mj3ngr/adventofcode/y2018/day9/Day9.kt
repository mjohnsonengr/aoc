package mj3ngr.adventofcode.y2018.day9

fun runTest(players: Int, last: Int, highScore: Long) {
  println("$players players, last marble is $last")
  println("Expected: $highScore")
  println("Actual:   ${getHighScore(players, last)}")
  println()
}

fun run(players: Int, last: Int) {
  println(getHighScore(players, last))
}

data class Node(val value: Int) {
  lateinit var next: Node
  lateinit var prev: Node

  constructor(prev: Node, next: Node, value: Int) : this(value) {
    this.prev = prev
    this.next = next
    prev.next = this
    next.prev = this
  }

  fun getPrev(n: Int): Node {
    return if (n == 0) this else prev.getPrev(n-1)
  }

  fun remove() {
    this.next.prev = this.prev
    this.prev.next = this.next
  }
}

fun getHighScore(players: Int, last: Int): Long {
  val scores: Array<Long> = Array(players) { 0L }

  var currentPlayer = 1
  val firstMarble = Node(0)
  var currentMarble = Node(firstMarble, firstMarble, 1)
  for (i in 2..last) {
    if (i % 23 == 0) {
      scores[currentPlayer] = scores[currentPlayer] + i
      currentMarble = currentMarble.getPrev(7)
      val toRemove = currentMarble
      currentMarble = toRemove.next
      toRemove.remove()
      scores[currentPlayer] = scores[currentPlayer] + toRemove.value
    } else {
      currentMarble = Node(currentMarble.next, currentMarble.next.next, i)
    }
    currentPlayer = (currentPlayer + 1) % players
  }

  return scores.sortedDescending().first()
}

fun main(args: Array<String>) {
  println("Test")
//  runTest(9, 25, 32)
  runTest(10, 1618, 8317)
  runTest(13, 7999, 146373)
  runTest(17, 1104, 2764)
  runTest(21, 6111, 54718)
  runTest(30, 5807, 37305)
  println()
  println("Prod")
  run(462, 71938)
  run(462, 7193800)
}


