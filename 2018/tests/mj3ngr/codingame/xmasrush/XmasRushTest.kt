package mj3ngr.codingame.xmasrush

import org.junit.jupiter.api.Test
import java.util.*

class XmasRushTest {
  @Test
  fun `tiles are parsed`() {
    val input = Scanner("""
      0110 1001 1010 1110 0011 1011 0101
      1111 1001 1010 1001 0111 1010 1110
      0110 1110 1010 0011 1010 0110 0111
      0110 0111 1101 1111 0111 1101 1001
      1101 1001 1010 1100 1010 1011 1001
      1011 1010 1101 0110 1010 0110 1111
      0101 1110 1100 1011 1010 0110 1001
    """.trimIndent())

    val board = parseTiles(input)
    printBoard(board, System.out)
  }

  @Test
  fun `everything is parsed`() {
    val input = Scanner(turnData1)
    val (turnType, board, me, them, items, quests) = parseGameState(input)
    println(turnType)
    printBoard(board, System.out)
    println(me)
    println(them)
    println(items)
    println(quests)
  }

  @Test
  fun `first turn works`() {
    val scanner = Scanner(turnData1)
    gameLoop(scanner)
  }


  private val turnData1 = """
  0
  0110 1001 1010 1110 0011 1011 0101
  1111 1001 1010 1001 0111 1010 1110
  0110 1110 1010 0011 1010 0110 0111
  0110 0111 1101 1111 0111 1101 1001
  1101 1001 1010 1100 1010 1011 1001
  1011 1010 1101 0110 1010 0110 1111
  0101 1110 1100 1011 1010 0110 1001
  1 0 0 0111
  1 6 6 1101
  2
  DIAMOND 1 6 0
  DIAMOND 5 0 1
  2
  DIAMOND 0
  DIAMOND 1
  """.trimIndent()
}
