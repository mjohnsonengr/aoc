package mj3ngr.codingame.xmasrush

import java.io.PrintStream
import java.util.*

fun main(args: Array<String>) {
  val input = Scanner(System.`in`)
  while (true) {
    println(gameLoop(input))
  }
}

fun gameLoop(input: Scanner): String {
  val (turnType, board, me, them, items, quests) = parseGameState(input)

  return if (turnType == 0) {
    pushTurn(board, me, them, items, quests)
  } else {
    moveTurn(board, me, them, items, quests)
  }
}

typealias GameBoard = List<List<Tile>>
typealias Items = List<Item>
typealias Quests = List<Quest>

data class GameState(
  val turnType: Int,
  val board: GameBoard,
  val me: Player,
  val them: Player,
  val items: Items,
  val quests: Quests)

data class Tile(val up: Boolean, val right: Boolean, val down: Boolean, val left: Boolean) {
  constructor(d: String) : this(
    up = toBool(d[0]),
    right = toBool(d[1]),
    down = toBool(d[2]),
    left = toBool(d[3])
  )

  constructor(input: Scanner) : this(input.next())

  override fun toString(): String =
    when ("${toChar(up)}${toChar(right)}${toChar(down)}${toChar(left)}") {
      "0000" -> " "
      "0001" -> "╴"
      "0010" -> "╷"
      "0011" -> "┐"
      "0100" -> "╶"
      "0101" -> "─"
      "0110" -> "┌"
      "0111" -> "┬"
      "1000" -> "╵"
      "1001" -> "┘"
      "1010" -> "│"
      "1011" -> "┤"
      "1100" -> "└"
      "1101" -> "┴"
      "1110" -> "├"
      "1111" -> "┼"
      else -> throw AssertionError()
    }
}

data class Player(val numPlayerCards: Int, val x: Int, val y: Int, val tile: Tile) {
  constructor(input: Scanner) : this(input.nextInt(), input.nextInt(), input.nextInt(), Tile(input))
}

data class Item(val name: String, val x: Int, val y: Int, val playerId: Int) {
  constructor(input: Scanner) : this(input.next(), input.nextInt(), input.nextInt(), input.nextInt())
}

data class Quest(val itemName: String, val playerId: Int) {
  constructor(input: Scanner) : this(input.next(), input.nextInt())
}
fun toBool(c: Char) = c == '1'
fun toChar(b: Boolean) = if (b) '1' else '0'

fun parseGameState(input: Scanner) = GameState(
  input.nextInt(),
  parseTiles(input),
  Player(input),
  Player(input),
  parseItems(input),
  parseQuests(input))

fun parseTiles(input: Scanner): GameBoard {
  val tiles = ArrayList<ArrayList<Tile>>()

  for (i in 0 until 7) {
    val row = ArrayList<Tile>()
    for (j in 0 until 7) {
      row.add(Tile(input))
    }
    tiles.add(row)
  }

  return tiles.toList()
}

fun printBoard(board: GameBoard, stream: PrintStream) {
  stream.println("    1    2    3    4    5    6    7")
  for ((i, row) in board.withIndex()) {
    // print top filler
    stream.print("  ")
    for (col in row) {
      stream.print(if (col.up) "  │  " else "     ")
    }
    stream.println()

    // print actual row
    stream.print("$i:")
    for (col in row) {
      stream.print(if (col.left) "──" else "  ")
      stream.print(col)
      stream.print(if (col.right) "──" else "  ")
    }
    stream.println()

    // print bottom filler
    stream.print("  ")
    for (col in row) {
      stream.print(if (col.down) "  │  " else "     ")
    }
    stream.println()
  }
}

fun parseItems(input: Scanner): Items {
  val items = ArrayList<Item>()
  val n = input.nextInt()
  for (i in 0 until n) {
    items.add(Item(input))
  }
  return items.toList()
}

fun parseQuests(input: Scanner): Quests {
  val quests = ArrayList<Quest>()
  val n = input.nextInt()
  for (i in 0 until n) {
    quests.add(Quest(input))
  }
  return quests.toList()
}

fun pushTurn(tiles: GameBoard, me: Player, them: Player, items: Items, quests: Quests): String {
  // initial strategy: push item I need closer to me.
  return "PUSH 3 RIGHT"
}

fun moveTurn(tiles: GameBoard, me: Player, them: Player, items: Items, quests: Quests): String {
  // path-finding algo towards desired item, or away from edge.
  // version 1: away from edge.
  return "MOVE UP"
}
