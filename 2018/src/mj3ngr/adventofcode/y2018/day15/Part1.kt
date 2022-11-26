package mj3ngr.adventofcode.y2018.day15

import java.io.File
import java.util.*
import kotlin.math.abs

enum class Debug { TURN, ROUND, GAME, NONE }

var debug = Debug.NONE

data class Point(val x: Int, val y: Int) : Comparable<Point> {
  val neighbors by lazy {
    listOf(Point(x, y - 1), Point(x - 1, y), Point(x + 1, y), Point(x, y + 1))
  }

  fun distance(point: Point) = abs(x - point.x) + abs(y - point.y)

  override fun compareTo(other: Point) =
      compareValuesBy(this, other, { it.y }, { it.x })

  override fun toString() = "($x,$y)"
}


interface MaybePath

val NO_PATH = object : MaybePath {}

data class Path(val points: List<Point>) : MaybePath


abstract class ParentNode : Comparable<ParentNode> {
  abstract val point: Point
  abstract val target: Point
  abstract val path: List<Point>

  /** Manhattan distance */
  val distance by lazy {
    point.distance(target)
  }

  val value by lazy {
    distance + path.size
  }

  override fun compareTo(other: ParentNode) =
      compareValuesBy(this, other, { it.value }, { it.point })
}

data class RootNode(
    override val point: Point,
    override val target: Point) : ParentNode() {

  override val path = listOf(point)

  override fun toString() =
      "Root(" +
          "value=$value, " +
          "point=$point, " +
          "target=$target, " +
          "distance=$distance)"
}

data class Node(
    override val point: Point,
    override val target: Point,
    val parent: ParentNode) : ParentNode() {

  override val path by lazy {
    parent.path + point
  }

  override fun toString() =
      "Node(" +
          "value=$value, " +
          "point=$point, " +
          "target=$target, " +
          "distance=$distance, " +
          "parent=$parent.point)"
}


fun Iterable<ParentNode>.containsLesser(node: ParentNode): Boolean {
  val matchingNode = this.find { it.point == node.point } ?: return false
  if (matchingNode.value == node.value) {
    return matchingNode.path[1] < node.path[1]
  }
  return matchingNode.value < node.value
}

fun PriorityQueue<ParentNode>.replaceAll(nodes: Iterable<ParentNode>) =
    nodes.forEach { this.replace(it) }

fun PriorityQueue<ParentNode>.replace(node: ParentNode) {
  this.removeAll { it.point == node.point }
  this.add(node)
}

fun MutableSet<ParentNode>.replace(node: ParentNode) {
  this.removeAll { it.point == node.point }
  this.add(node)
}


interface Entity {
  val char: Char
}

val EMPTY = object : Entity {
  override val char = '.'
}
val WALL = object : Entity {
  override val char = '#'
}


interface Player : Entity {
  val point: Point
  val hitpoints: Int
  val attack: Int
  val round: Int

  fun new(point: Point, round: Int, hitpoints: Int, attack: Int): Player
  fun moveTo(newPoint: Point, newRound: Int) = new(newPoint, newRound, hitpoints, attack)
  fun damaged(damageTaken: Int) =
      if (hitpoints <= damageTaken)
        null
      else
        new(point, round, hitpoints - damageTaken, attack)
  fun update(newRound: Int) = new(point, newRound, hitpoints, attack)
}

data class Elf(
    override val point: Point,
    override val round: Int,
    override val hitpoints: Int = 200,
    override val attack: Int = 3) : Player {

  override val char = 'E'
  override fun new(point: Point, round: Int, hitpoints: Int, attack: Int) =
      Elf(point, round, hitpoints, attack)
}

data class Goblin(
    override val point: Point,
    override val round: Int,
    override val hitpoints: Int = 200,
    override val attack: Int = 3) : Player {

  override val char = 'G'
  override fun new(point: Point, round: Int, hitpoints: Int, attack: Int) =
      Goblin(point, round, hitpoints, attack)
}


fun Iterable<Entity?>.filterIsEnemy(player: Player) =
    this.filterIsInstance<Player>().filter { it::class !== player::class }


data class GameState(val board: Board, val rounds: Int) {
  constructor(rs: RoundState) : this(rs.board, rs.roundsCompleted)

  fun totalHitpoints() = board.getPlayers().sumBy { it.hitpoints }.toLong()
  fun calculate() = rounds * totalHitpoints()
}

data class RoundState(
    val board: Board, val rounds: Int, val completed: Boolean) {
  val roundsCompleted = rounds + if (completed) 1 else 0
}


typealias Board = Map<Point, Entity>

fun Board.isFinished() =
    getEnemiesOf(getPlayers().first()).isEmpty()
fun Board.getPlayers() = this.values.filterIsInstance<Player>()
fun Board.getEnemiesOf(player: Player) = this.values.filterIsEnemy(player)
fun Board.countElves() = this.values.filterIsInstance<Elf>().count()
fun Board.getAdjacentEnemiesOf(player: Player) =
    player.point.neighbors.map { this[it] }.filterIsEnemy(player)

fun Board.print() {
  val maxX = keys.map { it.x }.max()!!
  val maxY = keys.map { it.y }.max()!!
  for (y in 0..maxY) {
    val players = mutableListOf<Player>()
    for (x in 0..maxX) {
      val point = Point(x, y)
      val entity = this[point] ?: EMPTY
      if (entity is Player) {
        players.add(entity)
      }
      print(entity.char)
    }
    print("  ")
    players.forEach { print("${it.char}(${it.hitpoints}), ") }
    println()
  }
}

fun Board.updatePlayer(player: Player, round: Int): Pair<Player, Board> {
  val mutable = this.toMutableMap()
  val newPlayer = player.update(round)
  mutable[player.point] = newPlayer
  return Pair(newPlayer, mutable.toMap())
}

fun Board.movePlayer(
    player: Player, to: Point, round: Int): Pair<Player, Board> {
  val mutable = this.toMutableMap()
  mutable.remove(player.point)
  val newPlayer = player.moveTo(to, round)
  mutable[to] = newPlayer
  return Pair(newPlayer, mutable.toMap())
}

fun Board.damagePlayer(
    player: Player,
    damage: Int): Board {
  val mutable = this.toMutableMap()
  val maybeNewPlayer = player.damaged(damage)
  if (maybeNewPlayer != null) {
    mutable[player.point] = maybeNewPlayer
  } else {
    mutable.remove(player.point)
  }
  return mutable.toMap()
}


fun parseBoard(lines: List<String>, elfAttack: Int = 3): Board {
  val board = mutableMapOf<Point, Entity>()
  for ((y, line) in lines.withIndex()) {
    for ((x, char) in line.withIndex()) {
      if (char == '.') {
        continue
      }
      val point = Point(x, y)
      board[point] = when (char) {
        '#' -> WALL
        'G' -> Goblin(point, -1)
        'E' -> Elf(point, -1, attack = elfAttack)
        else -> throw AssertionError("Invalid character encountered in input")
      }
    }
  }
  return board.toMap()
}


tailrec fun simulateCombat(state: GameState): GameState = when {
  state.board.isFinished() -> state
  else -> simulateCombat(playRound(state))
}

fun playRound(state: GameState): GameState = GameState(
    playRound(state.board, state.rounds))

tailrec fun playRound(board: Board, round: Int): RoundState {
  val currentPlayer = board.getPlayers()
      .filter { it.round < round }
      .sortedBy { it.point }
      .first()
  val newBoard = playTurn(
      currentPlayer,
      board,
      round)

  val playersRemaining = newBoard.getPlayers().filter { it.round < round }.size
  val completed = playersRemaining == 0

  if (debug == Debug.TURN) {
    println("Turn: $currentPlayer")
    newBoard.print()
    println()
  }

  if (playersRemaining == 0 || newBoard.isFinished()) {
    if (debug == Debug.ROUND) {
      println("After ${round + if (completed) 1 else 0} rounds:")
      newBoard.print()
      println()
    }
    return RoundState(newBoard, round, completed)
  }
  return playRound(newBoard, round)
}

fun playTurn(player: Player, initBoard: Board, round: Int): Board =
    turnAttack(turnMove(player, initBoard, round))


fun turnMove(player: Player, board: Board, round: Int): Pair<Player, Board> {
  // Don't move if we're already next to an enemy.
  if (isNearEnemy(player, board)) {
    return board.updatePlayer(player, round)
  }

  // Identify all possible open squares adjacent to an enemy
  val nextPoints = board.getEnemiesOf(player)
      .flatMap { it.point.neighbors }
      .filter { !board.contains(it) }
      .sortedBy { it.distance(player.point) }

  var minPathLength = Int.MAX_VALUE
  val minPaths = mutableListOf<Path>()
  for (point in nextPoints) {
    val maybePath = findPath(board, player.point, point, minPathLength)
    if (maybePath is Path) {
      assert(maybePath.points.size <= minPathLength)
      if (maybePath.points.size < minPathLength) {
        minPathLength = maybePath.points.size
        minPaths.clear()
      }
      minPaths += maybePath
    }
  }

  val moveTo = minPaths
      .sortedBy { it.points.last() }
      .firstOrNull()?.points?.get(1)

  return if (moveTo != null)
    board.movePlayer(player, moveTo, round)
  else
    board.updatePlayer(player, round)
}

fun isNearEnemy(player: Player, board: Board): Boolean =
    player.point.neighbors
        .map { board[it] ?: EMPTY }
        .filterIsInstance<Player>()
        .any { it::class != player::class }

/** Returns a Path including start and target, or NO_PATH. */
fun findPath(board: Board, start: Point, target: Point, maxLength: Int): MaybePath {
  // TODO: What I should probably do is a single traversal that adds a path to
  // the list each time an enemy is encountered, rather than a traversal per
  // enemy.
  if (start == target) {
    return Path(listOf(start))
  }

  val visited = mutableSetOf<ParentNode>()  // Closed list
  val nextNodes = PriorityQueue<ParentNode>()  // Open list
  nextNodes.add(RootNode(start, target))

  while (nextNodes.isNotEmpty()) {
    val nextNode = nextNodes.poll()!!

    if (nextNode.point == target) {
      return Path(nextNode.path)
    }

    if (nextNode.path.size >= maxLength) {
      visited.replace(nextNode)
      continue
    }

    val successors = nextNode.point.neighbors.filter { !board.contains(it) }
        .map { Node(point = it, target = target, parent = nextNode) }
        .filterNot { nextNodes.containsLesser(it) }
        .filterNot { visited.containsLesser(it) }

    nextNodes.replaceAll(successors)

    visited.replace(nextNode)
  }

  return NO_PATH
}

fun turnAttack(playerBoard: Pair<Player, Board>): Board =
    turnAttack(playerBoard.first, playerBoard.second)

fun turnAttack(player: Player, board: Board): Board {
  val enemyToHit = board.getAdjacentEnemiesOf(player)
      .sortedWith(compareBy({ it.hitpoints }, { it.point }))
      .firstOrNull()
  return if (enemyToHit != null)
    board.damagePlayer(enemyToHit, player.attack)
  else
    board
}

fun moveTest() {
  val initBoard = """
    #########
    #G..G..G#
    #.......#
    #.......#
    #G..E..G#
    #.......#
    #.......#
    #G..G..G#
    #########
  """.trimIndent()
  var board = parseBoard(initBoard.lines())
  board.print()
  for (i in 0..3) {
    val sortedPlayers =
        board.getPlayers().sortedBy { it.point }
    for (player in sortedPlayers) {
      val (_, newBoard) = turnMove(player, board, i)
      board = newBoard
    }
    board.print()
  }
}

fun playTest1() {
  val initBoard = """
    #######
    #.G...#
    #...EG#
    #.#.#G#
    #..G#E#
    #.....#
    #######
  """.trimIndent()
  run(initBoard.lines())
  // Expected: 27730
  // #######
  // #G....#   G(200)
  // #.G...#   G(131)
  // #.#.#G#   G(59)
  // #...#.#
  // #....G#   G(200)
  // #######
}

fun playTest2() {
  val initBoard = """
    #######
    #G..#E#
    #E#E.E#
    #G.##.#
    #...#E#
    #...E.#
    #######
  """.trimIndent()
  run(initBoard.lines())
  // Expected: 36334
  // #######
  // #...#E#   E(200)
  // #E#...#   E(197)
  // #.E##.#   E(185)
  // #E..#E#   E(200), E(200)
  // #.....#
  // #######
}

fun playTest3() {
  val initBoard = """
    #######
    #E..EG#
    #.#G.E#
    #E.##E#
    #G..#.#
    #..E#.#
    #######
  """.trimIndent()
  run(initBoard.lines())
  // Expected: 39514
  // #######
  // #.E.E.#   E(164), E(197)
  // #.#E..#   E(200)
  // #E.##.#   E(98)
  // #.E.#.#   E(200)
  // #...#.#
  // #######
}

fun playTest4() {
  val initBoard = """
    #######
    #E.G#.#
    #.#G..#
    #G.#.G#
    #G..#.#
    #...E.#
    #######
  """.trimIndent()
  run(initBoard.lines())
  println("playTest4: Expected: 27755")
  // TODO this one is wrong right now.
  // #######
  // #G.G#.#   G(200), G(98)
  // #.#G..#   G(200)
  // #..#..#
  // #...#G#   G(95)
  // #...G.#   G(200)
  // #######
}

fun playTest5() {
  val initBoard = """
    #######
    #.E...#
    #.#..G#
    #.###.#
    #E#G#G#
    #...#G#
    #######
  """.trimIndent()
  run(initBoard.lines())
  // Expected: 28944
  // #######
  // #.....#
  // #.#G..#   G(200)
  // #.###.#
  // #.#.#.#
  // #G.G#G#   G(98), G(38), G(200)
  // #######
}

fun playTest6() {
  val initBoard = """
    #########
    #G......#
    #.E.#...#
    #..##..G#
    #...##..#
    #...#...#
    #.G...G.#
    #.....G.#
    #########
  """.trimIndent()
  run(initBoard.lines())
  // Expected: 18740
  // #########
  // #.G.....#   G(137)
  // #G.G#...#   G(200), G(200)
  // #.G##...#   G(200)
  // #...##..#
  // #.G.#...#   G(200)
  // #.......#
  // #.......#
  // #########
}

fun prod() {
  run(File("data/2018/15.txt").readLines())
}

fun part2Test1() {
  val initBoard = """
    #######
    #.G...#
    #...EG#
    #.#.#G#
    #..G#E#
    #.....#
    #######
  """.trimIndent()
  runPart2(initBoard.lines())
  // Expected: E=15; 4988
  // #######
  // #..E..#   E(158)
  // #...E.#   E(14)
  // #.#.#.#
  // #...#.#
  // #.....#
  // #######
}

fun part2Test2() {
  val initBoard = """
    #######
    #E..EG#
    #.#G.E#
    #E.##E#
    #G..#.#
    #..E#.#
    #######
  """.trimIndent()
  runPart2(initBoard.lines())
  // Expected: E=4; 31284
  // #######
  // #.E.E.#   E(200), E(23)
  // #.#E..#   E(200)
  // #E.##E#   E(125), E(200)
  // #.E.#.#   E(200)
  // #...#.#
  // #######
}

fun part2Test3() {
  val initBoard = """
    #######
    #E.G#.#
    #.#G..#
    #G.#.G#
    #G..#.#
    #...E.#
    #######
  """.trimIndent()
  runPart2(initBoard.lines())
  // Expected: E=15; 3478
  // #######
  // #.E.#.#   E(8)
  // #.#E..#   E(86)
  // #..#..#
  // #...#.#
  // #.....#
  // #######
}

fun part2Test4() {
  val initBoard = """
    #######
    #.E...#
    #.#..G#
    #.###.#
    #E#G#G#
    #...#G#
    #######
  """.trimIndent()
  runPart2(initBoard.lines())
  // Expected: E=12; 6474
  // #######
  // #...E.#   E(14)
  // #.#..E#   E(152)
  // #.###.#
  // #.#.#.#
  // #...#.#
  // #######
}

fun part2Test5() {
  val initBoard = """
    #########
    #G......#
    #.E.#...#
    #..##..G#
    #...##..#
    #...#...#
    #.G...G.#
    #.....G.#
    #########
  """.trimIndent()
  runPart2(initBoard.lines())
  // Expected: E=34; 1140
  // #########
  // #.......#
  // #.E.#...#   E(38)
  // #..##...#
  // #...##..#
  // #...#...#
  // #.......#
  // #.......#
  // #########
}

fun part2(elfPower: Int) {
  runPart2(File("data/2018/15.txt").readLines(), elfPower)
}

fun run(lines: List<String>) {
  println()
  val state = simulateCombat(GameState(parseBoard(lines), 0))
  state.board.print()
  println("Rounds: ${state.rounds}")
  println("Outcome: ${state.calculate()}")
}

fun runPart2(lines: List<String>, elfPower: Int = 4) {
  // TODO: possible optimizations (aside from fixing findPath):
  // * binary search to find the best game, rather than incrementally increasing elves power
  // * return early as soon as first elf dies
  // * as soon as I've finished implementing these though, I'll have seen an answer
  println()
  println("================")
  val initNumElves = parseBoard(lines).countElves()
  for (i in elfPower..Int.MAX_VALUE) {
    val state = simulateCombat(GameState(parseBoard(lines, i), 0))
    if (state.board.countElves() == initNumElves) {
      state.board.print()
      println("Elf power: $i")
      println("Rounds: ${state.rounds}")
      println("Outcome: ${state.calculate()}")
      break
    }
    if (debug == Debug.GAME) {
      println("Game: E=$i; Rounds=${state.rounds}, Outcome=${state.calculate()}, Elves remaining=${state.board.countElves()}")
    }
  }
}

fun main(args: Array<String>) {
  debug = Debug.TURN
//  moveTest()
//  playTest1()
//  playTest2()
//  playTest3()
//  playTest4()
//  playTest5()
//  playTest6()
//  prod()
//  part2Test1()
//  part2Test2()
//  part2Test3()
//  part2Test4()
//  part2Test5()
  part2(34)
}
