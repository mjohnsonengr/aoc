package mj3ngr.adventofcode.y2018.day13

import java.io.File

enum class Direction(
  private val char: Char,
  val nextX: (Point) -> Int,
  val nextY: (Point) -> Int,
  val turnLeft: () -> Direction,
  val turnRight: () -> Direction
) {
  LEFT('<', { it.x - 1 }, { it.y }, { DOWN }, { UP }),
  UP('^', { it.x }, { it.y - 1 }, { LEFT }, { RIGHT }),
  DOWN('v', { it.x }, { it.y + 1 }, { RIGHT }, { LEFT }),
  RIGHT('>', { it.x + 1 }, { it.y }, { UP }, { DOWN });

  fun turn(turn: Turn) = when (turn) {
    Turn.LEFT -> turnLeft()
    Turn.STRAIGHT -> this
    Turn.RIGHT -> turnRight()
  }

  override fun toString(): String {
    return char.toString()
  }
}

enum class Turn(val nextTurn: () -> Turn) {
  LEFT({ STRAIGHT }),
  STRAIGHT({ RIGHT }),
  RIGHT({ LEFT })
}

data class Point(val x: Int, val y: Int) {
  companion object {
    fun nextFor(cart: Cart): Point {
      return nextFor(cart.point, cart.direction)
    }

    fun nextFor(point: Point, direction: Direction): Point {
      return Point(direction.nextX(point), direction.nextY(point))
    }
  }
}

data class Cart(val point: Point, val direction: Direction, val turn: Turn) {
  fun move() = Cart(Point.nextFor(this), direction, turn)

  fun handleIntersection() = Cart(point, direction.turn(turn), turn.nextTurn())
  fun turnLeft() = Cart(point, direction.turnLeft(), turn)
  fun turnRight() = Cart(point, direction.turnRight(), turn)

  fun print() = direction.toString()
}

enum class Piece(private val char: Char, val updateCart: (Cart) -> Cart) {
  HORIZONTAL('-', { it }),
  VERTICAL('|', { it }),
  INTERSECTION('+', { it.handleIntersection() }),
  TOP_LEFT('/', {
    when (it.direction) {
      Direction.LEFT -> it.turnLeft()
      Direction.UP -> it.turnRight()
      else -> throw AssertionError()
    }
  }),
  TOP_RIGHT('\\', {
    when (it.direction) {
      Direction.RIGHT -> it.turnRight()
      Direction.UP -> it.turnLeft()
      else -> throw AssertionError()
    }
  }),
  BOTTOM_LEFT('\\', {
    when (it.direction) {
      Direction.LEFT -> it.turnRight()
      Direction.DOWN -> it.turnLeft()
      else -> throw AssertionError()
    }
  }),
  BOTTOM_RIGHT('/', {
    when (it.direction) {
      Direction.RIGHT -> it.turnLeft()
      Direction.DOWN -> it.turnRight()
      else -> throw AssertionError()
    }
  });

  override fun toString(): String {
    return char.toString()
  }
}

interface CollisionState
class NoCollision : CollisionState
data class Collision(val point: Point) : CollisionState

interface CartPresence
class NoCart : CartPresence
data class HasCart(val cart: Cart) : CartPresence

data class Track(val point: Point, val piece: Piece, val cart: CartPresence) {
  fun clearTrack() = Track(point, piece, NoCart())
  fun receiveCart(received: Cart) = Pair(
    Track(point, piece, HasCart(piece.updateCart(received))), when (cart) {
      is NoCart -> NoCollision()
      is HasCart -> Collision(point)
      else -> AssertionError("invalid CartPresence")
    }
  )

  fun print() = when (cart) {
    is NoCart -> piece.toString()
    is HasCart -> cart.cart.print()
    else -> throw AssertionError("invalid CartPresence")
  }
}

data class TurnState(val collision: CollisionState, val tracks: Tracks)

typealias Tracks = Map<Point, Track>
typealias MutableTracks = HashMap<Point, Track>

fun Tracks.getCarts() =
  this.values.filter { it.cart is HasCart }.map { (it.cart as HasCart).cart }

fun playTurn(tracks: Tracks): TurnState {
  val maxX = tracks.map { it.key.x }.max()!!
  val maxY = tracks.map { it.key.y }.max()!!

  val cartsMap = tracks.getCarts().associateBy { it.point }

  val newTracks = tracks.toMutableMap()

  val collisions = ArrayList<Collision>()

  for (y in 0..maxY) {
    for (x in 0..maxX) {
      val point = Point(x, y)
      val cart = cartsMap[point]
      if (cart != null) {
        // Clear previous track
        newTracks[point] = newTracks[point]!!.clearTrack()

        // Move the cart
        val movedCart = cart.move()
        val newPoint = movedCart.point

        // Update new track
        val (newTrack, collisionState) = newTracks[newPoint]!!.receiveCart(movedCart)
        if (collisionState is Collision) {
          collisions.add(collisionState)
        }
        newTracks[newPoint] = newTrack
      }
    }
  }
  return TurnState(collisions.firstOrNull() ?: NoCollision(), newTracks)
}

fun printTracks(tracks: Tracks) {
  val maxX = tracks.map { it.key.x }.max()!!
  val maxY = tracks.map { it.key.y }.max()!!
  val tensSb = StringBuilder(maxX+5)
  tensSb.append(" ".repeat(5))
  val onesSb = StringBuilder(maxX+5)
  onesSb.append(" ".repeat(5))
  for (x in 0..maxX) {
    tensSb.append(x/10-x/100*10)
    onesSb.append(x%10)
  }
  println(tensSb.toString())
  println(onesSb.toString())
  for (y in 0..maxY) {
    val sb = StringBuilder(maxX+5)
    sb.append("${y.toString().padStart(3)}: ")
    for (x in 0..maxX) {
      sb.append(tracks[Point(x, y)]?.print() ?: " ")
    }
    println(sb.toString())
  }
}

fun parseLines(lines: List<String>): Tracks {
  val tracks = MutableTracks()

  val validLeftOfCorner = setOf(Piece.HORIZONTAL, Piece.INTERSECTION, Piece.BOTTOM_LEFT)
  fun parseBackslash(tracks: Tracks, point: Point) =
    if (validLeftOfCorner.contains(tracks[Point(point.x - 1, point.y)]?.piece))
      Piece.TOP_RIGHT
    else
      Piece.BOTTOM_LEFT

  fun parseSlash(tracks: Tracks, point: Point) =
    if (validLeftOfCorner.contains(tracks[Point(point.x - 1, point.y)]?.piece))
      Piece.BOTTOM_RIGHT
    else
      Piece.TOP_LEFT

  for ((y, line) in lines.withIndex()) {
    for ((x, char) in line.withIndex()) {
      if (char == ' ') continue
      val point = Point(x, y)
      tracks[point] = Track(
        point,
        when (char) {
          '|' -> Piece.VERTICAL
          '^' -> Piece.VERTICAL
          'v' -> Piece.VERTICAL
          '-' -> Piece.HORIZONTAL
          '>' -> Piece.HORIZONTAL
          '<' -> Piece.HORIZONTAL
          '\\' -> parseBackslash(tracks, point)
          '/' -> parseSlash(tracks, point)
          '+' -> Piece.INTERSECTION
          else -> throw AssertionError("invalid char parse")
        },
        if ("<>^v".contains(char))
          HasCart(
            Cart(
              point, when (char) {
                '<' -> Direction.LEFT
                '>' -> Direction.RIGHT
                '^' -> Direction.UP
                'v' -> Direction.DOWN
                else -> throw AssertionError()
              }, Turn.LEFT
            )
          )
        else NoCart()
      )
    }
  }

  return tracks.toMap()
}

fun run(filename: String, debug: Boolean = false) {
  var tracks = parseLines(File(filename).readLines())
  if (debug) {
    printTracks(tracks)
    println()
  }
  while (true) {
    val (collision, newTracks) = playTurn(tracks)
    if (debug) {
      printTracks(newTracks)
      println()
    }
    if (collision is Collision) {
      println(collision)
      return
    }
    tracks = newTracks
  }
}

fun main(args: Array<String>) {
  println("Test. Expected 7,3")
  run("data/2018/13test.txt")
  println("Prod.")
  run("data/2018/13.txt")
  println("Test part 2. Expected 6,4")
  run("data/2018/13test2.txt")
  println("Prod part 2.")
  run("data/2018/13.txt")
}
