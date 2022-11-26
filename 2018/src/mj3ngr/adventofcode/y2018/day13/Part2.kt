package mj3ngr.adventofcode.y2018.day13

import java.io.File

fun main(args: Array<String>) {
  println("Test part 2. Expected 6,4")
  runPart2("data/2018/13test2.txt")
  println("Prod part 2.")
  runPart2("data/2018/13.txt")
}

fun Track.receiveCartPart2(received: Cart) = Track(point, piece, when (cart) {
  is NoCart -> HasCart(piece.updateCart(received))
  is HasCart -> NoCart()
  else -> throw AssertionError("invalid CartPresence")
})

fun playTurnPart2(tracks: Tracks): Tracks {
  val maxX = tracks.map { it.key.x }.max()!!
  val maxY = tracks.map { it.key.y }.max()!!

  val cartsMap = tracks.getCarts().associateBy { it.point }

  val newTracks = tracks.toMutableMap()

  for (y in 0..maxY) {
    for (x in 0..maxX) {
      val point = Point(x, y)
      val cart = cartsMap[point]
      if (cart != null) {
        if (newTracks[point]!!.cart is NoCart) {
          // This card already collided and has been removed from the track.
          continue
        }
        // Clear previous track
        newTracks[point] = newTracks[point]!!.clearTrack()

        // Move the cart
        val movedCart = cart.move()
        val newPoint = movedCart.point

        // Update new track
        newTracks[newPoint] = newTracks[newPoint]!!.receiveCartPart2(movedCart)
      }
    }
  }
  return newTracks
}


fun runPart2(filename: String, debug: Boolean = false) {
  var tracks = parseLines(File(filename).readLines())
  if (debug) {
    printTracks(tracks)
    println()
  }
  while (true) {
    tracks = playTurnPart2(tracks)
    if (debug) {
      printTracks(tracks)
      println()
    }
    val carts = tracks.getCarts()
    if (carts.isEmpty()) {
      throw AssertionError("No carts left")
    }
    if (carts.size == 1) {
      println(carts[0].point)
      return
    }
  }
}

