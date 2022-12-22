import Part2.Companion.INPUT_COORDS
import Part2.CubeFace.Companion.INPUT_MAPPING
import java.io.File

class Part2(
  val faceMapping: Map<Pair<CubeFace, Facing>, Pair<CubeFace, Facing>>,
  val faceCoords: Map<CubeFace, Pair<Int, Int>>
) {
  enum class CubeFace() {
    TOP,
    BOTTOM,
    LEFT,
    RIGHT,
    FRONT,
    BACK;

    companion object {
      // the sample input:
      // A: pitch moving up: top(U) -> back(U) -> bottom(D) -> front(U) ->
      // B: yaw moving right: front(R) -> right(R) -> back(L) -> left(D) ->
      // C: roll moving right: top(R) -> right(D) -> bottom(R) -> left(R) ->

      val SAMPLE_MAPPING = mapOf(
        // pitch rotation around cube:
        TOP to Facing.U to (BACK to Facing.U),
        BACK to Facing.U to (BOTTOM to Facing.D),
        BOTTOM to Facing.D to (FRONT to Facing.U),
        FRONT to Facing.U to (TOP to Facing.U),

        // pitch reverse
        TOP to Facing.D to (FRONT to Facing.D),
        FRONT to Facing.D to (BOTTOM to Facing.U),
        BOTTOM to Facing.U to (BACK to Facing.D),
        BACK to Facing.D to (TOP to Facing.D),

        // yaw
        FRONT to Facing.R to (RIGHT to Facing.R),
        RIGHT to Facing.R to (BACK to Facing.L),
        BACK to Facing.L to (LEFT to Facing.D),
        LEFT to Facing.D to (FRONT to Facing.R),

        // yaw reverse
        FRONT to Facing.L to (LEFT to Facing.U),
        LEFT to Facing.U to (BACK to Facing.R),
        BACK to Facing.R to (RIGHT to Facing.L),
        RIGHT to Facing.L to (FRONT to Facing.L),

        // roll
        TOP to Facing.R to (RIGHT to Facing.D),
        RIGHT to Facing.D to (BOTTOM to Facing.R),
        BOTTOM to Facing.R to (LEFT to Facing.R),
        LEFT to Facing.R to (TOP to Facing.R),

        // roll reverse
        TOP to Facing.L to (LEFT to Facing.L),
        LEFT to Facing.L to (BOTTOM to Facing.L),
        BOTTOM to Facing.L to (RIGHT to Facing.U),
        RIGHT to Facing.U to (TOP to Facing.L),
      )

      // there are 3 ways around the cube, and 6 faces.
      // A: pitch moving up: top(U) -> back(U) -> bottom(R) -> front(U) ->
      // B: yaw moving right: front(R) -> right(L) -> back(L) -> left(R) ->
      // C: roll moving right: top(R) -> right(U) -> bottom(U) -> left(U) ->
      val INPUT_MAPPING = mapOf(
        // pitch
        TOP to Facing.U to (BACK to Facing.U),
        BACK to Facing.U to (BOTTOM to Facing.R),
        BOTTOM to Facing.R to (FRONT to Facing.U),
        FRONT to Facing.U to (TOP to Facing.U),

        // pitch reverse
        TOP to Facing.D to (FRONT to Facing.D),
        FRONT to Facing.D to (BOTTOM to Facing.L),
        BOTTOM to Facing.L to (BACK to Facing.D),
        BACK to Facing.D to (TOP to Facing.D),

        // yaw
        FRONT to Facing.R to (RIGHT to Facing.L),
        RIGHT to Facing.L to (BACK to Facing.L),
        BACK to Facing.L to (LEFT to Facing.R),
        LEFT to Facing.R to (FRONT to Facing.R),

        // yaw reverse
        RIGHT to Facing.R to (FRONT to Facing.L),
        BACK to Facing.R to (RIGHT to Facing.R),
        LEFT to Facing.L to (BACK to Facing.R),
        FRONT to Facing.L to (LEFT to Facing.L),

        // roll
        TOP to Facing.R to (RIGHT to Facing.U),
        RIGHT to Facing.U to (BOTTOM to Facing.U),
        BOTTOM to Facing.U to (LEFT to Facing.U),
        LEFT to Facing.U to (TOP to Facing.R),

        // roll reverse
        TOP to Facing.L to (LEFT to Facing.D),
        LEFT to Facing.D to (BOTTOM to Facing.D),
        BOTTOM to Facing.D to (RIGHT to Facing.D),
        RIGHT to Facing.D to (TOP to Facing.L),
      )
    }
  }

  companion object {
    // TODO MAybe these could be programmatically determined by finding groups of 4 in the hRanges and vRanges
    // but then I'd also need to figure out how to determine which side is which
    val SAMPLE_COORDS = mapOf(
      CubeFace.BACK to (0 to 2),
      CubeFace.BOTTOM to (1 to 0),
      CubeFace.LEFT to (1 to 1),
      CubeFace.TOP to (1 to 2),
      CubeFace.FRONT to (2 to 2),
      CubeFace.RIGHT to (2 to 3),
    )

    val INPUT_COORDS = mapOf(
      CubeFace.BACK to (0 to 1),
      CubeFace.RIGHT to (0 to 2),
      CubeFace.TOP to (1 to 1),
      CubeFace.LEFT to (2 to 0),
      CubeFace.FRONT to (2 to 1),
      CubeFace.BOTTOM to (3 to 0),
    )
  }

  fun CubeFace.next(dir: Facing) = faceMapping.get(this to dir)!!

  data class CubeStep(val pos: Pos, val tile: Tile, val facing: Facing, val cubeFace: CubeFace)

  fun run(input: String): Int {
    val (gridIn, pathIn) = input.split("\n\n")
    val grid = parseGrid(gridIn)
    val path = parsePath(pathIn)

    val height = grid.size
    val width = grid.maxOf { it.size }
    val hRanges = grid.map { row -> range(row) }
    val vRanges =
      (0 until width).map { c -> range(grid.map { it.safeGet(c) }) }

    val faceDimension = (hRanges + vRanges).minOf { it.size }

    val allFaceRanges = faceCoords.mapValues { (k, v) ->
      val (r, c) = v
      r*faceDimension until (r+1)*faceDimension to (c*faceDimension until (c+1)*faceDimension)
    }

    fun CubeFace.entranceCoord(dir: Facing) = when(dir) {
      Facing.L -> allFaceRanges[this]!!.second.last
      Facing.R -> allFaceRanges[this]!!.second.first
      Facing.U -> allFaceRanges[this]!!.first.last
      Facing.D -> allFaceRanges[this]!!.first.first
    }

    fun Grid.straight(initPos: Pos, initFacing: Facing, initCubeFace: CubeFace): Sequence<CubeStep> {
      return generateSequence(CubeStep(initPos, this[initPos], initFacing, initCubeFace)) { (pos, _, facing, cubeFace) ->
        val (vFaceRange, hFaceRange) = allFaceRanges[cubeFace]!!

        val maybeNext = pos + facing.inc
        if (vFaceRange.contains(maybeNext.row) && hFaceRange.contains(maybeNext.col)) {
          return@generateSequence CubeStep(maybeNext, this[maybeNext], facing, cubeFace)
        }

        val (nextCubeFace, nextFacing) = cubeFace.next(facing)
        val newCubeRanges = allFaceRanges[nextCubeFace]!!
        val newVRange = newCubeRanges.first
        val newHRange = newCubeRanges.second
        val newNext = Pos.of(
          if (!vFaceRange.contains(maybeNext.row)) {
            // facing is U or D and we went past the top/bottom of current face
            if (nextFacing.axis == facing.axis) {
              nextCubeFace.entranceCoord(nextFacing) to
                  if (nextFacing.orientation == facing.orientation) {
                    maybeNext.col - hFaceRange.first + newHRange.first
                  } else {
                    hFaceRange.size - 1 - (maybeNext.col - hFaceRange.first) + newHRange.first
                  }
            } else {
              // other axis
              if (nextFacing.orientation == facing.orientation) {
                maybeNext.col - hFaceRange.first + newVRange.first
              } else {
                hFaceRange.size - 1 - (maybeNext.col - hFaceRange.first) + newVRange.first
              } to nextCubeFace.entranceCoord(nextFacing)
            }
          } else if (!hFaceRange.contains(maybeNext.col)) {
            // facing is R or L and we went past the right/left of current face
            if (nextFacing.axis == facing.axis) {
              if (nextFacing.orientation == facing.orientation) {
                maybeNext.row - vFaceRange.first + newVRange.first
              } else {
                vFaceRange.size - 1 - (maybeNext.row - vFaceRange.first) + newVRange.first
              } to nextCubeFace.entranceCoord(nextFacing)
            } else {
              // other axis
              nextCubeFace.entranceCoord(nextFacing) to
                  if (nextFacing.orientation == facing.orientation) {
                    maybeNext.row - vFaceRange.first + newHRange.first
                  } else {
                    vFaceRange.size - 1 - (maybeNext.row - vFaceRange.first) + newHRange.first
                  }
            }
          } else {
            throw AssertionError()
          })
        require(newVRange.contains(newNext.row) && newHRange.contains(newNext.col))
        return@generateSequence CubeStep(newNext, this[newNext], nextFacing, nextCubeFace)
      }
    }

    var curPos = Pos(0,
      grid.map { row -> row.indexOfFirst { it != Tile.VOID }..row.indexOfLast { it != Tile.VOID } }
        .first().first)
    var curFace = Facing.R
    var cubeFace = CubeFace.BACK
    val debugVisited = hashMapOf<Pos, Facing>()
    path.forEach { step ->
      when (step) {
        is Walk -> {
          val steps =
            grid.straight(curPos, curFace, cubeFace).take(step.steps + 1)
          val next = steps.windowed(2).firstOrNull { it[1].tile == Tile.WALL }
            ?.get(0)
            ?: steps.last()
          curPos = next.pos
          curFace = next.facing
          cubeFace = next.cubeFace
          debugVisited.putAll(steps.takeWhile { it.pos != curPos}.map { it.pos to curFace })
        }
        is Turn -> curFace = curFace.turn(step.dir)
      }
      debugVisited[curPos] = curFace
    }
    debugPrint(width, height, grid, debugVisited)

    return 1000*(curPos.row+1) + 4*(curPos.col+1) + curFace.value
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2(INPUT_MAPPING, INPUT_COORDS).run(file.readText().trimIndent()))
}
