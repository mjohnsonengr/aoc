import java.io.File

class Part1 {
  fun run(input: String): Int {
    // lower integer first
    // lists: compare first value of list; if all equal and left runs out first then right order
    // comparing list to int -> convert int to list with only that int
    val a = input
        .split("\n\n")
        .withIndex()
        .filter { (index, pair) ->
          val (left, right) = pair.lines()
          parse(left) < parse(right)
        }
    return a
        .map { it.index + 1 }
        .sum()
  }

  fun parse(packet: String): Lst {
    var lstStack = mutableListOf<Lst>(Lst())
    var inside = mutableListOf<Char>()
    packet.drop(1).dropLast(1).forEach {
      when (it) {
        '[' -> {
          // start a new list
          lstStack.last().addAll(inside)
          lstStack.add(Lst())
          inside.clear()
        }
        ']' -> {
          val oldLst = lstStack.removeLast()
          oldLst.addAll(inside)
          lstStack.last().add(oldLst)
          inside.clear()
        }
        else -> inside.add(it)
      }
    }
    lstStack.last().addAll(inside)

    return lstStack[0]
  }

  abstract class Contents {
    operator fun compareTo(other: Contents): Int {
      return when (this) {
        is Num -> {
          when (other) {
            is Num -> {
              this.compareTo(other)
            }
            is Lst -> {
              Lst.of(this).compareTo(other)
            }
            else -> throw AssertionError()
          }
        }
        is Lst -> {
          when (other) {
            is Num -> {
              this.compareTo(Lst.of(other))
            }
            is Lst -> {
              this.compareTo(other)
            }
            else -> throw AssertionError()
          }
        }
        else -> throw AssertionError()
      }
    }
  }

  data class Num(val value: Int) : Contents() {
    operator fun compareTo(other: Num): Int {
      return value.compareTo(other.value)
    }
  }
  data class Lst(val value: MutableList<Contents> = mutableListOf()) : Contents() {
    companion object {
      fun of(c: Contents): Lst {
        return Lst(mutableListOf(c))
      }
    }
    fun add(c: Contents): Boolean {
      return value.add(c)
    }

    fun addAll(c: List<Char>): Boolean {
      return value.addAll(c.joinToString("").split(',').mapNotNull { it.toIntOrNull() }.map { Num(it) })
    }

    operator fun compareTo(other: Lst): Int {
      value.zip(other.value).forEach { (left, right) ->
        val compare = left.compareTo(right)
        if (compare != 0) {
          return compare
        }
      }
      return value.size.compareTo(other.value.size)
    }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
