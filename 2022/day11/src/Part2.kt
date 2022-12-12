import java.io.File
import java.lang.AssertionError

class Part2 {
  fun run(input: String): Long {
    val inputGroups = input.split("\n\n")
    val monkeys =
        inputGroups.map { group ->
          val lines = group.lines()
          Monkey(
              lines[0][7].digitToInt(),
              lines[1]
                  .substringAfter("Starting items: ")
                  .split(',')
                  .map { it.trimIndent().toLong() }
                  .toMutableList(),
              Operation.of(lines[2].split('=')[1].trimIndent()),
              lines[3].substringAfter("Test: divisible by ").toInt(),
              lines[4].substringAfter("If true: throw to monkey ").toInt(),
              lines[5].substringAfter("If false: throw to monkey ").toInt())
        }

    // I "cheated" here by looking on Reddit for hints.
    // How this works: when we're working with the worry levels, we don't actually care about
    // anything except the divisibility tests.
    // In other words, if the only divisibility test is "11", the numbers 15 and 26 are
    // equivalent.  If the divisibility tests are 5 and 7 then 39 and 74 are equivalent
    // 39 % 5 == 39 % 7 == 74 % 5 == 74 % 7 == 4
    // So in effect, we will only store the smallest version of these similar numbers
    val commonDivisor = monkeys.map { it.divTest }.fold(1) { a, b -> a * b }
    println("commonDivisor: $commonDivisor")

    (1..10000).forEach { i ->
      monkeys.forEach { monkey ->
        monkey.items.toList().forEach {
          monkey.inspected++
          val worry = monkey.operation.apply(it)
          monkey.items.remove(it)
          // throw:
          monkeys[if (worry % monkey.divTest == 0L) monkey.ifTrue else monkey.ifFalse]
              .items
              .add(worry % commonDivisor)
        }
      }
      if (i == 1 || i == 20 || i % 1000 == 0) {
        println("Round $i")
        monkeys.forEach { println("Monkey ${it.index} inspected items ${it.inspected} times") }
      }
    }

    //    println(monkeys)
    val active = monkeys.map { it.inspected }.sortedDescending().take(2)
    return active[0].toLong() * active[1].toLong()
  }

  data class Monkey(
      val index: Int,
      val items: MutableList<Long>,
      val operation: Operation,
      val divTest: Int,
      val ifTrue: Int,
      val ifFalse: Int,
      var inspected: Int = 0
  )

  data class Operation(val operator: Operator, val left: Operand, val right: Operand) {
    companion object {
      fun of(operation: String): Operation {
        val parts = operation.split(' ')
        return Operation(Operator.of(parts[1]), Operand.of(parts[0]), Operand.of(parts[2]))
      }
    }

    fun apply(old: Long) = operator.apply(left.getValue(old), right.getValue(old))
  }
  interface Operand {
    companion object {
      fun of(o: String): Operand = if (o == "old") Old() else IntValue(o.toLong())
    }
    fun getValue(old: Long): Long
  }
  class Old : Operand {
    override fun getValue(old: Long) = old
  }
  data class IntValue(val value: Long) : Operand {
    override fun getValue(old: Long) = value
  }
  enum class Operator {
    PLUS {
      override fun apply(left: Long, right: Long) = left + right
    },
    TIMES {
      override fun apply(left: Long, right: Long) = left * right
    };

    abstract fun apply(left: Long, right: Long): Long

    companion object {
      fun of(operator: String) =
          when (operator) {
            "+" -> PLUS
            "*" -> TIMES
            else -> throw AssertionError()
          }
    }
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
