import java.io.File

class Part1 {
  inline operator fun <T> List<T>.component6(): T = get(5)

  fun run(input: String): Int {
    val inputGroups = input.split("\n\n")
    val monkeys =
        inputGroups.map { group ->
          val lines = group.lines()
          Monkey(
              lines[0][7].digitToInt(),
              lines[1]
                  .substringAfter("Starting items: ")
                  .split(',')
                  .map { it.trimIndent().toInt() }
                  .toMutableList(),
              Operation.of(lines[2].split('=')[1].trimIndent()),
              lines[3].substringAfter("  Test: divisible by ").toInt(),
              lines[4].substringAfter("    If true: throw to monkey ").toInt(),
              lines[5].substringAfter("    If false: throw to monkey ").toInt())
        }

    (1..20).forEach { i ->
      monkeys.forEach { monkey ->
        monkey.items.toList().forEach {
          monkey.inspected++
          val worry = monkey.operation.apply(it) / 3
          monkey.items.remove(it)
          // throw:
          monkeys[if (worry % monkey.divTest == 0) monkey.ifTrue else monkey.ifFalse]
              .items
              .add(worry)
        }
      }
      println("Round $i")
      monkeys.forEach { println("Monkey ${it.index}: ${it.items}") }
    }

    println(monkeys)
    val active = monkeys.map { it.inspected }.sortedDescending().take(2)
    return active[0] * active[1]
  }

  data class Monkey(
      val index: Int,
      val items: MutableList<Int>,
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
    fun apply(old: Int) = operator.apply(left.getValue(old), right.getValue(old))
  }
  interface Operand {
    companion object {
      fun of(o: String): Operand = if (o == "old") Old() else IntValue(o.toInt())
    }
    fun getValue(old: Int): Int
  }
  class Old : Operand {
    override fun getValue(old: Int) = old
  }
  data class IntValue(val value: Int) : Operand {
    override fun getValue(old: Int) = value
  }
  enum class Operator {
    PLUS {
      override fun apply(left: Int, right: Int) = left + right
    },
    TIMES {
      override fun apply(left: Int, right: Int) = left * right
    };

    abstract fun apply(left: Int, right: Int): Int

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
  println(Part1().run(file.readText().trimIndent()))
}
