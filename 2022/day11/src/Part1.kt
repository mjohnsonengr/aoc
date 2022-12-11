import java.io.File
import java.lang.AssertionError
import java.util.Scanner

class Part1 {
  fun run(input: String): Int {
    val inputGroups = input.split("\n\n")
    val monkeys =
      inputGroups.map { group ->
        val lines = group.lines()
        val monkey = lines[0][7].digitToInt()
        val startingItems = lines[1].split(':')[1].split(',')
          .map { it.trimIndent().toInt() }
        val operation = toOp(lines[2].split('=')[1].trimIndent())
        val divisibleBy = lines[3].substring("  Test: divisible by ".length).toInt()
        val ifTrue = lines[4].substring("    If true: throw to monkey ".length).toInt()
        val ifFalse = lines[5].substring("    If false: throw to monkey ".length).toInt()
        Monkey(monkey, startingItems.toMutableList(), operation, divisibleBy, ifTrue, ifFalse)
      }

    (1..20).forEach {
      monkeys.forEach { monkey ->
        monkey.items.toList().forEach {
          monkey.inspected++
          val worry = monkey.operation.apply(it) / 3
          monkey.items.remove(it)
          // throw:
          monkeys[if (worry % monkey.divTest == 0) monkey.ifTrue else monkey.ifFalse].items.add(worry)
        }
      }
      println("Round $it")
      monkeys.forEach {
        println("Monkey ${it.index}: ${it.items}")
      }
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
    var inspected: Int = 0)

  fun toOp(operation: String): Operation {
    val parts = operation.split(' ')
    return Operation(Operator.of(parts[1]), toOperand(parts[0]), toOperand(parts[2]))
  }
  fun toOperand(o: String): Operand = if (o == "old") Old() else IntValue(o.toInt())

  data class Operation(val operator: Operator, val left: Operand, val right: Operand) {
    fun apply(old: Int) = operator.apply(left.getValue(old), right.getValue(old))
  }
  interface Operand {
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
