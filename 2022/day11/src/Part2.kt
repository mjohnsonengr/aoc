import java.io.File
import java.lang.AssertionError
import java.math.BigInteger

class Part2 {
  fun run(input: String): BigInteger {
    val inputGroups = input.split("\n\n")
    val monkeys =
        inputGroups.map { group ->
          val lines = group.lines()
          val monkey = lines[0][7].digitToInt()
          val startingItems = lines[1].split(':')[1].split(',').map { BigInteger(it.trimIndent()) }
          val operation = toOp(lines[2].split('=')[1].trimIndent())
          val divisibleBy = BigInteger(lines[3].substring("  Test: divisible by ".length))
          val ifTrue = lines[4].substring("    If true: throw to monkey ".length).toInt()
          val ifFalse = lines[5].substring("    If false: throw to monkey ".length).toInt()
          Monkey(monkey, startingItems.toMutableList(), operation, divisibleBy, ifTrue, ifFalse)
        }

    // I cheated here by looking on Reddit for hints.
    // How this works: when we're working with the worry levels, we don't actually care about
    // anything except the divisibility tests.
    // In other words, if the only divisibility test is "11", the numbers 15 and 26 are
    // equivalent.  If the divisibility tests are 5 and 7 then 39 and 74 are equivalent
    // 39 % 5 == 39 % 7 == 74 % 5 == 74 % 7 == 4
    // So in effect, we will only store the smallest version of these similar numbers
    val commonDivisor = monkeys.map { it.divTest }.fold(BigInteger("1")) { a, b -> a * b }

    (1..10000).forEach {
      monkeys.forEach { monkey ->
        monkey.items.toList().forEach {
          monkey.inspected++
          val worry = monkey.operation.apply(it)
          monkey.items.remove(it)
          // throw:
          monkeys[if (worry.mod(monkey.divTest) == BigInteger.ZERO) monkey.ifTrue else monkey.ifFalse]
              .items
              .add(worry.mod(commonDivisor))
        }
      }
      if (it == 1 || it == 20 || it % 1000 == 0) {
        println("Round $it")
        monkeys.forEach {
          println("Monkey ${it.index} inspected items ${it.inspected} times")
        }
      }
    }

    //    println(monkeys)
    val active = monkeys.map { it.inspected }.sortedDescending().take(2)
    return active[0] * active[1]
  }

  data class Monkey(
      val index: Int,
      val items: MutableList<BigInteger>,
      val operation: Operation,
      val divTest: BigInteger,
      val ifTrue: Int,
      val ifFalse: Int,
      var inspected: BigInteger = BigInteger.ZERO
  )

  fun toOp(operation: String): Operation {
    val parts = operation.split(' ')
    return Operation(Operator.of(parts[1]), toOperand(parts[0]), toOperand(parts[2]))
  }
  fun toOperand(o: String): Operand = if (o == "old") Old() else IntValue(BigInteger(o))

  data class Operation(val operator: Operator, val left: Operand, val right: Operand) {
    fun apply(old: BigInteger) = operator.apply(left.getValue(old), right.getValue(old))
  }
  interface Operand {
    fun getValue(old: BigInteger): BigInteger
  }
  class Old : Operand {
    override fun getValue(old: BigInteger) = old
  }
  data class IntValue(val value: BigInteger) : Operand {
    override fun getValue(old: BigInteger) = value
  }
  enum class Operator {
    PLUS {
      override fun apply(left: BigInteger, right: BigInteger) = left + right
    },
    TIMES {
      override fun apply(left: BigInteger, right: BigInteger) = left * right
    };

    abstract fun apply(left: BigInteger, right: BigInteger): BigInteger

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
