import java.io.File

class Part2 {
  data class Monkey(val name: String)
  enum class Operator {
    PLUS {
      override fun apply(op1: Long, op2: Long) = op1 + op2
      override fun toString() = "+"
    },
    MINUS {
      override fun apply(op1: Long, op2: Long) = op1 - op2
      override fun toString() = "-"
    },
    TIMES {
      override fun apply(op1: Long, op2: Long) = op1 * op2
      override fun toString() = "*"
    },
    DIVIDE {
      override fun apply(op1: Long, op2: Long) = op1 / op2
      override fun toString() = "/"
    };

    fun opposite() =
        when (this) {
          PLUS -> MINUS
          MINUS -> PLUS
          TIMES -> DIVIDE
          DIVIDE -> TIMES
        }

    fun apply(op1: Value, op2: Value) = Value(apply(op1.value, op2.value))
    abstract fun apply(op1: Long, op2: Long): Long
  }
  data class Operation(val operator: Operator, val right: Monkey, val left: Monkey)

  abstract class Operand
  data class Value(val value: Long) : Operand() {
    override fun toString() = value.toString()
  }
  data class Variable(val name: String) : Operand() {
    override fun toString() = name
  }
  data class Resolved(val operator: Operator, val left: Operand, val right: Operand) : Operand() {
    fun unravel(c: Value): Value {
      if (right is Variable || left is Variable) {
        val value = if (right is Value) right else left as Value
        return operator.opposite().apply(c, value)
      }

      if ((operator == Operator.DIVIDE || operator == Operator.MINUS) && right is Resolved) {
        val value = left as Value
        val eq = right
        val newC = operator.apply(value, c)
        println("$value${operator}$c=$newC")
        return eq.unravel(newC)
      }

      val value = if (right is Value) right else left as Value
      val eq = if (right is Resolved) right else left as Resolved
      val newC = operator.opposite().apply(c, value)
      println("$c${operator.opposite()}$value=$newC")
      return eq.unravel(newC)
    }

    override fun toString(): String {
      return "($left$operator$right)"
    }
  }

  fun run(input: String): Long {
    val calculatedValues = hashMapOf<Monkey, Long>()
    val (vals, ops) = input.lines().partition { line -> line.length < 12 }
    vals.forEach { line ->
      val monkey = Monkey(line.take(4))
      calculatedValues[monkey] = Regex("""\d+""").findAll(line).map { it.value.toLong() }.first()
    }
    val operations =
        ops.associate { line ->
          val monkey = Monkey(line.take(4))
          val op1 = Monkey(line.substring(6, 10))
          val op2 = Monkey(line.substring(13, 17))
          monkey to
              when {
                line.contains('+') -> Operation(Operator.PLUS, op1, op2)
                line.contains('-') -> Operation(Operator.MINUS, op1, op2)
                line.contains('*') -> Operation(Operator.TIMES, op1, op2)
                line.contains('/') -> Operation(Operator.DIVIDE, op1, op2)
                else -> throw AssertionError()
              }
        }

    fun buildResolved(monkey: Monkey): Operand {
      if (monkey == Monkey("humn")) {
        return Variable("H")
      }
      if (calculatedValues.contains(monkey)) {
        return Value(calculatedValues[monkey]!!)
      }
      val (op, op1, op2) = operations[monkey]!!
      val v1 = buildResolved(op1)
      val v2 = buildResolved(op2)
      return if (v1 is Value && v2 is Value) {
        op.apply(v1, v2)
      } else {
        Resolved(op, v1, v2)
      }
    }

    val root = operations[Monkey("root")]!!
    val left = buildResolved(root.left)
    val right = buildResolved(root.right)
    println(left)
    println(right)
    println()
    val c = if (left is Value) left else right as Value
    val eq = if (left is Resolved) left else right as Resolved
    val maybeAnswer = eq.unravel(c).value

    fun resolveOperation(monkey: Monkey): Long {
      if (calculatedValues.contains(monkey)) {
        return calculatedValues[monkey]!!
      }
      val (op, op1, op2) = operations[monkey]!!
      val v1 = resolveOperation(op1)
      val v2 = resolveOperation(op2)
      return op.apply(v1, v2)
    }

    calculatedValues[Monkey("humn")] = maybeAnswer
    val sideWithHuman = if (left is Value) root.right else root.left
    val resolved = resolveOperation(sideWithHuman)
    println("Does this satisfy?")
    println("$resolved = ${c.value}")
    require(resolved == c. value)
    return maybeAnswer.toLong()
  }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
