import java.io.File

class Part1 {
  data class Monkey(val name: String)
  enum class Operator {
    PLUS {
      override fun apply(op1: Long, op2: Long) = op1 + op2
    },
    MINUS {
      override fun apply(op1: Long, op2: Long) = op1 - op2
    },
    TIMES {
      override fun apply(op1: Long, op2: Long) = op1 * op2
    },
    DIVIDE {
      override fun apply(op1: Long, op2: Long) = op1 / op2
    };

    abstract fun apply(op1: Long, op2: Long): Long
  }
  data class Operation(val operator: Operator, val right: Monkey, val left: Monkey)

  fun run(input: String): Long {
    val calculatedValues = hashMapOf<Monkey, Long>()
    val (vals, ops) = input.lines().partition { line -> line.length < 12 }
    vals.forEach { line ->
      val monkey = Monkey(line.take(4))
      calculatedValues[monkey] = Regex("""\d+""").findAll(line).map { it.value.toLong() }.first()
    }
    val operations = ops.associate { line ->
      val monkey = Monkey(line.take(4))
      val op1 = Monkey(line.substring(6, 10))
      val op2 = Monkey(line.substring(13, 17))
      monkey to when {
        line.contains('+') -> Operation(Operator.PLUS, op1, op2)
        line.contains('-') -> Operation(Operator.MINUS, op1, op2)
        line.contains('*') -> Operation(Operator.TIMES, op1, op2)
        line.contains('/') -> Operation(Operator.DIVIDE, op1, op2)
        else -> throw AssertionError()
      }
    }

    fun resolveOperation(operation: Operation): Long {
      val (op, op1, op2) = operation;
      val v1 = calculatedValues[op1] ?: resolveOperation(operations[op1]!!)
      val v2 = calculatedValues[op2] ?: resolveOperation(operations[op2]!!)
      return operation.operator.apply(v1, v2)
    }

    return resolveOperation(operations[Monkey("root")]!!)
  }
}

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
