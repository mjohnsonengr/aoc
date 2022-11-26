package mj3ngr.adventofcode.y2018.day19

import java.io.File

private fun Boolean.toInt() = if (this) 1 else 0
private fun Int.toVar() = when(this) {
  0 -> "a"
  1 -> "b"
  2 -> "c"
  3 -> "d"
  4 -> "e"
  5 -> "f"
  else -> "undefined"
}

enum class Opcode(val run: (Op) -> Regs, val translate: (Inputs) -> String) {
  ADDR (
      { with(it) { regs.copy().set(c, regs[a] + regs[b]) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} + ${b.toVar()}" } }),
  ADDI (
      { with(it) { regs.copy().set(c, regs[a] + b) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} + $b" } }),
  MULR (
      { with(it) { regs.copy().set(c, regs[a] * regs[b]) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} * ${b.toVar()}" } }),
  MULI (
      { with(it) { regs.copy().set(c, regs[a] * b) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} * $b" } }),
  BANR (
      { with(it) { regs.copy().set(c, regs[a] and regs[b]) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} & ${b.toVar()}" } }),
  BANI (
      { with(it) { regs.copy().set(c, regs[a] and b) } },
      // TODO: short-circuit 0 if b == 0
      // TODO: short-circuit a if b != 0
      { with(it) { "${c.toVar()} = ${a.toVar()} & $b" } }),
  BORR (
      { with(it) { regs.copy().set(c, regs[a] or regs[b]) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} | ${b.toVar()}" } }),
  BORI (
      { with(it) { regs.copy().set(c, regs[a] or b) } },
      // TODO: short-circuit for values of b
      { with(it) { "${c.toVar()} = ${a.toVar()} | $b" } }),
  SETR (
      { with(it) { regs.copy().set(c, regs[a]) } },
      { with(it) { "${c.toVar()} = ${a.toVar()}" } }),
  SETI (
      { with(it) { regs.copy().set(c, a) } },
      { with(it) { "${c.toVar()} = $a" } }),
  GTIR (
      { with(it) { regs.copy().set(c, (a > regs[b]).toInt()) } },
      { with(it) { "${c.toVar()} = $a > ${b.toVar()}" } }),
  GTRI (
      { with(it) { regs.copy().set(c, (regs[a] > b).toInt()) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} > $b" } }),
  GTRR (
      { with(it) { regs.copy().set(c, (regs[a] > regs[b]).toInt()) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} > ${b.toVar()}" } }),
  EQIR (
      { with(it) { regs.copy().set(c, (a == regs[b]).toInt()) } },
      { with(it) { "${c.toVar()} = $a == ${b.toVar()}" } }),
  EQRI (
      { with(it) { regs.copy().set(c, (regs[a] == b).toInt()) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} == $b" } }),
  EQRR (
      { with(it) { regs.copy().set(c, (regs[a] == regs[b]).toInt()) } },
      { with(it) { "${c.toVar()} = ${a.toVar()} == ${b.toVar()}" } });
}

fun incrementInstr(regs: Regs, ip: Int) = regs.copy().set(ip, regs[ip] + 1)

data class Instruction(
    private val _opcode: String, val a: Int, val b: Int, val c: Int) {
  constructor(opcode: String, l: List<Int>) : this(opcode, l[0], l[1], l[2])
  fun inputs() = listOf(a, b, c)
  val opcode
    get() = Opcode.valueOf(_opcode.toUpperCase())
  override fun toString() = "$opcode $a $b $c"
  fun translate() = opcode.translate(Inputs(inputs()))
}

data class Inputs(val a: Int, val b: Int, val c: Int) {
  constructor(l: List<Int>) : this(l[0], l[1], l[2])
}

data class Op(val regs: Regs, val a: Int, val b: Int, val c: Int) {
  constructor(regs: Regs, l: List<Int>) : this(regs, l[0], l[1], l[2])
}

data class Regs(
    var a: Int, var b: Int, var c: Int, var d: Int, var e: Int, var f: Int) {

  operator fun get(i: Int) = when(i) {
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    4 -> e
    5 -> f
    else -> throw IndexOutOfBoundsException(i)
  }

  operator fun set(i: Int, v: Int): Regs {
    when (i) {
      0 -> a = v
      1 -> b = v
      2 -> c = v
      3 -> d = v
      4 -> e = v
      5 -> f = v
      else -> throw IndexOutOfBoundsException(i)
    }
    return this
  }

  fun copy() = Regs(a, b, c, d, e, f)

  override fun toString() = "[$a, $b, $c, $d, $e, $f]"
}

fun parseIp(line: String) =
    """#ip (\d)""".toRegex().find(line)!!.destructured.component1().toInt()

fun parseProgram(lines: List<String>): List<Instruction> {
  return lines.map { parseLine(it) }.map { Instruction(it[0], getInts(it)) }
}

private fun getInts(components: List<String>) =
    components.drop(1).map { it.toInt() }

private fun parseLine(line: String) =
    """([a-zA-Z]+) (\d+) (\d+) (\d+)""".toRegex()
        .find(line)!!.destructured.toList()

fun translateProgram(filename: String) {
  val lines = File(filename).readLines()
  val program = parseProgram(lines.drop(1))
  program.withIndex()
      .forEach { (i, it) ->
        println("${i.toString().padStart(2)}: ${it.translate()}")
      }
}

fun runPart1(filename: String) {
  run(filename, Regs(0, 0, 0, 0, 0, 0))
}

fun runPart2(filename: String) {
  run(filename, Regs(1, 0, 0, 0, 0, 0))
}

fun run(filename: String, initRegs: Regs) {
  val lines = File(filename).readLines()
  val ip = parseIp(lines[0])
  val program = parseProgram(lines.drop(1))

  var regs = initRegs
  while (regs[ip] < program.size) {
    print(regs)
    val instr = program[regs[ip]]
    print(" $instr ")
    regs = instr.opcode.run(Op(regs, instr.inputs()))
    regs = incrementInstr(regs, ip)
    println(regs)
  }
  println(regs[0])
}

fun main(args: Array<String>) {
  // Can optionally specify a different file via second arg.
  val filename = if (args.size < 2) "data/2018/19.txt" else args[1]
  val operation = if (args.isEmpty()) "part1" else args[0]
  when (operation) {
    "translate" -> translateProgram(filename)
    "part1" -> runPart1(filename)
    "part2" -> runPart2(filename)
    "program0" -> println(program(0))
    "program1" -> println(program(1))
  }
}

/** Manually decompiled/optimized code */
fun program(init: Int): Int {
  val f = if (init == 0) 929 else 10551329
  var a = 0

  for (d in 1..f step 2) {
    for (b in 1..f step 2) {
      if (d*b == f) {
        a += d
      }
      if (d*b > f) {
        break
      }
    }
  }

  return a
}