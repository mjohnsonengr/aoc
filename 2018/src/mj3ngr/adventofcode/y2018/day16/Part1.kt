package mj3ngr.adventofcode.y2018.day16

import java.io.File

private fun Boolean.toInt() = if (this) 1 else 0

enum class Opcode(val run: (Op) -> Regs) {
  ADDR ({ with(it) { regs.copy().set(c, regs[a] + regs[b]) } }),
  ADDI ({ with(it) { regs.copy().set(c, regs[a] + b) } }),
  MULR ({ with(it) { regs.copy().set(c, regs[a] * regs[b]) } }),
  MULI ({ with(it) { regs.copy().set(c, regs[a] * b) } }),
  BANR ({ with(it) { regs.copy().set(c, regs[a] and regs[b]) } }),
  BANI ({ with(it) { regs.copy().set(c, regs[a] and b) } }),
  BORR ({ with(it) { regs.copy().set(c, regs[a] or regs[b]) } }),
  BORI ({ with(it) { regs.copy().set(c, regs[a] or b) } }),
  SETR ({ with(it) { regs.copy().set(c, regs[a]) } }),
  SETI ({ with(it) { regs.copy().set(c, a) } }),
  GTIR ({ with(it) { regs.copy().set(c, (a > regs[b]).toInt()) } }),
  GTRI ({ with(it) { regs.copy().set(c, (regs[a] > b).toInt()) } }),
  GTRR ({ with(it) { regs.copy().set(c, (regs[a] > regs[b]).toInt()) } }),
  EQIR ({ with(it) { regs.copy().set(c, (a == regs[b]).toInt()) } }),
  EQRI ({ with(it) { regs.copy().set(c, (regs[a] == b).toInt()) } }),
  EQRR ({ with(it) { regs.copy().set(c, (regs[a] == regs[b]).toInt()) } });
}

data class Instruction(val opcode: Int, val a: Int, val b: Int, val c: Int) {
  constructor(l: List<Int>) : this(l[0], l[1], l[2], l[3])
  fun inputs() = listOf(a, b, c)
}

data class Op(val regs: Regs, val a: Int, val b: Int, val c: Int) {
  constructor(regs: Regs, l: List<Int>) : this(regs, l[0], l[1], l[2])
}

data class Regs(var a: Int, var b: Int, var c: Int, var d: Int) {
  constructor(l: List<Int>) : this(l[0], l[1], l[2], l[3])

  operator fun get(i: Int) = when(i) {
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    else -> throw IndexOutOfBoundsException(i)
  }

  operator fun set(i: Int, v: Int): Regs {
    when (i) {
      0 -> a = v
      1 -> b = v
      2 -> c = v
      3 -> d = v
      else -> throw IndexOutOfBoundsException(i)
    }
    return this
  }

  fun copy() = Regs(a, b, c, d)
}

class LearningComputer {
  private val opcodeMap = Array(16) { Opcode.values().toMutableList() }

  fun processSample(opcode: Int, op: Op, after: Regs) =
      opcodeMap[opcode].filterNot { after == it.run(op) }.forEach { opcodeMap[opcode].remove(it) }

  fun finalize(): Computer {
    while (opcodeMap.any { it.size != 1}) {
      val (foundCodes, others) = opcodeMap.partition { it.size == 1 }
      foundCodes.map { it.first() }.forEach {
        foundCode -> others.forEach { it.remove(foundCode)}
      }
    }
    return Computer(opcodeMap.map { it.first() }.toTypedArray())
  }
}

class Computer(private val opcodeMap: Array<Opcode>) {
  operator fun get(i: Int) = opcodeMap[i]
}

fun findInts(str: String) = """(\d+),? (\d+),? (\d+),? (\d+)""".toRegex()
    .find(str)!!.destructured.toList()
    .map { it.toInt() }

fun prodPart1() = runPart1(File("data/2018/16.txt").readLines())

fun runPart1(lines: List<String>) {
  var counter = 0
  for (sample in lines.chunked(4)) {
    val before = Regs(findInts(sample[0]))
    val (opcode, a, b, c) = findInts(sample[1])
    val after = Regs(findInts(sample[2]))
    val op = Op(before, a, b, c)
    val matches = Opcode.values().map { after == it.run(op) }.count { it }
    if (matches >= 3) {
      counter++
    }
  }
  println(counter)
}

fun prodPart2() =
    runPart2(
        parseComputer(File("data/2018/16.txt").readLines()),
        parseOps(File("data/2018/16program.txt").readLines()))

fun parseComputer(lines: List<String>) =
  lines.chunked(4).fold(LearningComputer()) { computer, sample ->
    val before = findInts(sample[0])
    val (opcode, a, b, c) = findInts(sample[1])
    val after = findInts(sample[2])
    computer.processSample(opcode, Op(Regs(before), a, b, c), Regs(after))
    computer
  }.finalize()

fun parseOps(lines: List<String>) =
    lines.map { Instruction(findInts(it)) }

fun runPart2(computer: Computer, instrs: List<Instruction>) {
  var regs = Regs(0, 0, 0, 0)
  for (instr in instrs) {
    regs = computer[instr.opcode].run(Op(regs, instr.inputs()))
  }
  println(regs[0])
}

fun main(args: Array<String>) {
  prodPart1()
  prodPart2()
}