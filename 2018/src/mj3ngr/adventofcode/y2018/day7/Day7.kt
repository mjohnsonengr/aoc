package mj3ngr.adventofcode.y2018.day7

import java.io.File

fun main(args: Array<String>) {
  println("Test:")
  run(File("data/2018/7test.txt").readLines(), workers = 2, timeOffset = -60)
  println()
  println("Prod:")
  run(File("data/2018/7.txt").readLines(), workers = 5)
  println()
}

data class Step(
  val id: Char
) {
  val prereqs: ArrayList<Step> = ArrayList()
  val nextSteps: ArrayList<Step> = ArrayList()
  val time = id.toInt() - 4  // 'A'.toInt() = 65, but 'A' time is 61 seconds. 'B' = 62, etc.

  fun finish() {
    this.nextSteps.forEach { it.prereqs.remove(this) }
    print(this.id)
  }
}

fun HashMap<Char, Step>.lazyGet(key: Char): Step {
  if (this[key] == null) {
    this[key] = Step(key)
  }
  return this[key]!!
}

fun run(lines: List<String>, workers: Int, timeOffset: Int = 0) {
  runPart1(parseInput(lines))
  println()
  runPart2(parseInput(lines), workers, timeOffset)
}

private fun parseInput(lines: List<String>): MutableSet<Step> {
  val regex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".toRegex()
  val hashMap: HashMap<Char, Step> = HashMap()
  for (line in lines) {
    val (prereq, step) = regex.find(line)!!.destructured.toList().map { hashMap.lazyGet(it[0]) }
    addAssoc(prereq, step)
  }
  return hashMap.values.toMutableSet()
}

fun runPart1(steps: MutableSet<Step>) {
  while (!steps.isEmpty()) {
    val nextStep = steps.filter { it.prereqs.isEmpty() }.sortedBy { it.id }.first()
    nextStep.finish()
    steps.remove(nextStep)
  }
}

fun addAssoc(prereq: Step, step: Step) {
  prereq.nextSteps.add(step)
  step.prereqs.add(prereq)
}

data class StepWork(val step: Step, var timeLeft: Int) {
  fun work() {
    timeLeft--
  }

  val done: Boolean
    get() = timeLeft == 0
}

fun runPart2(steps: MutableSet<Step>, workers: Int, timeOffset: Int) {
  val work: ArrayList<StepWork> = ArrayList()
  var secondsPassed = -1
  while (!steps.isEmpty()) {
    secondsPassed++
    // Process existing steps.
    work.forEach { it.work() }
    val done = work.filter { it.done }
    done.forEach { it.step.finish() }
    work.removeAll(done)
    steps.removeAll(done.map { it.step })

    // Pick out new work.
    val currentSteps = work.map { it.step }
    val nextSteps = steps.filter { it.prereqs.isEmpty() && !currentSteps.contains(it) }.sortedBy { it.id }
    val freeWorkers = workers - work.size
    val newWork = nextSteps.take(freeWorkers)
    work.addAll(newWork.map { StepWork(it, it.time + timeOffset) })
  }

  println()
  println(secondsPassed)
}