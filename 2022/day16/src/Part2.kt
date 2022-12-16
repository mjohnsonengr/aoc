import java.io.File
import kotlin.math.max

class Part2 {
  fun run(input: String): Int {
    val valves = input.lines().associate { line ->
      val (name, rate, to) = Regex("""Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.*)""")
        .find(line)!!
        .groupValues
        .drop(1)
      name to Valve(
        name,
        rate.toInt(),
        to.split(", ")
      )
    }

    val numToOpen = valves.values.filter { it.rate > 0 }.size

    // first get shortest distance from each node to each other using table
    val distances = shortestDistances(valves)

    // for part 2 we need to figure out a way to codify prev solution into a list of steps
    // weight will have a nextTarget, and distance to target; each cycle decrements that distance

    // init step is to pick targets
    val initWeights = calcWeights(
      Weight(0, "AA", 26, listOf()), valves, distances)

    // each cycle will decrement distance for each weight;
    // anything needing a new target goes to calcWeights
    // each time I open one, elephant's list of weights doubles in size (one where I opened, one where I didn't)

    // Weight.timeLeft tells us when it expires
    // Weight.weight tells us destination

    val finishedWeights = mutableSetOf<Weight>()
    var maxWeight = 0
    var weights = initWeights
    (26 downTo 0).forEach { timeLeft ->
      weights = weights.flatMap { wt ->
        if (wt.timeLeft == timeLeft)
          calcWeights(wt, valves, distances)
        else
          listOf(wt)
      }

      weights.forEach {it1 ->
        weights.forEach {it2 ->
          if (it1.opened.intersect(it2.opened.toSet()).isEmpty()) {
            maxWeight = max(maxWeight, it1.weight + it2.weight)
          }
        }
      }
    }

    return maxWeight
  }

  data class Weight(
    val weight: Int,
    val valve: String,
    val timeLeft: Int,
    val opened: List<String>)

  fun calcWeights(from: Weight, valves: Map<String, Valve>, distances: Distances): List<Weight> {
    return valves.filter { it.value.rate > 0 }
      .mapValues { it.value.rate to from.timeLeft - distances[from.valve]!![it.key]!! - 1 }
      .filter { it.value.second > 0 && !from.opened.contains(it.key) }
      .map { Weight(
        from.weight + it.value.first * it.value.second,
        it.key,
        it.value.second,
        from.opened + it.key)
      }
  }

  fun shortestDistances(valves: Map<String, Valve>): Distances {
    val paths: Distances =
      valves.map { (key1, _) ->
        key1 to valves.map { (key2, _) ->
          if (valves.get(key1)!!.to.contains(key2))
            key2 to 1
          else if (key1 == key2)
            key2 to 0
          else
            key2 to Int.MAX_VALUE / 2
        }.toMap().toMutableMap()
      }.toMap()

    paths.keys.forEach { k ->
      paths.keys.forEach { i ->
        paths.keys.forEach { j ->
          if (paths[i]!![j]!! > paths[i]!![k]!! + paths[k]!![j]!!) {
            paths[i]!![j] = paths[i]!![k]!! + paths[k]!![j]!!
          }
        }
      }
    }
    return paths
  }

  data class Valve(val name: String, val rate: Int, val to: List<String>)
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
