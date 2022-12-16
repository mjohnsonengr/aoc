import java.io.File
import kotlin.math.max

class Part1 {
  fun run(input: String): Int {
    val time = 26 // TODO: Change to 30 for actual Part 1
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

    // first get shortest distance from each node to each other using table
    val distances = shortestDistances(valves)

    // for N iterations (N is # valves w/ rate > 0), calculate weights
    // iteration 0: calculate weights from AA to each

    val initWeights = calcWeights(Weight(0, "AA", time, listOf()), valves, distances)
    // iteration 1: for each of N valves, calculate weights to each N other valves except visited
    // O(N^3) but small N (15)
    val finishedWeights = initWeights.toMutableList()
    var weights = initWeights
    while (weights.isNotEmpty()) {
        weights = weights.flatMap { weight ->
          calcWeights(weight, valves, distances)
      }
      finishedWeights += weights
    }

    // part 2 play -- TODO: Move this to Part2 file or parameterize Part1 to make tests happy
    var maxWeight = 0
    val numToOpen = valves.values.filter { it.rate > 0 }.size
    finishedWeights.forEach { it1 ->
      finishedWeights
        .filter { it2 -> it2.opened.size + it1.opened.size <= numToOpen}
        .forEach { it2 ->
        if (it1.opened.intersect(it2.opened.toSet()).isEmpty()) {
          maxWeight = max(maxWeight, it1.weight + it2.weight)
        }
      }
    }
    println("part 2: $maxWeight")

    return finishedWeights.maxOf { it.weight }
  }

  data class Weight(val weight: Int, val valve: String, val timeLeft: Int, val opened: List<String>)

  fun calcWeights(from: Weight, valves: Map<String, Valve>, distances: Distances): List<Weight> {
      return valves.filter { it.value.rate > 0 }
        .mapValues { it.value.rate to from.timeLeft - distances[from.valve]!![it.key]!! - 1 }
        .filter { it.value.second > 0 && !from.opened.contains(it.key) }
        .map { Weight(from.weight + it.value.first * it.value.second, it.key, it.value.second, from.opened + it.key) }
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

typealias Distances = Map<String, MutableMap<String, Int>>

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
