import java.io.File
import java.util.*
import kotlin.math.max

class Part1 {
  fun run(input: String): Int {
    return parse(input).sumOf { it.id * calculateQualityLevel(24, it) }
  }
}

fun calculateQualityLevel(turns: Int, bp: Blueprint): Int {
//  calculateQualityLevelNaive(turns, bp)
  return calculateQualityLevelImproved(turns, bp)
}

fun calculateQualityLevelImproved(turns: Int, bp: Blueprint): Int {
  // brings sample Blueprint 1 down to 2_806_207
  // and input Blueprint 15 down to 3_003_361 (from 3_643_266_053)
  // for part 2 (32 turns) brings Blueprint 1 down from 14_189_913_168 to 21_084_029
  //     and also brings the runtime down to under a minute; with naive, it ran for over an hour
  //     and never even finished the second blueprint
  val startingStates = listOf(Turn2(turns, Units(), Units(ore = 1)))

  val q = LinkedList(startingStates)
  var maxGeodes = 0
  var loopCounter = 0L
  while (q.isNotEmpty()) {
    loopCounter++
    val turn = q.pop()
    val (turns, inv, prod) = turn
    if (turns == 0) {
      maxGeodes = max(maxGeodes, inv.geode)
      continue
    }
    val next = getNextImproved(turns, bp, inv, prod)
    next.forEach { bot ->
      val newProd = prod + Units.of(bot)
      q.push(Turn2(turns - 1, inv + prod - bp.get(bot), newProd))
    }
    if (next.size <= 1) {
      // TODO this heuristic could be a little smarter; we may want to spend 2 turns saving up for something
      // but that's clearly not the case if for everything we can currently produce at least one per turn, our
      // inventory has twice as many as the maximum needed for any recipe.
      // e.g. we produce ore and clay, need 4 ore and 14 clay max, but have 30 of each
      q.push(Turn2(turns - 1, inv + prod, prod))
    }
  }
  println("loops: $loopCounter")
  println("maxGeodes: $maxGeodes")
  println()
  return maxGeodes
}

fun getNextImproved(turns: Int, bp: Blueprint, inv: Units, prod: Units): List<Bot> {
  return Bot.values().reversed().filter { shouldBuild(it, turns, bp, inv, prod) }
}

fun shouldBuild(bot: Bot, turns: Int, bp: Blueprint, inv: Units, prod: Units): Boolean {
  // first check if we're over-producing
  // now do we have enough to construct?
  // also do we have too much?
  val max = bp.all.maxOf { it.get(bot) }
  if (bot == Bot.GEODE) {
    return bp.canAfford(bot, inv)
  }
  return prod.get(bot) < max
      && !tooMuch(turns, inv.get(bot), prod.get(bot), max)
      && bp.canAfford(bot, inv)
}

fun tooMuch(turns: Int, inv: Int, prod: Int, maxToBuild: Int): Boolean {
  val prodRemain = turns * prod
  // is our current inventory, plus remaining production more than enough to build
  // the most expensive once per turn?
  return inv + prodRemain >= maxToBuild * turns
}

fun calculateQualityLevelNaive(turns: Int, bp: Blueprint): Int {
  // naive approach with no optimizations.
  // Blueprint 15 from input has 3_643_266_053 loops!!
  // Blueprint1 from the sample on the website has 44_491_923 loops
  val startingStates =
      listOf(Bot.ORE, Bot.CLAY).map { bot -> Turn(turns, Units(), Units(ore = 1), bot) }

  val q = LinkedList(startingStates)
  var maxGeodes = 0
  var loopCounter = 0L
  while (q.isNotEmpty()) {
    loopCounter++
    val turn = q.pop()
    val (turns, inv, prod, next) = turn
    if (turns == 0) {
      maxGeodes = max(maxGeodes, inv.geode)
      continue
    }
    when {
      next == Bot.ORE && inv >= bp.ore -> {
        val newProd = prod + Units(ore = 1)
        getNext(bp, inv, newProd).forEach {
          q.push(Turn(turns - 1, inv + prod - bp.ore, newProd, it))
        }
      }
      next == Bot.CLAY && inv >= bp.clay -> {
        val newProd = prod + Units(clay = 1)
        getNext(bp, inv, newProd).forEach {
          q.push(Turn(turns - 1, inv + prod - bp.clay, newProd, it))
        }
      }
      next == Bot.OBS && inv >= bp.obs -> {
        val newProd = prod + Units(obs = 1)
        getNext(bp, inv, newProd).forEach {
          q.push(Turn(turns - 1, inv + prod - bp.obs, newProd, it))
        }
      }
      next == Bot.GEODE && inv >= bp.geode -> {
        val newProd = prod + Units(geode = 1)
        getNext(bp, inv, newProd).forEach {
          q.push(Turn(turns - 1, inv + prod - bp.geode, newProd, it))
        }
      }
      else -> q.push(Turn(turns - 1, inv + prod, prod, next))
    }
  }
  println("loops: $loopCounter")
  println("maxGeodes: $maxGeodes")
  println()
  return maxGeodes
}

fun getNext(bp: Blueprint, inv: Units, prod: Units): List<Bot> {
  if (prod.ore < 0) return listOf()
  return buildList {
    if (prod.obs > 0) add(Bot.GEODE)
    if (prod.clay > 0) add(Bot.OBS)
    add(Bot.CLAY)
    add(Bot.ORE)
  }
}

enum class Bot {
  ORE,
  CLAY,
  OBS,
  GEODE
}

data class Units(val ore: Int = 0, val clay: Int = 0, val obs: Int = 0, val geode: Int = 0) :
    Comparable<Units> {
  companion object {
    fun of(bot: Bot) =
        when (bot) {
          Bot.ORE -> Units(ore = 1)
          Bot.CLAY -> Units(clay = 1)
          Bot.OBS -> Units(obs = 1)
          Bot.GEODE -> Units(geode = 1)
        }
  }

  fun get(bot: Bot) =
      when (bot) {
        Bot.ORE -> ore
        Bot.CLAY -> clay
        Bot.OBS -> obs
        Bot.GEODE -> geode
      }

  operator fun plus(other: Units): Units {
    return Units(ore + other.ore, clay + other.clay, obs + other.obs, geode + other.geode)
  }

  operator fun minus(other: Units): Units {
    return Units(ore - other.ore, clay - other.clay, obs - other.obs, geode - other.geode)
  }

  @Deprecated("this really doesn't work")
  override fun compareTo(other: Units): Int {
    val l = listOf(this, other).map { listOf(it.ore, it.clay, it.obs, it.geode) }
    val c = l[0].zip(l[1]).map { (a, b) -> a.compareTo(b) }
    return when {
      c.any { it < 0 } -> -1
      c.any { it > 0 } -> 1
      else -> 0
    }
  }
}

data class Blueprint(
    val id: Int,
    val ore: Units,
    val clay: Units,
    val obs: Units,
    val geode: Units
) {
  val all = listOf(ore, clay, obs, geode)

  fun canAfford(bot: Bot, inv: Units): Boolean {
    return Bot.values().all { get(bot).get(it) <= inv.get(it) }
  }

  fun get(bot: Bot) =
      when (bot) {
        Bot.ORE -> ore
        Bot.CLAY -> clay
        Bot.OBS -> obs
        Bot.GEODE -> geode
      }
}

data class Turn(val turnsLeft: Int, val inventory: Units, val prod: Units, val nextPurchase: Bot)

data class Turn2(val turnsLeft: Int, val inventory: Units, val prod: Units)

operator fun <E> List<E>.component6() = this[5]

operator fun <E> List<E>.component7() = this[6]

fun parse(input: String) =
    input.lines().map { line ->
      val (id, oreCost, clayCost, obsCostOre, obsCostClay, geodeCostOre, geodeCostObs) =
          Regex("""\d+""").findAll(line).map { it.value.toInt() }.toList()
      Blueprint(
          id,
          Units(ore = oreCost),
          Units(ore = clayCost),
          Units(ore = obsCostOre, clay = obsCostClay),
          Units(ore = geodeCostOre, obs = geodeCostObs))
    }

fun main() {
  val file = File("input.txt")
  println(Part1().run(file.readText().trimIndent()))
}
