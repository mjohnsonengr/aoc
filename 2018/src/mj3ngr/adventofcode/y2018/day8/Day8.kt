package mj3ngr.adventofcode.y2018.day8

import java.io.File
import java.util.*

data class Node(val childNodes: List<Node>, val metadata: List<Int>)

fun main(args: Array<String>) {
  run("Test", "data/2018/8test.txt")
  run("Prod", "data/2018/8.txt")
}

fun run(env: String, filename: String) {
  println(env)
  val tree = parseLine(File(filename).readLines()[0])
  println(sumMetadata(tree))
  println(calcNode(tree))
  println()
}

fun parseLine(line: String): Node {
  return parseNode(Scanner(line))
}

fun parseNode(scanner: Scanner): Node {
  val numChildren = scanner.nextInt()
  val numMetadata = scanner.nextInt()

  val childNodes: ArrayList<Node> = ArrayList()
  for (i in 0 until numChildren) {
    childNodes.add(parseNode(scanner))
  }

  val metadata: ArrayList<Int> = ArrayList()
  for (i in 0 until numMetadata) {
    metadata.add(scanner.nextInt())
  }

  return Node(childNodes, metadata)
}

fun sumMetadata(node: Node): Int {
  return node.metadata.sum() + node.childNodes.map { sumMetadata(it) }.sum()
}

fun calcNode(node: Node): Int {
  if (node.childNodes.isEmpty()) {
    return node.metadata.sum()
  } else {
    return node.metadata.filter { it != 0 && it <= node.childNodes.size }.map { calcNode(node.childNodes[it-1]) }.sum()
  }
}