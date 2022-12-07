import java.io.File

class Part2 {
  val cmdRegex = Regex("""\$ ([a-z]+)( (.*))?""")
  val dirRegex = Regex("""dir ([a-z]+)""")
  val docRegex = Regex("""([0-9]+) ([a-z.]+)""")

  val totalSpace = 70000000
  val requiredSpace = 30000000

  fun run(input: String): Int {
    val root = Dir("/")
    val allDirs = processCommandLine(input, root)

    val totalSize = root.size
    val freeSpace = totalSpace - totalSize
    val needed = requiredSpace - freeSpace
    println("$totalSize, $freeSpace, $needed")

    return allDirs
      .values
      .map { it.size }
      .filter { it >= needed }
      .min()
  }

  private fun processCommandLine(
    input: String,
    root: Dir
  ): HashMap<String, Dir> {
    var cwd = "/"
    var curDir: Dir = root
    val allDirs = hashMapOf<String, Dir>()
    allDirs[cwd] = curDir
    input.lines().forEach { line ->
      // $ cmd
      val cmdMatch = cmdRegex.find(line)
      if (cmdMatch != null) {
        val (cmd, rest1, rest2) = cmdMatch.groupValues.drop(1)
        if (cmd == "cd") {
          if (rest2 == "..") {
            cwd = cwd.substringBeforeLast("/")
          } else if (rest2.startsWith("/")) {
            cwd = rest2
          } else {
            cwd = "$cwd/$rest2"
          }
          curDir = allDirs[cwd]!!
        }

        // ignore ls for now
      }

      // dir <dirname>
      val dirMatch = dirRegex.find(line)
      if (dirMatch != null) {
        val (name) = dirMatch.groupValues.drop(1)
        val dir = Dir(name)
        curDir.dirs.add(dir)
        allDirs["$cwd/$name"] = dir
      }

      // 1234 <filename>
      val docMatch = docRegex.find(line)
      if (docMatch != null) {
        val (size, name) = docMatch.groupValues.drop(1)
        val doc = Doc(name, size.toInt())
        curDir.files.add(doc)
      }
    }
    return allDirs
  }

  data class Dir(
    val name: String,
    val dirs: MutableList<Dir> = mutableListOf(),
    val files: MutableList<Doc> = mutableListOf()
  ) {
    val size: Int by lazy { dirs.sumOf { it.size } + files.sumOf { it.size } }

    fun print(indent: Int = 0) {
      val pad = " ".repeat(indent)
      println("${" ".repeat(indent)}- $name")
    }
  }

  data class Doc(val name: String, val size: Int)
}

data class Dir(
    val name: String,
    val dirs: MutableList<Dir> = mutableListOf(),
    val files: MutableList<Doc> = mutableListOf()
) {
  val size: Int by lazy { dirs.sumOf { it.size } + files.sumOf { it.size } }
}

data class Doc(val name: String, val size: Int)

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
