import java.io.File

class Part1 {
    fun run(lines: List<String>): Int {
        return lines.sumOf(::wrap)
    }

    fun wrap(d: String): Int {
        val vals = requireNotNull(Regex("""(\d+)x(\d+)x(\d+)""").find(d)?.groupValues?.drop(1)?.map { it.toInt() })
        require(vals.size == 3)
        val area = 2*vals[0]*(vals[1] + vals[2]) + 2*vals[1]*vals[2]
        val sorted = vals.sorted()
        val excess = sorted[0]*sorted[1]
        return area + excess
    }
}

fun main() {
    val file = File("input.txt")
    println(Part1().run(file.readLines()))
}
