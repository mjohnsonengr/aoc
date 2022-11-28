import java.io.File

class Part2 {
    fun run(lines: List<String>): Int {
        return lines.sumOf(::ribbon)
    }

    fun ribbon(d: String): Int {
        val vals = requireNotNull(Regex("""(\d+)x(\d+)x(\d+)""").find(d)?.groupValues?.drop(1)?.map { it.toInt() })
        require(vals.size == 3)
        val s = vals.sorted()
        val wrap = 2*s[0]+2*s[1]
        val bow = s[0]*s[1]*s[2]
        return wrap+bow
    }
}

fun main() {
    val file = File("input.txt")
    println(Part2().run(file.readLines()))
}
