import java.io.File

class Part2 {
    fun run(input: List<String>): Int {
        return input.size
    }
}

fun main() {
    val file = File("input.txt")
    println(Part2().run(file.readLines()))
}
