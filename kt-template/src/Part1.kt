import java.io.File

class Part1 {
    fun run(input: List<String>): Int {
        return input.size
    }
}

fun main() {
    val file = File("input.txt")
    println(Part1().run(file.readLines()))
}
