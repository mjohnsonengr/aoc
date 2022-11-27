import java.io.File

class Part1 {
    fun run(input: List<String>): Int {
        var counter = 0
        input.forEach {
            line ->
            line.forEach {
                c ->
                if (c == '(') {
                    counter++
                }
                if (c == ')') {
                    counter--
                }
            }
        }
        return counter
    }
}

fun main() {
    val file = File("input.txt")
    println(Part1().run(file.readLines()))
}
