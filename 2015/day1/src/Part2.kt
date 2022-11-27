import java.io.File

class Part2 {
    fun run(input: List<String>): Int {
        var counter = 0
        var index = 1
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
                if (counter == -1) {
                    return index
                }
                index++
            }
        }
        throw IllegalStateException("Unreachable")
    }
}

fun main() {
    val file = File("input.txt")
    println(Part2().run(file.readLines()))
}
