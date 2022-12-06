import java.io.File
import java.security.MessageDigest
import kotlin.text.Charsets.UTF_8

class Part2 {
  fun run(input: String): Int {
    println(input)
    for (i in 0..10000000) {
      val hash = md5("$input$i").toHex()
      if (hash.take(6) == "000000") {
        return i
      }
    }
    return -1
  }

  fun md5(str: String): ByteArray = MessageDigest.getInstance("MD5").digest(str.toByteArray(UTF_8))
  fun ByteArray.toHex() = joinToString(separator = "") { byte -> "%02x".format(byte) }
}

fun main() {
  val file = File("input.txt")
  println(Part2().run(file.readText().trimIndent()))
}
