
object addline {

  def main (args: Array[String])  = {
    val lines = io.Source.fromFile ("S_.scala").getLines.toList.zipWithIndex
    for (line <- lines) {println ((line._2 + 1) + " " + line._1)}
  }
}

    
