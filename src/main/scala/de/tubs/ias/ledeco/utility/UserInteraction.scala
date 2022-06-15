package de.tubs.ias.ledeco.utility

object UserInteraction {

  def yesNo(message: String): Boolean = {
    var result: Option[Boolean] = None
    while (result.isEmpty) {
      print(s"$message [N/y]:")
      result = scala.io.StdIn.readLine() match {
        case "y" | "yes" => Some(true)
        case "n" | "no"  => Some(false)
        case _ =>
          println("you have to say (y)es or (n)o")
          None
      }
    }
    result.get
  }

}
