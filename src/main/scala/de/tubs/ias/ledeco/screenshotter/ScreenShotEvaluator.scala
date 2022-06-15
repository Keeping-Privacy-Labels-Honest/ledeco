package de.tubs.ias.ledeco.screenshotter

import com.typesafe.config.Config
import de.halcony.argparse.{Parser, ParsingResult}

import java.awt.event.{KeyEvent, KeyListener}
import java.io.{File, FileWriter}
import javax.swing.ImageIcon
import scala.swing._

class ScreenShotEvaluator(screenshots: Seq[String], out: String)
    extends MainFrame {
  preferredSize = new Dimension(750, 1334)
  val fileWriter = new FileWriter(new File(out))
  title = "Screen Shot Evaluator"
  this.visible = true
  var labelCounter = 0
  contents = new Label {
    icon = new ImageIcon(screenshots(labelCounter))
  }

  def getCurrentName: String = {
    screenshots(labelCounter)
      .split("/")
      .last
      .split("\\.")
      .reverse
      .tail
      .reverse
      .mkString(".")
  }

  def writeDialogue(): Unit = {
    fileWriter.write(s"${getCurrentName},Dialogue\n")
  }

  def writeNoDialogue(): Unit = {
    fileWriter.write(s"${getCurrentName},No Dialogue\n")
  }

  def writeCrash(): Unit = {
    fileWriter.write(s"${getCurrentName},Crash\n")
  }

  this.peer.addKeyListener(
    new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = {}

      override def keyPressed(e: KeyEvent): Unit = {
        val actions = Map[String, Unit => Unit](
          "a" -> (_ => writeDialogue()),
          "s" -> (_ => writeNoDialogue()),
          "d" -> (_ => writeCrash())
        )
        actions.get(e.getKeyChar.toString) match {
          case Some(value) =>
            value.apply()
            labelCounter += 1
            if (labelCounter < screenshots.size) {
              fileWriter.flush()
              contents = new Label {
                icon = new ImageIcon(screenshots(labelCounter))
              }
            }
          case None =>
        }
      }

      override def keyReleased(e: KeyEvent): Unit = {}
    }
  )

  reactions += {
    case _: event.WindowClosing =>
      fileWriter.flush()
      fileWriter.close()
      println("closed and flushed")
  }
}

object ScreenShotEvaluator {

  val parser: Parser = Parser("scoter", "the screenshot evaluator")
    .addPositional("folder", "the folder containing the screenshots")
    .addPositional("out", "the file to write the results to")
    .addDefault[(ParsingResult, Config) => Unit]("func", main)

  private def main(pargs: ParsingResult,
                   @annotation.unused config: Config): Unit = {
    val files = new File(pargs.get[String]("folder"))
      .listFiles()
      .filter(_.getAbsolutePath.endsWith(".jpeg"))
      .map(_.getAbsolutePath)
    val outFileWriter = pargs.get[String]("out")
    new ScreenShotEvaluator(files, outFileWriter)
  }

}
