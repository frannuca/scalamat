package org.fjn.gui

import scala.swing._
import scala.swing.event.ButtonClicked
import swing.event.{TableRowsSelected, TableEvent, TableColumnsSelected, ButtonClicked,EditDone}
import scala.collection.mutable.ArrayBuffer

object MainWindow extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Celsius/Fahrenheit Converter"
    object celsius extends TextField { columns = 5 }
    object fahrenheit extends TextField { columns = 5 }
    contents = new FlowPanel {
      contents += new Label(" Celsius = ")
      contents += celsius
      contents += new Label(" Fahrenheit=")
      contents += fahrenheit

      border = Swing.EmptyBorder(15, 10, 10, 10)
    }
    listenTo(celsius, fahrenheit)
    reactions += {
      case EditDone(`fahrenheit`) =>
        val f = fahrenheit.text.toInt
        val c = (f - 32) * 5 / 9
      celsius.text = c.toString
      case EditDone(`celsius`) =>
      val c = celsius.text.toInt
      val f = c * 9 / 5 + 32
      fahrenheit.text = f.toString
    }
  }
}