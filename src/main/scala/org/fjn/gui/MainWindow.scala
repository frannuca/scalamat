package org.fjn.gui

import scala.swing._
import scala.swing.event.ButtonClicked
import swing.event.{TableRowsSelected, TableEvent, TableColumnsSelected, ButtonClicked}
import scala.collection.mutable.ArrayBuffer

object MainWindow extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "First Swing App"
    val button = new Button{
      text = "click me"
    }
    val button2 = new Button{
      text = "Reset"
    }
    val label = new Label{
      text="click to compute"
    }
    val data = ArrayBuffer(
      Array("Name","Birthday","ID"),
      Array("Bob", "07/19/1986", "2354"),
      Array("Sue", "05/07/1980", "2355")
    )
    val table  = new Table(data.tail.toArray map (_.toArray[Any]),
      data.head) {

      }

    contents = new  BoxPanel(Orientation.Vertical){
      contents +=    button
      contents += label
      contents += button2
      contents += table
      contents += new ScrollPane{

        viewportView = table

        //      border = Swing.EmptyBorder(30,30,10,30)
      }
      border= Swing.EmptyBorder(30,30,10,30)
    }


    listenTo(button,button2)
    var nClicks = 0
    reactions += {
      case ButtonClicked(b) if b == button =>
      nClicks+=1
      label.text="number of clicks %s".format(nClicks)
      case ButtonClicked(b) if b == button2 =>
        nClicks = 0
        label.text=" resetted number of clicks %s".format(nClicks)
    }
  }
}