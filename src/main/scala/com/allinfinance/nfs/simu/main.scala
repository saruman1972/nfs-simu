package com.allinfinance.nfs.simu

import scala.swing._
import scala.swing.event._
import Swing._
import collection.JavaConversions._

import javax.swing.UIManager
import javax.swing.plaf.FontUIResource
import javax.swing.SwingUtilities

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver

import xsd._

import cn.webank.rmb.api.RMBInternalErrorCodes;
import cn.webank.rmb.api.RMB;
import cn.webank.rmb.api.Util;
import cn.webank.rmb.destination.Destination;
import cn.webank.rmb.message.AppHeader;
import cn.webank.rmb.message.Message;
import cn.webank.rmb.message.SysHeader;

// object Main {
//     def main(args: Array[String]): Unit = {
//         val xsd = XSParser.parse("NF11000Request.xsd")
//         Console.out.println("schema=" + xsd)
//     }
// }

case class NfsConfig(val ip: String, val port: Int, val pik: String, headLen: Int, val format: String, val charset: String)
case class PathValuePair(val path: String, val value: String)
case class CaseValue(val desc: String, val xsd: String, val values: java.util.List[PathValuePair])

object NfsSimu extends SimpleSwingApplication {
//UIManager.getDefaults.keys.foreach {key => 
//val value = UIManager.get(key)
//if (value.isInstanceOf[FontUIResource])
//Console.out.println("key=" + key + ", value=" + value)
//                                }
//UIManager.put("swing.boldMetal", false)
val font = UIManager.get("TextField.font")
UIManager.put("RadioButton.font", font)
UIManager.put("Menu.font", font)
UIManager.put("CheckBox.font", font)
UIManager.put("MenuItem.font", font)
UIManager.put("PopupMenu.font", font)
UIManager.put("ComboBox.font", font)
UIManager.put("Label.font", font)

    var config = {
        val s = scala.io.Source.fromFile("nfs-config.xml").mkString
        val xstream = new XStream(new DomDriver)
        xstream.alias("nfs-config", classOf[NfsConfig])
        xstream.fromXML(s).asInstanceOf[NfsConfig]
    }

//    val ipTextField = new MaskedTextField("###.###.###.###")
    val ipTextField = new TextField(config.ip)
    ipTextField.peer.setColumns(15)
//    val portTextField = new MaskedTextField("#####")
    val portTextField = new TextField(config.port.toString)
    portTextField.peer.setColumns(5)
//    val pikTextField = new MaskedTextField("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")
    val pikTextField = new TextField(config.pik)
    pikTextField.peer.setColumns(32)
    val hlTextField = new TextField(config.headLen.toString)
    hlTextField.peer.setColumns(1)
  val charsetTextField = new TextField(if (config.charset == null) "UTF-8" else config.charset)
  charsetTextField.peer.setColumns(20)

    var messageFormat = if (config.format == "json") "json" else "xml"
    val xmlBtn = new RadioButton("xml") {
        action = Action("xml") { messageFormat = "xml" }
    }
    val jsonBtn = new RadioButton("json") {
        action = Action("json") { messageFormat = "json" }
    }
    if (messageFormat == "json") {
        xmlBtn.selected = false;
        jsonBtn.selected = true;
    }
    else {
        xmlBtn.selected = true;
        jsonBtn.selected = false;
    }

    class ServiceInvoker(val textArea: TextArea, val sendBtn: Button) {
        import java.net.{Socket, InetSocketAddress}
        import java.io._

        val sock = new Socket()
        val buf = new Array[Byte](102400)
        var receiveThread: Thread = null

        def printStackTrace(e: Throwable): Unit = {
            val sw = new StringWriter
            e.printStackTrace(new PrintWriter(sw))
            textArea.text += sw
        }

        def send(message: String, headLen: Int): Boolean = {
            try {
                sendBtn.enabled = false
                textArea.text += "send message: =====>\n" + message + "\n"
println("send message: ======>\n" + message + "\n")

                sock.connect(new InetSocketAddress(config.ip, config.port))
                val out = sock.getOutputStream
                val bytes = message.getBytes(charsetTextField.text)
                out.write(("%0"+headLen+"d").format(bytes.size).getBytes)
                out.write(bytes)

                receiveThread = new Thread {
                    override def run: Unit = {
                        try {
//                           val msgHeader = Util.createSysHeader("111111111111111", "111111111111111", "06")
//                           val appHeader = new AppHeader
//                           val serviceId = "10031800"
//                           val dcn = "001"
//                           val scenario = "00"
//                           val destination = Util.createDestination(serviceId, dcn, scenario)
// scala.Console.out.println("message="+message)
//                           val rmbMsg = Util.createMessage(msgHeader, appHeader, destination, message)
//                           val request = Util.createRequest(rmbMsg, 30)
//                           val response = RMB.sendRequest(request)
//                           val ret = if (response.getErrCode == RMBInternalErrorCodes.RQUEST_TIMEOUT)
//                             "服务响应超时!!!!!!!!!!!!!!!!!!!!!!!!!!"
//                           else
//                             response.getMessage.getContent

                            val in = sock.getInputStream
                            in.read(buf, 0, headLen)
                            val len = (new String(buf, 0, headLen)).toInt
println("read length=" + (new String(buf, 0, headLen)) + ", len=" + len)
                            var offset = 0
                            var n = 0
                            do {
                                n = in.read(buf, offset, len-offset)
println("read [" + n + "] bytes")
                                if (n > 0) offset += n
                            } while((offset < len) && (n >= 0))
                            val ret = new String(buf, 0, len, charsetTextField.text)
println("receive message: =====>\n" + ret + "\n")
                            SwingUtilities.invokeLater(new Runnable {
                                override def run: Unit = {
                                    textArea.text += "receive message: =====>\n" + ret + "\n"
                                    sendBtn.enabled = true
                                }
                            })
                        }
                        catch {case e =>
                          sendBtn.enabled = true
                          printStackTrace(e)
                        }
                        finally {sock.close}
                    }
                }
                receiveThread.start
                true
            }
            catch {
                case e =>
                    sendBtn.enabled = true
                    sock.close
                    printStackTrace(e)
                    false
            }
        }

        def cancel: Unit = if (receiveThread != null) {
            receiveThread.stop
            sendBtn.enabled = true
            receiveThread = null
        }
    }

    def top = new MainFrame {
        title = "nfs simulator"
        var currentXsd: String = null
        val caseLabel = new Label("NonFinance Transaction:")
        val tree = new XsdTree
        var serviceInvoker: ServiceInvoker = null
        val bottom1 = new BoxPanel(Orientation.Horizontal) {
            contents += new Button(Action("open") {
                val chooser = new FileChooser {
                    peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")+"/xsd"))
                }
                chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                if (chooser.showOpenDialog(this) == FileChooser.Result.Approve) {
                    caseLabel.text = "NonFinance Transaction:" + chooser.selectedFile.getPath
                    tree.setFile(chooser.selectedFile.getPath)
                    currentXsd = chooser.selectedFile.getPath
                }
            })
            val sendBtn: Button = new Button(Action("send") {
scala.Console.out.println("ip=" + ipTextField.text)
scala.Console.out.println("port=" + portTextField.text)
scala.Console.out.println("pik=" + pikTextField.text)
scala.Console.out.println("headLen=" + hlTextField.text)
scala.Console.out.println("messageFormat=" + messageFormat)
                config = NfsConfig(ipTextField.text, portTextField.text.toInt, pikTextField.text, hlTextField.text.toInt, messageFormat, charsetTextField.text)

                val serviceNode = tree.xsdTree.model.roots(0)
                XsdNode.clearAutoGeneratedField(serviceNode)
                XsdNode.fillAutoGenerateField(serviceNode, config.pik)
                val xml =
                    if (messageFormat == "json")
                        XsdNode.rootToJson(serviceNode)
                    else {
                      val xmlheader = "<?xml version=\"1.0\" encoding=\"" +
                                      charsetTextField.text +
                                      "\"?>\n"
                      val xmlstr = XsdNode.rootToXML(serviceNode)
                      val header = XsdNode.findTag(serviceNode, "SERVICE_HEADER").get
                      if (XsdNode.useFixedType(header)) {
                        // note: (?s) - enable multiline matching
                        val Pttrn = "(?s)(.*?)<SERVICE_HEADER>[\n]?(.*?)</SERVICE_HEADER>(.*)".r
                        xmlstr match {
                          case Pttrn(prev,curr,post) => curr + xmlheader + prev + post
                          case _ => xmlheader + xmlstr
                        }
                      }
                      else
                        xmlheader + xmlstr
                    }

                val serviceInvoker = new ServiceInvoker(right, sendBtn)
                serviceInvoker.send(xml, hlTextField.text.toInt)
            })
            contents += sendBtn
            contents += new Button(Action("cancel") {
                if (serviceInvoker != null) serviceInvoker.cancel
            })
            contents += new Button(Action("clear") {
                right.text = ""
            })
            contents += new Label("    ")
            contents += new Button(Action("save as") {
                val chooser = new FileChooser {
                    peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")+"/cases"))
                }
                chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                if (chooser.showSaveDialog(this) == FileChooser.Result.Approve) {
                    val xstream = new XStream(new DomDriver)
                    xstream.alias("nfs-case", classOf[CaseValue])
                    xstream.alias("entry", classOf[PathValuePair])
                    val serviceNode = tree.xsdTree.model.roots(0)
                    val pvs = new java.util.ArrayList[PathValuePair]()
                    XsdNode.serializeInputValue(serviceNode).foreach {x => pvs.add(PathValuePair(x._1, x._2))}
                    val cv = CaseValue(chooser.selectedFile.getPath, currentXsd, pvs)
                    val s = xstream.toXML(cv)
                    Some(new java.io.PrintWriter(chooser.selectedFile.getPath)).foreach{p =>
                        p.write(s)
                        p.close
                    }
                }
            })
            contents += new Button(Action("load") {
                val chooser = new FileChooser {
                    peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")+"/cases"))
                }
                chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                if (chooser.showOpenDialog(this) == FileChooser.Result.Approve) {
                    val s = scala.io.Source.fromFile(chooser.selectedFile.getPath).mkString

                    val xstream = new XStream(new DomDriver)
                    xstream.alias("nfs-case", classOf[CaseValue])
                    xstream.alias("entry", classOf[PathValuePair])
                    val cv = xstream.fromXML(s).asInstanceOf[CaseValue]
                    tree.setFile(cv.xsd)
                    val serviceNode = tree.xsdTree.model.roots(0)
//                    XsdNode.fillLoadedValues(serviceNode, cv.values.map {x => (x.path, x.value)} .toList)
                    cv.values.foreach {x =>
                        val node = XsdNode(new XSElementDecl(None))
                        node.value = x.value
                        XsdNode.stringPathToTreePath(serviceNode, x.path).map {path => tree.xsdTree.model.update(path, node)}
                    }
                }
            })
        }
      val bottom2 = new BoxPanel(Orientation.Horizontal) {
            contents += new Label("    ")
            contents += new FlowPanel {
                contents += new Label("ip:")
                contents += ipTextField
                contents += new Label("port:")
                contents += portTextField
                contents += new Label("pik:")
                contents += pikTextField
                contents += new Label("head len:")
                contents += hlTextField
                val mutex = new ButtonGroup(xmlBtn, jsonBtn)
                contents ++= mutex.buttons
                contents += new Label("charset:")
                contents += charsetTextField
            }
      }
        val right = new TextArea()

        tree.preferredSize = new java.awt.Dimension(600, 600)
//        right.minimumSize = new java.awt.Dimension(400, 600)
        contents = new BoxPanel(Orientation.Vertical) {
            contents += caseLabel
            contents += new BoxPanel(Orientation.Horizontal) {
                val scrollTextArea = new ScrollPane(right)
//                scrollTextArea.horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
//                scrollTextArea.verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
                contents += new SplitPane(Orientation.Vertical, tree, scrollTextArea) {
                    continuousLayout = true
                }
            }
            contents += bottom1
          contents += bottom2
        }
        minimumSize = new java.awt.Dimension(1100, 600)

    }
}
