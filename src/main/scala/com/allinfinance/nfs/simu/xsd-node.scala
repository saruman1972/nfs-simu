package com.allinfinance.nfs.simu

import scala.swing._
import scala.swing.event._
import scala.util.matching.Regex

import javax.swing.JFormattedTextField
import javax.swing.text.MaskFormatter
import java.awt.event.FocusAdapter

import javax.crypto.Cipher
import javax.crypto.SecretKey
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.DESedeKeySpec

import org.apache.commons.codec.binary.Hex

import xsd._

import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.xml.{Node, Text, Elem, XML}

case class MaskEditDone(override val source: MaskedTextField) extends ValueChanged(source)
class MaskedTextField(mask: String) extends FormattedTextField(null) {
    override lazy val peer: JFormattedTextField = new JFormattedTextField(new MaskFormatter(mask)) with SuperMixin
import java.awt.Font
val ft = new Font( "Monospaced", Font.PLAIN, 12 )
peer.setFont(ft)
// peer.setColumns(19)

    private lazy val actionListener = Swing.ActionListener { e =>
        publish(MaskEditDone(MaskedTextField.this))
                                                        }

    protected override def onFirstSubscribe() {
        super.onFirstSubscribe
        peer.addActionListener(actionListener)
        peer.addFocusListener(new FocusAdapter {
            override def focusLost(e: java.awt.event.FocusEvent) { publish(MaskEditDone(MaskedTextField.this)) }
        })
    }

    protected override def onLastUnsubscribe() {
        super.onLastUnsubscribe
        peer.removeActionListener(actionListener)
    }
}

class PatternedTextField(val pattern: Regex, maxLength: Int=10) extends TextField {
    private var effPattern = pattern
    val doc = new javax.swing.text.PlainDocument {
        override def insertString(index: Int, s: String, a: javax.swing.text.AttributeSet) {
            val newString = getText(0, index) + s + getText(index, getLength-index)
            if (effPattern != null) {
                for (_ <- effPattern.findFirstIn(newString)) {
                    if (newString.length <= maxLength)
                        super.insertString(index, s, a)
                }
            }
            else if (newString.length <= maxLength)
                super.insertString(index, s, a)
        }
        override def remove(index: Int, len: Int) {
            val newString = getText(0, index) + getText(index+len, getLength-index-len)
            if (effPattern != null)
                for (_ <- effPattern.findFirstIn(newString)) {
                    super.remove(index, len)
                }
            else
                super.remove(index, len)
        }
    }

import java.awt.Font
val ft = new Font( "Monospaced", Font.PLAIN, 12 )
peer.setFont(ft)
    peer.setDocument(doc)
    peer.setColumns(maxLength)

    override def text_=(s: String): Unit = {
        effPattern = null
        doc.remove(0, doc.getLength)
        super.text = s
        effPattern = pattern
    }
}

class XsdNode (val elmt: XSElementDecl) {
    var value: String = ""
    var transformedValue: String = ""
    override def toString = elmt.toString + ":" + value
    val children = new collection.mutable.ArrayBuffer[XsdNode]()
    var parent: Option[XsdNode] = None
    def add(node: XsdNode): Unit = {
        children += node
        node.parent = Some(this)
    }
    def insert(n: Int, node: XsdNode): Unit = {
        children.insert(n, node)
        node.parent = Some(this)
    }

    def setEditValue(value: String): Unit = {
        editor match {
            case comboBox: ComboBox[String] => comboBox.selection.item = value
            case t: PatternedTextField => elmt.elmtType match {
                case Some(tp) =>
                    if (tp.derivedFrom(XSBuiltinDate)) {
                        if (value == "") t.text = "--" else t.text = value
                    }
                    else t.text = value
                case _ => t.text = value
            }
            case m: MaskedTextField => m.text = value
        }
    }
    def getEditValue: String = {
        editor match {
            case comboBox: ComboBox[String] => try {comboBox.selection.item} catch { case _ => "" }
            case t: PatternedTextField => elmt.elmtType match {
                case Some(tp) =>
                    if (tp.derivedFrom(XSBuiltinDate)) {
                        if (value == "--") "" else t.text
                    }
                    else t.text
                case _ => t.text
            }
            case m: MaskedTextField => m.text
        }
    }
    lazy val editor: Component = {
        elmt.elmtType match {
            case Some(tp) => tp match {
                case XSBuiltinDate => new MaskedTextField("####-##-##")
                case XSBuiltinTime => new MaskedTextField("##:##:##")
                case XSBuiltinDatetime => new MaskedTextField("####-##-##T##:##:##")
                case bt: XSBuiltinType => new PatternedTextField(bt.pattern)
                case st: XSSimpleType =>
                    st.facetsMap.get("enumeration") match {
                        case Some(enums) => new ComboBox(enums.map{f => f.asInstanceOf[XSFacetEnumeration].value})
                        case None => st.baseType match {
                            case XSBuiltinDate => new MaskedTextField("####-##-##")
                            case XSBuiltinTime => new MaskedTextField("##:##:##")
                            case XSBuiltinDatetime => new MaskedTextField("####-##-##T##:##:##")
                            case _ =>
                                new PatternedTextField(st.pattern, if (st.maxLength < 0) 999 else st.maxLength)
                        }
                    }
                case _ => new TextField
            }
            case _ => new TextField
        }
    }
    def hasInputValue: Boolean = ((value != "") || (transformedValue != "") || (elmt.actualFixedValue != None) || (children.filter(x => x.hasInputValue).size > 0))
    def actualValue: String = elmt.actualFixedValue match {
        case Some(v) => v
        case None =>
// Console.out.println("name=" + elmt + ",value=" + value + ",transformedValue=" + transformedValue)
            if (transformedValue != "")
                transformedValue
            else
                value
    }
}

object XsdNode {
    def apply(value: XSElementDecl) = new XsdNode(value)
    def buildSchemaTree(schema: XSSchema): XsdNode = {
        val root = XsdNode(new XSElementDecl(None))
        schema.rootElement map {elmt => buildElement(elmt, root)}
        root.children(0)
    }
    def buildElement(elmt: XSElementDecl, parent: XsdNode): XsdNode = {
        def buildElementHelper(elmt: XSElementDecl, node: XsdNode): XsdNode = {
            elmt.nameOrRef match {
                case Some(r @ XSReference(_)) => buildElementHelper(r.ref.get, node)
                case _ =>
                    elmt.elmtType match {
                        case Some(t) => t match {
                            case ct : XSComplexType => ct.particle match {
                                case Some(g : XSGroupDecl) => buildGroupDecl(g, node)
                                case Some(a : XSGroup) => buildGroup(a, node)
                                case _ =>
                            }
                            case _ => // simple type, no need to create node
                        }
                        case None => throw new IllegalArgumentException("element[" + elmt + "] has no type defined")
                    }
            }
            node
        }
        val node = XsdNode(elmt)
        parent.add(node)
        buildElementHelper(elmt, node)
    }
    def buildGroupDecl(group: XSGroupDecl, parent: XsdNode): Unit = {
        group.nameOrRef match {
            case Some(r @ XSReference(_)) => buildGroupDecl(r.ref.get, parent)
            case _ => buildGroup(group.group.get, parent)
        }
    }
    def buildGroup(group: XSGroup, parent: XsdNode): Unit = {
        group.children.foreach {particle =>
            particle match {
                case elmt: XSElementDecl => buildElement(elmt, parent)
                case g: XSGroupDecl => buildGroupDecl(g, parent)
                case g: XSGroup => buildGroup(g, parent)
                case any: XSAny => 
            }
        }
    }
    def cloneNode(from: XsdNode): XsdNode = {
        def cloneNodeHelper(from: XsdNode, parent: Option[XsdNode]): XsdNode = {
            val node = new XsdNode(from.elmt)
            parent.map {_.add(node)}
            from.children.foreach {cloneNodeHelper(_, Some(node))}
            node
        }
        cloneNodeHelper(from, None)
    }

    implicit object NodeFormat extends JsonFormat[Node] {
      def write(node: Node) =
        if (node.child.count(_.isInstanceOf[Elem]) >= 1)
            JsObject(node.child.collect {
              case e: Elem if (!((e.label == "EXT_ATTRIBUTES") && (e.child.count(_.isInstanceOf[Elem]) == 0))) =>
                  e.label -> write(e)
            }: _*)
        else {
              JsString(node.text)
        }

      def read(jsValue: JsValue) = null // not implemented
    }

    def rootToJson(root: XsdNode): String = {
      val xmlString = rootToXML(root)
scala.Console.out.println(xmlString)
        val xml = scala.xml.parsing.XhtmlParser(scala.io.Source.fromString(xmlString))
// scala.Console.out.println("xml=" + xml)
        "{\"SERVICE\":\n" + xml.toSeq.apply(0).asInstanceOf[Node].toJson.toString + "\n}"
    }

  var inRoot = false
    def rootToXML(root: XsdNode): String = {
        inRoot = true
//        val xmlheader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      val xml = toXML(root, false, 0)
      xml
    }

  def useFixedType(node: XsdNode): Boolean = node.elmt.elmtType match {
      case Some(st: XSComponent) => st.annotation match {
        case Some(annt) => annt.documentation match {
          case Some(doc) =>
            if (doc == "FIXED_TYPE") true else false
          case _ => false
        }
        case _ => false}
      case _ => false
  }

  def toXML(node: XsdNode, fixed: Boolean, tab: Int=4): String = {
    val useFixed = (useFixedType(node) || fixed)

        def childToXML(fixed: Boolean, tab: Int): String =
            if (inRoot) {
                inRoot = false
            " " * tab + "<" + node.elmt.actualName + " xmlns=\"http://www.allinfinance.com/dataspec/\">\n" + node.children.foldLeft(""){(accum,x) => accum + toXML(x, fixed, tab+4)} +  (" " * tab) + "</" + node.elmt.actualName + ">\n"
            }
            else
            " " * tab + "<" + node.elmt.actualName + ">\n" + node.children.foldLeft(""){(accum,x) => accum + toXML(x, fixed, tab+4)} +  (" " * tab) + "</" + node.elmt.actualName + ">\n"

    if (node.children.size > 0) { // complexType
      val xml = if (node.hasInputValue) {
              if (useFixed)
                childToXML(useFixed, 0)
              else
                childToXML(useFixed, tab)
      }
      else if (node.elmt.minOccurs > 0) {
                // TODO: should repeat minOccurs times
              if (useFixed)
                childToXML(useFixed, 0)
              else
                childToXML(useFixed, tab)
      }
      else ""
          xml
        }
        else {
if (useFixed) {
val fmt = node.elmt.elmtType match {
  case Some(st : XSSimpleType) => "%-" + st.maxLength + "s"
  case _ => "%1s"}
            if (node.hasInputValue)
                fmt.format(node.actualValue)
            else fmt.format("")
}
else {
            if (node.hasInputValue)
                " " * tab + "<" + node.elmt.actualName + ">" + node.actualValue + "</" + node.elmt.actualName + ">\n"
            else if (node.elmt.minOccurs > 0)
                " " * tab + "<" + node.elmt.actualName + "/>\n"
            else ""
}
        }
    }

    def findTag(node: XsdNode, tag: String): Option[XsdNode] = {
        node.children.find {x => x.elmt.actualName == tag}
    }
    var dateFormater = new java.text.SimpleDateFormat("yyyyMMdd")
    var datetimeFormater = new java.text.SimpleDateFormat("yyyyMMddHHmmss")
    def encryptPIN(key: String, pin: String, cardno: Option[String]): String = {
        val effKey = key.size match {
            case 16 => key + key + key
            case 32 => key + key.substring(0, 16)
            case 48 => key
            case _ => throw new IllegalArgumentException("invalid key length")
        }
        val keySpec = new DESedeKeySpec(Hex.decodeHex(effKey.toArray))
        val secretKey = SecretKeyFactory.getInstance("DESede").generateSecret(keySpec)
        val pinpadded = Hex.decodeHex(("%02d%s".format(pin.size, pin) + "F"*(16-2-pin.size)).toArray)
Console.out.println("pinpadded=" + Hex.encodeHex(pinpadded).mkString)
        val acctno = Hex.decodeHex((cardno match {
            case Some(cn) if (cn.size > 13) => "0000" + cn.substring(cn.size-1-12, cn.size-1)
            case _ => "0"*16
        }).toArray)
Console.out.println("acctno=" + Hex.encodeHex(acctno).mkString)
        val pinblock = pinpadded.zip(acctno).map {case (a,b) => (a ^ b).toByte}.toArray
Console.out.println("pinblock=" + Hex.encodeHex(pinblock).mkString)
        val cipher = Cipher.getInstance("DESede")
        cipher.init(Cipher.ENCRYPT_MODE, secretKey)
        Hex.encodeHex(cipher.doFinal(pinblock)).mkString.substring(0, 16).toUpperCase
    }
    def clearAutoGeneratedField(node: XsdNode): Unit = {
        node.transformedValue = ""
        for (n <- node.children)
            clearAutoGeneratedField(n)
    }
    def fillAutoGenerateField(root: XsdNode, pik: String): Unit = {
        val now = java.util.Calendar.getInstance().getTime()
        val header = findTag(root, "SERVICE_HEADER").get
//        findTag(header, "SERVICE_SN").map {x => x.transformedValue = "%019d".format(now.getTime)}
        findTag(header, "SERVICE_SN").map {x =>
          val fmt = x.elmt.elmtType match {
            case Some(st : XSSimpleType) => "%0" + st.maxLength + "d"
            case _ => "%019d"}
          x.transformedValue = fmt.format(now.getTime)}
        findTag(header, "REQUST_TIME").map {x => x.transformedValue = datetimeFormater.format(now)}
      // for new message head
      findTag(header, "REQUST_DATE").map {x => x.transformedValue = dateFormater.format(now)}

        val body = findTag(root, "SERVICE_BODY").get
        val extAtt = findTag(body, "EXT_ATTRIBUTES").get
        val request = findTag(body, "REQUEST").get
        val cardno = findTag(request, "CARD_NO").map(x => x.value)

        for (auth <- findTag(extAtt, "AUTH");
             qpin <- findTag(auth, "Q_PIN");
             if (qpin.value != "")) {
                 qpin.transformedValue = encryptPIN(pik, qpin.value, cardno)
             }
        for (auth <- findTag(extAtt, "AUTH");
             ppin <- findTag(auth, "P_PIN");
             if (ppin.value != "")) {
                 ppin.transformedValue = encryptPIN(pik, ppin.value, cardno)
             }
        for (pin <- findTag(request, "PIN");
             if (pin.value != "")) {
                 pin.transformedValue = encryptPIN(pik, pin.value, cardno)
             }
        for (pin <- findTag(request, "NEWPIN");
             if (pin.value != "")) {
                 pin.transformedValue = encryptPIN(pik, pin.value, cardno)
             }
        for (qpin <- findTag(request, "Q_PIN");
             if (qpin.value != "")) {
                 qpin.transformedValue = encryptPIN(pik, qpin.value, cardno)
             }
        for (ppin <- findTag(request, "P_PIN");
             if (ppin.value != "")) {
                 ppin.transformedValue = encryptPIN(pik, ppin.value, cardno)
             }
    }

    def serializeInputValue(root: XsdNode): List[(String, String)] = {
        def serializeHelper(path: String, node: XsdNode): List[Option[(String, String)]] = {
            val currPath =
                if (path == "")
                    node.elmt.toString
                else
                    path + "." + node.elmt.toString
            if (node.children.size > 0)
                node.children.toList.flatMap {n => serializeHelper(currPath, n)}
            else if (node.value != "")
                List(Some((currPath, node.value)))
            else
                List(None)
        }

        for (option <- serializeHelper("", root); x <- option) yield x
    }

    def stringPathToTreePath(root: XsdNode, path: String): Option[scala.collection.immutable.IndexedSeq[XsdNode]] = {
            path.split("""\.""").foldLeft(Some((List[XsdNode](), List(root))): Option[(List[XsdNode], List[XsdNode])]) {(ps, p) =>
                ps match {
                    case Some((pt, nodes)) => nodes.find {x => x.elmt.toString == p} map {x => (pt :+ x, x.children.toList)}
                    case None => None
                }
            } map {p => p._1.toIndexedSeq}
    }
    def fillLoadedValues(root: XsdNode, values: List[(String, String)]): Unit = {
        values.foreach {case (path, value) =>
            path.split("""\.""").foldLeft(Some(null, List(root)): Option[(XsdNode, List[XsdNode])]) {(ln, p) =>
                ln match {
                    case Some((_, nodes)) => nodes.find {x => x.elmt.toString == p} map {x => (x, x.children.toList)}
                    case None => None
                }
            } map {case (n, _) => Console.out.println(n + "=" + value); n.value = value}
        }
    }
}

