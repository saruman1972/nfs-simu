package com.allinfinance.nfs.simu

import scala.swing._
import scala.swing.event._
import Swing._

import javax.swing.JTree
import javax.swing.ToolTipManager
import javax.swing.JComponent
import javax.swing.JComboBox
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.JTextField
import javax.{swing => js}
import js.{tree => jst}
import js.{event => jse}
import javax.swing.UIManager

import scalaswingcontrib.tree._
import scalaswingcontrib.event._

import xsd._

class XsdTree extends ScrollPane {

    def setFile(name: String):Unit = {
        val root = XsdNode.buildSchemaTree(XSParser.parse(name))
        xsdTree.model = new ExternalTreeModel(List(root), ((n: XsdNode) => n.children)).makeUpdatableWith {
            (path, node) => 
                path.last.value = node.value
                path.last
        } makeInsertableWith {
            (parentPath, node, index) =>
                parentPath.last.insert(index, node)
                true
        } makeRemovableWith {
            (pathToRemove) =>
                val node = pathToRemove.last
//                node.parent.remove(node)
                true
        }
        xsdTree.expandAll
    }

    val xsdTree = new Tree[XsdNode] {thisTree =>

        listenTo(keys)
        reactions += {
            case KeyPressed(_, Key.Enter, _, _) =>
                val sel = selection.paths.leadSelection
                sel.map{path =>
                    if (path.last.children.size == 0) // leaf
                        startEditingAtPath(path)
                }
        }

        lineStyle = Tree.LineStyle.Angled

        val actualRenderer = new Tree.DefaultRenderer[XsdNode] {
            override def componentFor(tree: Tree[_], value: XsdNode, info: companion.CellInfo): Component = {
                peer.defaultRendererComponent(tree.peer, value.asInstanceOf[AnyRef], info.isSelected, info.isExpanded, info.isLeaf, info.row, info.hasFocus)
//                backgroundNonSelectionColor = if (value.dirty) java.awt.Color.RED else java.awt.Color.WHITE
                for (annotation <- value.elmt.annotation;
                     doc <- annotation.documentation) {
                    tooltip = doc
                }
                this
            }
        }
        renderer = new Tree.AbstractRenderer[XsdNode, Label](new Label("hello")) {
            val nodeLabel = new Label("") { background = java.awt.Color.WHITE }
            val renderPanel = new BoxPanel(Orientation.Horizontal) {
                contents += new Button("+")
                contents += nodeLabel
            }

            override def configure(tree: Tree[_], value: XsdNode, info: companion.CellInfo) = {}
            override def componentFor(tree: Tree[_], value: XsdNode, info: companion.CellInfo): Component = {
                actualRenderer.componentFor(tree, value, info)
                if (value.elmt.unbounded) {
                    nodeLabel.text = value.toString
                    renderPanel
                }
                else
                    actualRenderer
            }
        }
        ToolTipManager.sharedInstance().registerComponent(peer)

        editable = true
        editor = new Tree.Editor[XsdNode] {thisEditor =>
            private[this] lazy val lazyPeer: jst.TreeCellEditor = new TreeEditorPeer {
                override def isCellEditable(e: java.util.EventObject) = {
                    if (e == null) true  // for key enter start editing
                    else {
                        val tree = getTreeWrapper(e.getSource.asInstanceOf[JTree])
//                            val sn = tree.selection.selectedNode
                        e match {
                            case me: java.awt.event.MouseEvent =>
                                val path = treePathToPath(tree.peer.getPathForLocation(me.getX, me.getY))
                                (tree.model.peer.isLeaf(path.last) || path.last.elmt.unbounded)
                            case _ => false
                        }
                    }
                }
            }
            override def peer = lazyPeer // We can't use a lazy val directly, as Wrapped wouldn't be able to override with a non-lazy val

            val editingIcon = UIManager.getIcon("Tree.leafIcon")
            object cellEditor extends Component {
                override lazy val peer: JComponent = new JLabel with SuperMixin {
                    val offset = 4 + editingIcon.getIconWidth
                    setLayout(null)

                    def calculateIconY: Int = {
                        val iconHeight = editingIcon.getIconHeight
                        val textHeight = lastComponent.peer.getFontMetrics(lastComponent.peer.getFont).getHeight
                        val textY = iconHeight/2 - textHeight/2
                        val totalY = math.min(0, textY)
                        val totalHeight = math.max(iconHeight, textY + textHeight) - totalY
                        getHeight/2 - (totalY + (totalHeight/2))
                    }
                    override def paint(g: java.awt.Graphics) {
                        val yLoc = calculateIconY
                        editingIcon.paintIcon(this, g, 0, yLoc)
                        super.paint(g)
                    }

                    override def getPreferredSize: Dimension = if (lastComponent == null) new Dimension(0, 0) else {
                        var sz = lastComponent.preferredSize
                        sz.width += offset + 5 + label.preferredSize.width + 2
                        sz.height = math.max(sz.height, editingIcon.getIconHeight)
                        sz.width = math.max(sz.width, 100)
                        sz
                    }

                    override def doLayout: Unit = {
                        label.peer.setBounds(offset, 0, label.preferredSize.width, getHeight)
                        lastComponent.peer.setBounds(offset+label.preferredSize.width+2, 0, getWidth - offset - label.preferredSize.width - 2, getHeight)
                    }
                }

                opaque = false
                border = null

                val label = new Label
                label.opaque = false
//              contents += label
                peer.add(label.peer)

                var lastComponent: Component = null
                reactions += {
                    case MaskEditDone(_) => thisEditor.peer.stopCellEditing
                    case EditDone(_) => thisEditor.peer.stopCellEditing                   // from textfield
                    case SelectionChanged(_) => thisEditor.peer.stopCellEditing           // from combobox
                    case ButtonClicked(_) => thisEditor.peer.stopCellEditing              // from checkbox
                }

                def setComponent(c: Component) {
                    if (lastComponent != null) {
                        deafTo(lastComponent)
//                        contents -= lastComponent
                        peer.remove(lastComponent.peer) // remove all child component
                    }
                    c match {
                        case maskField: MaskedTextField => listenTo(maskField)
                        case textField: TextField => listenTo(textField)
                        case comboBox: ComboBox[_] => listenTo(comboBox.selection)
                        case checkBox: CheckBox => listenTo(checkBox)
                        case _ => listenTo(c)
                    }
//                    contents += c
                    peer.add(c.peer)
                    lastComponent = c
                }
            }
            var currentNode: XsdNode = null
            var currentEditor: Component = null

            val multiOccurLabel = new Label("") { background = java.awt.Color.WHITE }
            val multiOccurEditor = new BoxPanel(Orientation.Horizontal) {
                contents += new Button(Action("+") {
                    val selectionPath = thisTree.treePathToPath(thisTree.peer.getSelectionPath)
                    scala.Console.out.println("=====================> button clicked 1================>" + selectionPath)
                    thisEditor.peer.stopCellEditing
                    val node = XsdNode.cloneNode(currentNode)
                    thisTree.model.insertAfter(selectionPath, node)
                })
                contents += multiOccurLabel
            }

            override def componentFor(tree: Tree[_], a: XsdNode, info: companion.CellInfo): Component = {
                currentNode = a
                currentEditor = 
                    if (a.elmt.unbounded) {
                        multiOccurLabel.text = a.toString
                        multiOccurEditor
                    }
                    else {
                        cellEditor.setComponent(a.editor)
                        cellEditor.label.text = a.elmt + ":"
                        a.setEditValue(a.value)
                        cellEditor
                    }
                currentEditor
            }
            override def value =
                if (currentEditor == multiOccurEditor)
                    currentNode
                else {
                    val node = XsdNode(new XSElementDecl(None))
                    node.value = currentNode.getEditValue
                    node
                }
            override def cellEditable = true

//            override def fireCellEditingCancelled() { Console.out.println("editing cancelled") }
//            override def fireCellEditingStopped() { Console.out.println("editing stopped") }

        }
        showsRootHandles = true
    }

    viewportView = xsdTree
}
