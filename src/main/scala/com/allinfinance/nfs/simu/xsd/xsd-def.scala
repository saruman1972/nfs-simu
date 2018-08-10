package com.allinfinance.nfs.simu.xsd

import scala.util.matching.Regex

sealed trait XSAttributeUse
case object XSAttributeUseOptional extends XSAttributeUse
case object XSAttributeUseProhibited extends XSAttributeUse
case object XSAttributeUseRequired extends XSAttributeUse

case class XSAnnotation() {
    var appInfo: Option[String] = None
    var documentation: Option[String] = None
}
sealed trait XSNameOrRef[T]
case class XSName[T](val name: String) extends XSNameOrRef[T]
case class XSReference[T](val reference: String) extends XSNameOrRef[T] {var ref: Option[T] = None}
trait XSComponent {var annotation: Option[XSAnnotation] = None}
trait XSParticle {
    var maxOccurs: Int = 1
    var minOccurs: Int = 1
    def unbounded: Boolean = maxOccurs < 0
}
trait XSType {
    def baseType: XSType
    def derivedFrom(tp: XSType): Boolean =
        if (baseType == null) false
        else if (baseType == tp) true
        else baseType.derivedFrom(tp)
}
abstract class XSBuiltinType extends XSType {
    override def baseType: XSType = null
    def pattern: Regex
}
trait XSBuiltinStringTrait
trait XSBuiltinDecimalTrait
trait XSBuiltinDateTrait
object XSBuiltinString extends XSBuiltinType with XSBuiltinStringTrait {
    override def pattern: Regex = """^.*$""".r
}
object XSBuiltinToken extends XSBuiltinType with XSBuiltinStringTrait {
    override def pattern: Regex = """^.*$""".r
}
object XSBuiltinNormalizedString extends XSBuiltinType with XSBuiltinStringTrait {
    override def pattern: Regex = """^.*$""".r
}
object XSBuiltinDate extends XSBuiltinType with XSBuiltinDateTrait {
    override def pattern: Regex = """^\d\d\d\d-\d\d-\d\d$""".r
}
object XSBuiltinTime extends XSBuiltinType with XSBuiltinDateTrait {
    override def pattern: Regex = """^\d\d:\d\d:\d\d$""".r
}
object XSBuiltinDatetime extends XSBuiltinType with XSBuiltinDateTrait {
    override def pattern: Regex = """^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d$""".r
}
object XSBuiltinDecimal extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^(-)?\d*(\.\d+)?$""".r
}
object XSBuiltinByte extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^.*$""".r
}
object XSBuiltinInt extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^(-)?\d*$""".r
}
object XSBuiltinInteger extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^(-)?\d*$""".r
}
case object XSBuiltinLong extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^(-)?\d*$""".r
}
case object XSBuiltinNegativeInteger extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^-\d*$""".r
}
case object XSBuiltinNonNegativeInteger extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^\d*$""".r
}
case object XSBuiltinNonPositiveInteger extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^-\d*$""".r
}
case object XSBuiltinPositiveInteger extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^\d*$""".r
}
case object XSBuiltinShort extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^\d*$""".r
}
case object XSBuiltinUnsignedLong extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^\d*$""".r
}
case object XSBuiltinUnsignedInt extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^\d*$""".r
}
case object XSBuiltinUnsignedShort extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^\d*$""".r
}
case object XSBuiltinUnsignedByte extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^\d*$""".r
}
case object XSBuiltinBoolean extends XSBuiltinType with XSBuiltinDecimalTrait {
    override def pattern: Regex = """^true|false$""".r
}
sealed trait XSFacet {
    val value: String
    var fixed: Option[String] = None
    val desc: String
}
case class XSSchema() extends XSComponent {
    override def toString: String = "rootElement=" + rootElement + "\nelements=" + elementDecls + "\nsimpleTypes=" + simpleTypes + "\ncomplexTypes=" + complexTypes + "\ngroups=" + groupDecls + "\nattributes=" + attributeDecls + "\nattGroups=" + attGroupDecls
    var rootElement: Option[XSElementDecl] = None
    val rootElmts = collection.mutable.ArrayBuffer[XSElementDecl]()

    val attGroupDecls = new collection.mutable.ArrayBuffer[XSAttGroupDecl]()
    val attGroupDeclMap = new collection.mutable.HashMap[String, XSAttGroupDecl]()
    def addAttGroupDecl(attGroupDecl: XSAttGroupDecl): Unit = {
        attGroupDecls += attGroupDecl
        attGroupDecl.nameOrRef match {
            case Some(XSName(name)) => attGroupDeclMap += (name -> attGroupDecl)
            case _ =>
        }
    }
    val attributeDecls = new collection.mutable.ArrayBuffer[XSAttributeDecl]()
    val attributeDeclMap = new collection.mutable.HashMap[String, XSAttributeDecl]()
    def addAttributeDecl(att: XSAttributeDecl): Unit = {
        attributeDecls += att
        att.nameOrRef match {
            case Some(XSName(name)) => attributeDeclMap += (name -> att)
            case _ =>
        }
    }
    val complexTypes = new collection.mutable.ArrayBuffer[XSComplexType]()
    val complexTypeMap = new collection.mutable.HashMap[String, XSComplexType]()
    def addComplexType(xsType: XSComplexType): Unit = {
        complexTypes += xsType
        xsType.name match {
            case Some(name) =>
                complexTypeMap += (name -> xsType)
                types += (name -> xsType)
            case _ =>
        }
    }
    val elementDecls = new collection.mutable.ArrayBuffer[XSElementDecl]()
    val elementDeclMap = new collection.mutable.HashMap[String, XSElementDecl]()
    def addElementDecl(elmt: XSElementDecl): Unit = {
        elementDecls += elmt
        elmt.nameOrRef match {
            case Some(XSName(name)) => elementDeclMap += (name -> elmt)
            case _ =>
        }
    }
    val groupDecls = new collection.mutable.ArrayBuffer[XSGroupDecl]()
    val groupDeclMap = new collection.mutable.HashMap[String, XSGroupDecl]()
    def addGroupDecl(group: XSGroupDecl): Unit = {
        groupDecls += group
        group.nameOrRef match {
            case Some(XSName(name)) => groupDeclMap += (name -> group)
            case _ =>
        }
    }
    val simpleTypes = new collection.mutable.ArrayBuffer[XSSimpleType]()
    val simpleTypeMap = new collection.mutable.HashMap[String, XSSimpleType]()
    def addSimpleType(xsType: XSSimpleType): Unit = {
        simpleTypes += xsType
        xsType.name match {
            case Some(name) =>
                simpleTypeMap += (name -> xsType)
                types += (name -> xsType)
            case _ =>
        }
    }
    val types = collection.mutable.HashMap[String, XSType]("xs:string" -> XSBuiltinString,
                                                           "xs:token" -> XSBuiltinToken,
                                                           "xs:normalizedString" -> XSBuiltinNormalizedString,
                                                           "xs:date" -> XSBuiltinDate,
                                                           "xs:dateTime" -> XSBuiltinDatetime,
                                                           "xs:time" -> XSBuiltinTime,
                                                           "xs:decimal" -> XSBuiltinDecimal,
                                                           "xs:byte" -> XSBuiltinByte,
                                                           "xs:int" -> XSBuiltinInt,
                                                           "xs:integer" -> XSBuiltinInteger,
                                                           "xs:long" -> XSBuiltinLong,
                                                           "xs:negativeInteger" -> XSBuiltinNegativeInteger,
                                                           "xs:nonNegativeInteger" -> XSBuiltinNonNegativeInteger,
                                                           "xs:nonPositiveInteger" -> XSBuiltinNonPositiveInteger,
                                                           "xs:positiveInteger" -> XSBuiltinPositiveInteger,
                                                           "xs:short" -> XSBuiltinShort,
                                                           "xs:unsignedLong" -> XSBuiltinUnsignedLong,
                                                           "xs:unsignedInt" -> XSBuiltinUnsignedInt,
                                                           "xs:unsignedShort" -> XSBuiltinUnsignedShort,
                                                           "xs:unsignedByte" -> XSBuiltinUnsignedByte,
                                                           "xs:boolean" -> XSBuiltinBoolean
                                                       )
}
trait XSAttributeContainer {
    val atts = new collection.mutable.ArrayBuffer[XSAttributeContainer]()
}
case class XSAttGroupDecl(val nameOrRef: Option[XSNameOrRef[XSAttGroupDecl]]) extends XSComponent with XSAttributeContainer
case class XSAttributeDecl(val nameOrRef: Option[XSNameOrRef[XSAttributeDecl]]) extends XSComponent with XSAttributeContainer {
    var attType: Option[XSSimpleType] = None
    var typeRef: Option[String] = None
    var defaultValue: Option[String] = None
    var fixedValue: Option[String] = None
    var use: XSAttributeUse = XSAttributeUseOptional
}
case class XSElementDecl(val nameOrRef: Option[XSNameOrRef[XSElementDecl]]) extends XSComponent with XSParticle {
    override def toString: String = nameOrRef match {
        case Some(XSName(name)) => name
        case Some(r @ XSReference(_)) => r.ref.map{_.toString}.getOrElse("")
        case None => ""
    }
    var defaultValue: Option[String] = None
    var fixedValue: Option[String] = None
    var elmtType: Option[XSType] = None
    var typeRef: Option[String] = None
    var isAbstract: Boolean = false
    var nullable: Boolean = false
    var referenced: Boolean = false

    def getPattern: Regex = {
        elmtType match {
            case bt : XSBuiltinType => bt.pattern
            case st : XSSimpleType => st.pattern
            case _ => ".*".r
        }
    }
    def actualName: String = nameOrRef match {
        case Some(XSName(name)) => name
        case Some(r @ XSReference(_)) => r.ref.get.actualName
        case _ => ""
    }
    def actualFixedValue: Option[String] = nameOrRef match {
        case Some(XSName(_)) => fixedValue
        case Some(r @ XSReference(_)) => r.ref.get.actualFixedValue
        case _ => None
    }
}
case class XSSimpleType(val name: Option[String]) extends XSComponent with XSType {
    var restriction: XSSimpleRestriction = null
    override def baseType: XSType = restriction.baseType
    lazy val ancestor: XSBuiltinType = {
        baseType match {
            case bt: XSBuiltinType => bt
            case st: XSSimpleType => st.ancestor
            case ct: XSComplexType => throw new IllegalArgumentException("parent of simpleType must be either simpleType or builtinType")
        }
    }
    lazy val facetsMap: Map[String, List[XSFacet]] = {
        val fm = baseType match {
            case bt: XSBuiltinType => Map[String, List[XSFacet]]()
            case st: XSSimpleType => st.facetsMap
        }
        val enum = "enumeration"
        restriction.facets.foldLeft(restriction.facets.find(x => x.desc == enum) match {
                                                                                                case Some(_) => fm - enum  // this level enumeration should override base type's enumeration
                                                                                                case None => fm}
                                       ) {(m,f) =>
                                           f.desc match {
                                               case `enum` => m + (enum -> (m.getOrElse(f.desc, List[XSFacet]()) :+ f))
                                               case d => m + (d -> List(f))
                                           }
                                      }
    }

    lazy val maxLength: Int = facetsMap.get("maxLength") match {
        case None => facetsMap.get("length") match {
            case None => -1
            case Some(x) => x(0).value.toInt
        }
        case Some(x) => x(0).value.toInt
    }
    lazy val minLength = facetsMap.get("minLength") match {
        case None => facetsMap.get("length") match {
            case None => -1
            case Some(x) => x(0).value.toInt
        }
        case Some(x) => x(0).value.toInt
    }
    lazy val pattern: Regex = ancestor match {
        case _: XSBuiltinStringTrait => facetsMap.get("pattern") match {
            case Some(p) => new Regex(p(0).value)
            case None => ancestor.pattern
        }
        case XSBuiltinDecimal => facetsMap.get("fractionDigits") match {
            case Some(fd) =>
                if (fd(0).value.toInt == 0)
                    new Regex("""^(-)?\d*$""")
                else
                    new Regex("""^(-)?\d*(\.\d{0,""" + fd(0).value.toInt + """})?$""")
            case None => ancestor.pattern
        }
        case _ => ancestor.pattern
    }
}
case class XSComplexType(val name: Option[String]) extends XSComponent with XSType with XSAttributeContainer {
    var mixed: Boolean = false
    var particle: Option[XSParticle] = None
    override def baseType: XSType = particle match {
        case Some(XSSimpleContent(derivation)) => derivation.baseType
        case Some(XSComplexContent(derivation)) => derivation.baseType
        case _ => XSBuiltinString
    }
}
case class XSGroupDecl(val nameOrRef: Option[XSNameOrRef[XSGroupDecl]]) extends XSComponent with XSParticle {
    var group: Option[XSGroup] = None
}
sealed trait XSGroup extends XSComponent with XSParticle {
    val children = new collection.mutable.ArrayBuffer[XSParticle]()
}
case class XSGroupAll() extends XSGroup
case class XSGroupChoice() extends XSGroup
case class XSGroupSequence() extends XSGroup
case class XSAny() extends XSComponent with XSParticle
case class XSAnyAttribute() extends XSComponent
trait XSDerivation extends XSAttributeContainer {
    var baseName: String
    var baseType: XSType = null
}
case class XSSimpleContent(val derivation: XSDerivation) extends XSComponent with XSParticle
case class XSComplexContent(val derivation: XSDerivation) extends XSComponent with XSParticle
sealed trait XSRestriction extends XSComponent with XSDerivation {
    val facets = new collection.mutable.ArrayBuffer[XSFacet]
}
case class XSSimpleRestriction(override var baseName: String) extends XSRestriction {
}
case class XSComplexRestriction(override var baseName: String) extends XSRestriction {
    var particle: Option[XSParticle] = None
}
case class XSExtension(override var baseName: String) extends XSComponent with XSDerivation {
    var particle: Option[XSParticle] = None
}
case class XSFacetEnumeration(override val value: String) extends XSComponent with XSFacet {
    override val desc = "enumeration"
}
case class XSFacetFractionDigits(override val value: String) extends XSComponent with XSFacet {
    override val desc = "fractionDigits"
}
case class XSFacetLength(override val value: String) extends XSComponent with XSFacet {
    override val desc = "length"
}
case class XSFacetMaxExclusive(override val value: String) extends XSComponent with XSFacet {
    override val desc = "maxExclusive"
}
case class XSFacetMaxInclusive(override val value: String) extends XSComponent with XSFacet {
    override val desc = "maxInclusive"
}
case class XSFacetMaxLength(override val value: String) extends XSComponent with XSFacet {
    override val desc = "maxLength"
}
case class XSFacetMinExclusive(override val value: String) extends XSComponent with XSFacet {
    override val desc = "minExclusive"
}
case class XSFacetMinInclusive(override val value: String) extends XSComponent with XSFacet {
    override val desc = "minInclusive"
}
case class XSFacetMinLength(override val value: String) extends XSComponent with XSFacet {
    override val desc = "minLength"
}
case class XSFacetPattern(override val value: String) extends XSComponent with XSFacet {
    override val desc = "pattern"
}
case class XSFacetTotalDigits(override val value: String) extends XSComponent with XSFacet {
    override val desc = "totalDigits"
}
case class XSFacetWhiteSpace(override val value: String) extends XSComponent with XSFacet {
    override val desc = "whiteSpace"
}

