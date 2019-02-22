package com.allinfinance.nfs.simu.xsd

import scala.xml._

object XSParser {
    var baseDir: String = ""
    def parse(fileName: String): XSSchema = {
        val schema = new XSSchema
        baseDir = (new java.io.File(fileName)).getParent
        parseSchema(XML.loadFile(fileName), schema)
        schema
    }

    def parseSchema(node: Node, schema: XSSchema): XSSchema = {
        (node \ "include").foreach {x => parseInclude(x, schema)}
//          case i @ <xs:`import`>{_*}</xs:`import`> =>

        (node \ "attribute").foreach {x => parseAttribute(x, schema)}
        (node \ "attributeGroup").foreach {x => parseAttributeGroup(x, schema)}
        (node \ "simpleType").foreach {x => parseSimpleType(x, schema)}
        (node \ "complexType").foreach {x => parseComplexType(x, schema)}
        (node \ "group").foreach {x => parseGroupDecl(x, schema)}
        (node \ "element").foreach {x =>
            val elmt = parseElementDecl(x, schema)
            schema.rootElmts += elmt
        }

        // resolve reference
        resolveSymbolRef(schema)
        // find root element
        val rootElmts = schema.rootElmts.filter(elmt => !elmt.referenced)
        rootElmts.size match {
            case 0 =>
            case 1 => schema.rootElement = Some(rootElmts(0))
            case _ => throw new IllegalArgumentException("schema should have only one element as root")
        }

        schema
    }

    def resolveSymbolRef(schema: XSSchema): Unit = {
        schema.attributeDecls.foreach {a => resolveAttributeRef(a, schema)}
        schema.attGroupDecls.foreach {a => resolveAttGroupRef(a, schema)}
        schema.simpleTypes.foreach {t => resolveSimpleTypeRef(t, schema)}
        schema.complexTypes.foreach {t => resolveComplexTypeRef(t, schema)}
        schema.groupDecls.foreach {g => resolveGroupDeclRef(g, schema)}
        schema.elementDecls.foreach {elmt => resolveElementRef(elmt, schema)}
    }
    def resolveElementRef(elmt: XSElementDecl, schema: XSSchema): Unit = {
        elmt.nameOrRef match {
            case Some(r @ XSReference(reference)) => r.ref match {
                case None =>
                    r.ref = schema.elementDeclMap.get(reference)
                    r.ref.map(elmt => elmt.referenced = true)
                case _ =>
            }
            case _ =>
                elmt.elmtType match {
                    case Some(bt : XSBuiltinType) => // do nothing
                    case Some(st : XSSimpleType) => resolveSimpleTypeRef(st, schema)
                    case Some(ct : XSComplexType) => resolveComplexTypeRef(ct, schema)
                    case None => elmt.typeRef match {
                        case Some(t) => elmt.elmtType = schema.types.get(t)
                        case None =>
                    }
                }
        }
    }
    def resolveSimpleTypeRef(simpleType: XSSimpleType, schema: XSSchema): Unit = {
        resolveDerivationRef(simpleType.restriction, schema)
    }
    def resolveComplexTypeRef(complexType: XSComplexType, schema: XSSchema): Unit = {
        complexType.particle match {
            case Some(XSSimpleContent(derivation)) => resolveDerivationRef(derivation, schema)
            case Some(XSComplexContent(derivation)) => resolveDerivationRef(derivation, schema)
            case Some(g : XSGroupDecl) => resolveGroupDeclRef(g, schema)
            case Some(g : XSGroup) => resolveGroupRef(g, schema)
            case _ =>
        }
        resolveAttributeContainerRef(complexType, schema)
    }
    def resolveGroupDeclRef(group: XSGroupDecl, schema: XSSchema): Unit = {
        group.nameOrRef match {
            case Some(r @ XSReference(reference)) => r.ref match {
                case None => r.ref = schema.groupDeclMap.get(reference)
                case _ =>
            }
            case _ =>
                group.group match {
                    case Some(g) => resolveGroupRef(g, schema)
                    case _ =>
                }
        }
    }
    def resolveGroupRef(group: XSGroup, schema: XSSchema): Unit = {
        group.children.foreach (x => x match {
            case e : XSElementDecl => resolveElementRef(e, schema)
            case g : XSGroupDecl => resolveGroupDeclRef(g, schema)
            case g : XSGroup => resolveGroupRef(g, schema)
            case _ =>
        })
    }
    def resolveAttributeRef(att: XSAttributeDecl, schema: XSSchema): Unit = {
        att.nameOrRef match {
            case Some(r @ XSReference(reference)) => r.ref match {
                case None => r.ref = schema.attributeDeclMap.get(reference)
                case _ =>
            }
            case _ => att.typeRef match {
                case Some(t) =>
                    schema.types.get(t) match {
                        case Some(tp: XSSimpleType) => att.attType = Some(tp)
                        case _ =>
                    }
                case None =>
            }
        }
    }
    def resolveAttGroupRef(attGroup: XSAttGroupDecl, schema: XSSchema): Unit = {
        attGroup.nameOrRef match {
            case Some(r @ XSReference(reference)) => r.ref match {
                case None => r.ref = schema.attGroupDeclMap.get(reference)
                case _ =>
            }
            case _ =>
        }
        resolveAttributeContainerRef(attGroup, schema)
    }
    def resolveAttributeContainerRef(attContainer: XSAttributeContainer, schema: XSSchema): Unit = {
        attContainer.atts.foreach (x => x match {
            case a @ XSAttributeDecl(_) => resolveAttributeRef(a, schema)
            case g @ XSAttGroupDecl(nameOrRef) => resolveAttGroupRef(g, schema)
        })
    }
    def resolveDerivationRef(derivation: XSDerivation, schema: XSSchema): Unit = {
        derivation match {
            case r : XSSimpleRestriction => resolveSimpleRestrictionRef(r, schema)
            case r : XSComplexRestriction => resolveComplexRestrictionRef(r, schema)
            case e : XSExtension => resolveExtensionRef(e, schema)
        }
    }
    def resolveSimpleRestrictionRef(restriction: XSSimpleRestriction, schema: XSSchema): Unit = {
        restriction.baseType match {
            case null => schema.types.get(restriction.baseName).map {x => restriction.baseType = x}
            case _ =>
        }
        resolveAttributeContainerRef(restriction, schema)
    }
    def resolveComplexRestrictionRef(restriction: XSComplexRestriction, schema: XSSchema): Unit = {
        restriction.baseType match {
            case null => schema.types.get(restriction.baseName).map {x => restriction.baseType = x}
            case _ =>
        }
        restriction.particle match {
            case g : XSGroupDecl => resolveGroupDeclRef(g, schema)
            case g : XSGroup => resolveGroupRef(g, schema)
            case _ =>
        }
        resolveAttributeContainerRef(restriction, schema)
    }
    def resolveExtensionRef(extension: XSExtension, schema: XSSchema): Unit = {
        extension.baseType match {
            case null => schema.types.get(extension.baseName).map {x => extension.baseType = x}
            case _ =>
        }
        extension.particle match {
            case g : XSGroupDecl => resolveGroupDeclRef(g, schema)
            case g : XSGroup => resolveGroupRef(g, schema)
            case _ =>
        }
        resolveAttributeContainerRef(extension, schema)
    }

    def parseInclude(node: Node, schema: XSSchema): Unit = {
        val name = (node \ "@schemaLocation").text
//        val doc = XML.loadFile((node \ "@schemaLocation").text)
        val doc = XML.loadFile(baseDir + "/" + name)
        parseSchema(doc, schema)
    }
    def parsePossibleAnnotation(nodeSeq: NodeSeq): (NodeSeq, Option[XSAnnotation]) = if (nodeSeq.size == 0) (nodeSeq, None) else {
        nodeSeq(0) match {
            case a @ <xs:annotation>{_*}</xs:annotation> =>
                (nodeSeq.tail, Some(parseAnnotation(a)))
            case _ => (nodeSeq, None)
        }
    }
    def parseAnnotation(node: Node): XSAnnotation = {
        val annotation = new XSAnnotation()
        (node \ "_").foreach (x => x match {
            case <xs:documentation>{s @ _*}</xs:documentation> => annotation.documentation = Some(s.text)
            case <xs:appInfo>{s @ _*}</xs:appInfo> => annotation.appInfo = Some(s.text)
        })
        annotation
    }
    def parseNameOrRef[T](node: Node): Option[XSNameOrRef[T]] = {
        (node \ "@ref").text match {
            case "" => (node \ "@name").text match {
                case "" => None
                case n => Some(XSName[T](n))
            }
            case ref => Some(XSReference[T](ref))
        }
    }

    def parseAny(node: Node): XSAny = {
        val (_, annotation) = parsePossibleAnnotation(node \ "_")
        val any = new XSAny
        any.annotation = annotation
        parseParticle(node, any)
        any
    }
    def parseAttribute(node: Node, schema: XSSchema): XSAttributeDecl = {
        val att = XSAttributeDecl(parseNameOrRef(node))
        schema.addAttributeDecl(att)
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        att.annotation = annotation

        (node \ "@type").text match {
            case "" =>
            case t =>
                att.typeRef = Some(t)
                schema.types.get(t) match {
                    case Some(tp: XSSimpleType) => att.attType = Some(tp)
                    case _ =>
                }
        }
        if (subs.size > 1)
            throw new IllegalArgumentException("group declaration should contain only one simpleType:" + subs)
        subs(0) match {
            case s @ <xs:simpleType>{_*}</xs:simpleType> => att.attType = Some(parseSimpleType(s, schema))
            case e => throw new IllegalArgumentException("invalid element in extension declaration:" + e)
        }
        (node \ "@default").text match {
            case "" =>
            case d => att.defaultValue = Some(d)
        }
        (node \ "@fixed").text match {
            case "" =>
            case f => att.fixedValue = Some(f)
        }
        (node \ "@use").text match {
            case "" =>
            case "optional" => att.use = XSAttributeUseOptional
            case "prohibited" => att.use = XSAttributeUseProhibited
            case "required" => att.use = XSAttributeUseRequired
            case e => throw new IllegalArgumentException("invalid value for attribute[use]:" + e)
        }

        att
    }
    def parseAttributeGroup(node: Node, schema: XSSchema): XSAttGroupDecl = {
        val attGroup = XSAttGroupDecl(parseNameOrRef(node))
        schema.addAttGroupDecl(attGroup)
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        attGroup.annotation = annotation

        subs.foreach (x => x match {
            case a @ <xs:attribute>{_*}</xs:attribute> => attGroup.atts += parseAttribute(a, schema)
            case a @ <xs:attributeGroup>{_*}</xs:attributeGroup> => attGroup.atts += parseAttributeGroup(a, schema)
            case e => throw new IllegalArgumentException("invalid element in extension declaration:" + e)
        })

        attGroup
    }
    def parseParticle(node: Node, particle: XSParticle): Unit = {
        particle.maxOccurs = (node \ "@maxOccurs").text match {
            case "" => 1
            case "unbounded" => -1
            case att => att.toInt
        }
        particle.minOccurs = (node \ "@minOccurs").text match {
            case "" => 1
            case att => att.toInt
        }
    }
    def parseGroupDecl(node: Node, schema: XSSchema): XSGroupDecl = {
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        if (subs.size > 1)
            throw new IllegalArgumentException("group declaration should contain only one of (all|choice|sequence):" + subs)
        val groupDecl = XSGroupDecl(parseNameOrRef(node))
        schema.addGroupDecl(groupDecl)
        groupDecl.group =
            if (subs.size == 0)
                None
            else subs(0) match {
                case g @ <xs:all>{_*}</xs:all> => Some(parseGroup(g, new XSGroupAll, schema))
                case g @ <xs:choice>{_*}</xs:choice> => Some(parseGroup(g, new XSGroupChoice, schema))
                case g @ <xs:sequence>{_*}</xs:sequence> => Some(parseGroup(g, new XSGroupSequence, schema))
                case e => throw new IllegalArgumentException("invalid element in group declaration:" + e)
            }
        parseParticle(node, groupDecl)
        groupDecl
    }

    def parseGroup(node: Node, group: XSGroup, schema: XSSchema): XSGroup = {
        parseParticle(node, group)
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        subs.foreach (x =>
            x match {
                case e @ <xs:element>{_*}</xs:element> => group.children += parseElementDecl(e, schema)
                case g @ <xs:group>{_*}</xs:group> => group.children += parseGroupDecl(g, schema)
                case c @ <xs:choice>{_*}</xs:choice> => group.children += parseGroup(c, new XSGroupChoice, schema)
                case s @ <xs:sequence>{_*}</xs:sequence> => group.children += parseGroup(s, new XSGroupSequence, schema)
                case a @ <xs:any>{_*}</xs:any> => group.children += parseAny(a)
                case e => throw new IllegalArgumentException("invalid element in group declaration:" + e)
            }
        )
        group
    }

    def parseElementDecl(node: Node, schema: XSSchema): XSElementDecl = {
        val elmt = XSElementDecl(parseNameOrRef(node))
        schema.addElementDecl(elmt)
        parseParticle(node, elmt)
        (node \ "@type").text match {
            case "" =>
            case t =>
                elmt.typeRef = Some(t)
                elmt.elmtType = schema.types.get(t)
        }
        (node \ "@default").text match {
            case "" =>
            case d => elmt.defaultValue = Some(d)
        }
        (node \ "@fixed").text match {
            case "" =>
            case f => elmt.fixedValue = Some(f)
        }
        (node \ "@nillable").text match {
            case "" =>
            case "true" => elmt.nullable = true
            case "false" => elmt.nullable = false
            case _ => throw new IllegalArgumentException("unrecognized value for attribute[nillable]")
        }
        var (subs, annotation) = parsePossibleAnnotation(node \ "_")
        elmt.annotation = annotation
        if (subs.size > 0) {
            subs(0) match {
                case s @ <xs:simpleType>{_*}</xs:simpleType> => elmt.elmtType = Some(parseSimpleType(s, schema))
                case c @ <xs:complexType>{_*}</xs:complexType> => elmt.elmtType = Some(parseComplexType(c, schema))
                case _ =>
            }
            subs = subs.tail
        }
        // unique|key|keyref not implemented yet

        elmt
    }

    def parseSimpleType(node: Node, schema: XSSchema): XSSimpleType = {
        val st = XSSimpleType((node \ "@name").text match {
            case "" => None
            case n => Some(n)
        })
        schema.addSimpleType(st)
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        st.annotation = annotation
        if (subs.size != 1)
            throw new IllegalArgumentException("simpleType declaration should contain only one of (restriction|list|union):" + subs)
        subs(0) match {
            case r @ <xs:restriction>{_*}</xs:restriction> => st.restriction = parseSimpleRestriction(r, schema)
            case l @ <xs:list>{_*}</xs:list> => throw new IllegalArgumentException("list not implemented yet")
            case u @ <xs:union>{_*}</xs:union> => throw new IllegalArgumentException("union not implemented yet")
            case e => throw new IllegalArgumentException("invalid element in simpleType declaration:" + e)
        }
        st
    }

    def parseComplexType(node: Node, schema: XSSchema): XSComplexType = {
        val ct = XSComplexType((node \ "@name").text match {
            case "" => None
            case n => Some(n)
        })
        schema.addComplexType(ct)
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        ct.annotation = annotation
        (node \ "@mixed").text match {
            case "" =>
            case "true" => ct.mixed = true
            case "false" => ct.mixed = false
            case _ => throw new IllegalArgumentException("invlaid value for attribute[mixed]")
        }

        subs.foreach(x => x match {
            case s @ <xs:simpleContent>{_*}</xs:simpleContent> => ct.particle = Some(parseSimpleContent(s, ct, schema))
            case c @ <xs:complexContent>{_*}</xs:complexContent> => ct.particle = Some(parseComplexContent(c, ct, schema))
            case g @ <xs:group>{_*}</xs:group> => ct.particle = Some(parseGroupDecl(g, schema))
            case a @ <xs:all>{_*}</xs:all> => ct.particle = Some(parseGroup(a, new XSGroupAll, schema))
            case c @ <xs:choice>{_*}</xs:choice> => ct.particle = Some(parseGroup(c, new XSGroupChoice, schema))
            case s @ <xs:sequence>{_*}</xs:sequence> => ct.particle = Some(parseGroup(s, new XSGroupSequence, schema))
            case a @ <xs:attribute>{_*}</xs:attribute> => ct.atts += parseAttribute(a, schema)
            case a @ <xs:attributeGroup>{_*}</xs:attributeGroup> => ct.atts += parseAttributeGroup(a, schema)
            case e => throw new IllegalArgumentException("invalid element in simpleType declaration:" + e)
        })
        ct
    }

    def parseSimpleContent(node: Node, parent: XSComplexType, schema: XSSchema): XSSimpleContent = {
        if (parent.mixed) throw new IllegalArgumentException("simpleContent can't be in a mixed complexType")
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        if (subs.size != 1)
            throw new IllegalArgumentException("simpleContent declaration should contain only one of (restriction|extension):" + subs)

        val derivation = subs(0) match {
            case r @ <xs:restriction>{_*}</xs:restriction> => parseSimpleRestriction(r, schema)
            case e @ <xs:extension>{_*}</xs:extension> => parseExtension(e, schema)
            case e => throw new IllegalArgumentException("invalid element in simpleContent declaration:" + e)
        }
        val sc = XSSimpleContent(derivation)
        sc.annotation = annotation
        sc
    }

    def parseComplexContent(node: Node, parent: XSComplexType, schema: XSSchema): XSComplexContent = {
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")
        if (subs.size != 1)
            throw new IllegalArgumentException("simpleContent declaration should contain only one of (restriction|extension):" + subs)

        val derivation = subs(0) match {
            case r @ <xs:restriction>{_*}</xs:restriction> => parseComplexRestriction(r, schema)
            case e @ <xs:extension>{_*}</xs:extension> => parseExtension(e, schema)
            case e => throw new IllegalArgumentException("invalid element in complexContent declaration:" + e)
        }
        (node \ "@mixed").text match {
            case "" =>
            case "true" => parent.mixed = true
            case "false" => parent.mixed = false
            case _ => throw new IllegalArgumentException("invlaid value for attribute[mixed]")
        }
        val cc = XSComplexContent(derivation)
        cc.annotation = annotation
        cc
    }

    def parseSimpleRestriction(node: Node, schema: XSSchema): XSSimpleRestriction = {
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")

        val base = (node \ "@base")(0).text
        val sr = XSSimpleRestriction(base)
        schema.types.get(base).map {x => sr.baseType = x}
        sr.annotation = annotation
        subs.foreach (x => x match {
            case a @ <xs:minExclusive>{_*}</xs:minExclusive> => sr.facets += new XSFacetMinExclusive((x \ "@value").text)
            case c @ <xs:minInclusive>{_*}</xs:minInclusive> => sr.facets += new XSFacetMinInclusive((x \ "@value").text)
            case a @ <xs:maxExclusive>{_*}</xs:maxExclusive> => sr.facets += new XSFacetMaxExclusive((x \ "@value").text)
            case c @ <xs:maxInclusive>{_*}</xs:maxInclusive> => sr.facets += new XSFacetMaxInclusive((x \ "@value").text)
            case s @ <xs:totalDigits>{_*}</xs:totalDigits> => sr.facets += new XSFacetTotalDigits((x \ "@value").text)
            case a @ <xs:fractionDigits>{_*}</xs:fractionDigits> => sr.facets += new XSFacetFractionDigits((x \ "@value").text)
            case a @ <xs:length>{_*}</xs:length> => sr.facets += new XSFacetLength((x \ "@value").text)
            case l @ <xs:minLength>{_*}</xs:minLength> => sr.facets += new XSFacetMinLength((x \ "@value").text)
            case l @ <xs:maxLength>{_*}</xs:maxLength> => sr.facets += new XSFacetMaxLength((x \ "@value").text)
            case e @ <xs:enumeration>{_*}</xs:enumeration> => sr.facets += new XSFacetEnumeration((x \ "@value").text)
            case w @ <xs:whiteSpace>{_*}</xs:whiteSpace> => sr.facets += new XSFacetWhiteSpace((x \ "@value").text)
            case p @ <xs:pattern>{_*}</xs:pattern> => sr.facets += new XSFacetPattern((x \ "@value").text)
            case a @ <xs:attribute>{_*}</xs:attribute> =>
            case a @ <xs:attributeGroup>{_*}</xs:attributeGroup> =>
            case e => throw new IllegalArgumentException("invalid element in restriction declaration:" + e)
        })

        sr
    }
    def parseComplexRestriction(node: Node, schema: XSSchema): XSComplexRestriction = {
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")

        val base = (node \ "@base")(0).text
        val cr  = XSComplexRestriction(base)
        schema.types.get(base).map {x => cr.baseType = x}
        cr.annotation = annotation
        subs.foreach (x => x match {
            case g @ <xs:group>{_*}</xs:group> => cr.particle = Some(parseGroupDecl(g, schema))
            case a @ <xs:all>{_*}</xs:all> => cr.particle = Some(parseGroup(a, new XSGroupAll, schema))
            case c @ <xs:choice>{_*}</xs:choice> => cr.particle = Some(parseGroup(c, new XSGroupChoice, schema))
            case s @ <xs:sequence>{_*}</xs:sequence> => cr.particle = Some(parseGroup(s, new XSGroupSequence, schema))
            case a @ <xs:attribute>{_*}</xs:attribute> => cr.atts += parseAttribute(a, schema)
            case a @ <xs:attributeGroup>{_*}</xs:attributeGroup> => cr.atts += parseAttributeGroup(a, schema)
            case e => throw new IllegalArgumentException("invalid element in extension declaration:" + e)
        })

        cr
    }
    def parseExtension(node: Node, schema: XSSchema): XSExtension = {
        val (subs, annotation) = parsePossibleAnnotation(node \ "_")

        val base = (node \ "@base")(0).text
        val es = XSExtension(base)
        schema.types.get(base).map {x => es.baseType = x}
        es.annotation = annotation
        subs.foreach (x => x match {
            case g @ <xs:group>{_*}</xs:group> => es.particle = Some(parseGroupDecl(g, schema))
            case a @ <xs:all>{_*}</xs:all> => es.particle = Some(parseGroup(a, new XSGroupAll, schema))
            case c @ <xs:choice>{_*}</xs:choice> => es.particle = Some(parseGroup(c, new XSGroupChoice, schema))
            case s @ <xs:sequence>{_*}</xs:sequence> => es.particle = Some(parseGroup(s, new XSGroupSequence, schema))
            case a @ <xs:attribute>{_*}</xs:attribute> => es.atts += parseAttribute(a, schema)
            case a @ <xs:attributeGroup>{_*}</xs:attributeGroup> => es.atts += parseAttributeGroup(a, schema)
            case e => throw new IllegalArgumentException("invalid element in extension declaration:" + e)
        })

        es
    }
}
