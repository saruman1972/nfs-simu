import scala.xml._
import spray.json._
import spray.json.DefaultJsonProtocol._
import org.apache.commons.codec.binary.Hex

object Main {
    def main(args: Array[String]): Unit = {
val aa = "𪚺"
println(aa)
println(aa.length)
println(Hex.encodeHexString(aa.getBytes("UTF-8")))
return

implicit object NodeFormat extends JsonFormat[Node] {
  def write(node: Node) =
    if (node.child.count(_.isInstanceOf[Text]) == 1)
      JsString(node.text)
    else
      JsObject(node.child.collect {
        case e: Elem => e.label -> write(e)
      }: _*)

  def read(jsValue: JsValue) = null // not implement
}

val fruits =
  <fruits>
    <fruit>
      <name>apple</name>
      <taste>
        <sweet>true</sweet>
        <juicy>true</juicy>
      </taste>
    </fruit>
    <fruit>
      <name>banana</name>
      <taste>better</taste>
    </fruit>
  </fruits>

val json = """[{"name":"apple","taste":{"sweet":"true","juicy":"true"}},{"name":"banana","taste":"better"}]"""

assert((fruits \\ "fruit").toSeq.toJson.toString == json)
println((fruits \\ "fruit").toSeq.toJson.toString)
println(json.parseJson.getClass)

val xml = """<?xml version="1.0" encoding="UTF-8"?>
<SERVICE xmlns="http://www.allinfinance.com/dataspec/">
    <SERVICE_HEADER>
        <SERVICE_SN>0000001410596745921</SERVICE_SN>
        <SERVICE_ID>11040</SERVICE_ID>
        <ORG/>
        <CHANNEL_ID/>
        <REQUST_TIME>20140913162545</REQUST_TIME>
        <VERSION_ID>10</VERSION_ID>
    </SERVICE_HEADER>
    <SERVICE_BODY>
        <EXT_ATTRIBUTES>
        </EXT_ATTRIBUTES>
        <REQUEST>
            <ID_TYPE/>
            <ID_NO/>
            <OPT/>
            <FIRSTROW/>
            <LASTROW/>
            <RELATIONSHIP/>
            <NAME/>
            <GENDER/>
            <MOBILE_NO/>
            <BIRTHDAY/>
            <CORP_NAME/>
            <CONTACT_ID_TYPE/>
            <CONTACT_ID_NO/>
            <CORP_PHONE/>
            <CORP_FAX/>
            <CORP_POST/>
        </REQUEST>
    </SERVICE_BODY>
</SERVICE>
"""
val tt = parsing.XhtmlParser(scala.io.Source.fromString(xml))
println(tt.toSeq.apply(0).asInstanceOf[Node].toJson)


println(System.getProperty("java.class.path"))
            val theClass = fruits.getClass();
//            val theLoader = theClass.getClassLoader();
//      val theLoader = ClassLoader.getSystemClassLoader
//      val theLoader = cn.webank.rmb.common.ResourceLoader.getInstance.getClass.getClassLoader
val theLoader = new java.net.URLClassLoader(Array(new java.net.URL("file:./"), new java.net.URL("file:lib/rmb-local-api-0.5.RELEASE.jar")))
println(theLoader.asInstanceOf[java.net.URLClassLoader].getURLs.apply(0))
println(theLoader.asInstanceOf[java.net.URLClassLoader].getURLs.apply(1))
println(Class.forName("cn.webank.rmb.api.RMB", true, theLoader))
            val prop = new java.util.Properties();
            val is = theLoader.getResourceAsStream("rmb-client.properties");
println(is)
            prop.load(is);
            println("properties=" + prop);

}
}
