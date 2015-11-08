package scala.json

object AST {

  sealed trait JValue

  case object JNull extends JValue

  case class JString(s: String) extends JValue

  sealed trait JNumber
  case class JDecimal(num: BigDecimal) extends JValue with JNumber
  case class JInt(num: BigInt) extends JValue with JNumber

  case class JBool(value: Boolean) extends JValue
  object JBool {
    val True = JBool(true)
    val False = JBool(false)
  }

  case class JArray(arr: List[JValue]) extends JValue {
    def apply(i: Int): JValue = arr(i)
  }

  type JField = (String, JValue)
  object JField {
    def apply(name: String, value: JValue) = (name, value)
    def unapply(f: JField): Option[(String, JValue)] = Some(f)
  }

  case class JObject(fields: List[JField]) extends JValue
  object JObject {
    def apply(fs: JField*): JObject = JObject(fs.toList)
  }

}
