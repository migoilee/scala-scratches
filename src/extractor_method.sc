// EXTRACTOR OBJECTS (OBJECT THAT HAS UNAPPLY METHOD)

// SINGLE PARAMETER
class CustomerId(val id: Long)

object CustomerId {
  def apply(id: Long) = new CustomerId(id)
  def unapply(cid: CustomerId): Option[Long] = Some(cid.id)
}

val cid = CustomerId(123L)
cid match {
  case CustomerId(id) => println(s"Id: [$id]")
}

// MULTIPLE PARAMETERS
class CustomerDetails(val id: CustomerId, val name: String, val address: String)

object CustomerDetails {
  def apply(id: CustomerId, name: String, address: String) = new CustomerDetails(id, name, address)
  def unapply(cd: CustomerDetails): Option[(CustomerId, String, String)] = Some((cd.id, cd.name, cd.address))
}

val cd = CustomerDetails(CustomerId(123L), "amy", "136 elstree gardens")
cd match {
  case CustomerDetails(id, name, addr) =>  println(s"Id: [${id.id}], Name: [$name], Address: [$addr]")
}

// CUSTOM FACTORY METHOD
object SpecialString {
  def apply(str: String): String = s"SpecialString: $str"

  val regex = "SpecialString: [A-Za-z0-9]+".r
  def unapply(specialString: String): Option[String] = {
    regex.findFirstIn(specialString)
  }
}

def extractOriginalString(specialString: String) = {
  specialString match {
    case SpecialString(original) => println(s"Got original string [$original] from special [$specialString]")
    case _ => println(s"[$specialString] is not a special string")
  }
}

extractOriginalString(SpecialString("amy"))
extractOriginalString("iAmNotSpecial")