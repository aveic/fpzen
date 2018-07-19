import shapeless._
import shapeless.tag.@@

import scala.annotation.StaticAnnotation
import scala.meta._

// type tags
trait Show
trait Hide


// my case class
case class My(yes:Int @@ Show, no:String @@ Hide)


// well this sux:
val obj = My(tag[Show](2), tag[Hide]("ABC"))

// gen
val gen = LabelledGeneric[My]

// hlst
val hlist = gen.to(obj)


object mapFields extends Poly1 {
  implicit def show[A] = at[A @@ Show]((v:A) => v)
  implicit def hide[B] = at[B @@ Hide](_ => ())
}

class mappable extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods class $tName (..$params) extends $template" = defn
    println(tName) //this actually prints during compilation
  }
}


hlist.map(mapFields).filterNot[Unit] // на выходе даёт:
// res0: Int with shapeless.tag.Tagged[Show] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("yes")],Int with shapeless.tag.Tagged[Show]] :: shapeless.HNil = 2 :: HNil


