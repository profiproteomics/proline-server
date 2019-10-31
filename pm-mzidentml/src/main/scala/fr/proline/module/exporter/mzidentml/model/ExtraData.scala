package fr.proline.module.exporter.mzidentml.model

case class Contact(firstName : String,
                   lastName: String,
                   email: Option[String],
                   url:Option[String])

case class Organization(name: String,
                        url:Option[String])




