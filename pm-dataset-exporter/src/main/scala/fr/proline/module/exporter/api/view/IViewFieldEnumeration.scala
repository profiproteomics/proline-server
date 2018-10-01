package fr.proline.module.exporter.api.view

trait IViewFieldEnumeration extends Enumeration {
  val thisenum: IViewFieldEnumeration = this
  
  //def values: Seq[Field] = thisenum.values
  
  protected final def Field(name: String): Field = new Field(this.nextId, name)  
  class Field(i: Int, name: String) extends Val(i: Int, name: String) {
    val $fields: IViewFieldEnumeration = thisenum    
  }
  
}
