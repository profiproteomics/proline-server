package fr.profi.obo

/**
 * OboTerm
 * Specification: http://oboformat.googlecode.com/svn/trunk/doc/GO.format.obo-1_2.html
 * 
 * @param id The unique id of the current term.
 * @param name The term name.
 * @param definition The definition of the current term.
 * @param isAnonymous Whether or not the current object has an anonymous id.
 * @param altId Defines an alternate id for this term. A term may have any number of alternate ids.
 * @param comment A comment for this term.
 * @param subsets This field indicates a term subset to which this term belongs.
 * @param synonyms This field gives a synonym for this term, some xrefs to describe the origins of the synonym, and may indicate a synonym category or scope information.
 * @param xrefs A dbxref that describes an analagous term in another vocabulary.
 * @param isA This tag describes a subclassing relationship between one term and another.
 * @param intersectionOf This tag indicates that this term is equivalent to the intersection of several other terms.
 * @param unionOf This tag indicates that this term represents the union of several other terms.
 * @param disjointFrom This tag indicates that a term is disjoint from another, meaning that the two terms have no instances or subclasses in common.
 * @param relationship This tag describes a typed relationship between this term and another term.
 * @param isObsolete Whether or not this term is obsolete.
 * @param replacedBy Gives a term which replaces an obsolete term.
 * @param consider Gives a term which may be an appropriate substitute for an obsolete term, but needs to be looked at carefully by a human expert before the replacement is done. 
 * @param createdBy Optional tag added by OBO-Edit to indicate the creator of the term.
 * @param creationDate Optional tag added by OBO-Edit to indicate the creation time and date of the term.
 */
case class OboTerm(
  
  // Cv term attributes
  id: String,
  name: String,
  definition: String,
  isAnonymous: Boolean = false,
  altIds: Array[String] = Array(),
  comment: Option[String] = None,
  subsets: Array[String] = Array(),
  synonyms: Array[OboTermSynonym] = Array(),
  xrefs: Array[OboTermXRef] = Array(),
  isA: Array[String] = Array(),
  intersectionOf: Array[OboTermRelationship] = Array(),
  unionOf: Array[String] = Array(),
  disjointFrom: Array[String] = Array(),
  relationships: Array[OboTermRelationship] = Array(),
  isObsolete: Boolean = false,
  replacedBy: Array[String] = Array(),
  consider: Array[String] = Array(),
  createdBy: Option[String] = None,
  creationDate: Option[String] = None
)

object OboSynonymScope extends Enumeration {
  val BROAD = Value("BROAD")
  val EXACT = Value("EXACT")
  val NARROW = Value("NARROW")
  val RELATED = Value("RELATED")
}

case class OboTermRelationship(
  linkType: String,
  target: String
)

case class OboTermSynonym(
  scope: String,
  definition: String,
  typeName: Option[String] = None,
  dbXrefs: Array[OboTermXRef] = Array()
)

case class OboTermXRef(
  db: String,
  ac: String,
  description: Option[String] = None,
  modifier: Option[String] = None
) {
  def name = db + ':' + ac
}