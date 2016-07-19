package fr.proline.cortex.api

/**
 * The Interface IRemoteServiceIdentity specifies some attributes needed for Remote Service identification.
 *
 */
trait IRemoteServiceIdentity {
  
  /** The namesapce of this service like "proline/dps/msi" */
  val serviceNamespace: String
  
  /** The name of this service like "ImportResultFiles" */
  val serviceName: String
  
  /** The description of this service */
  var serviceDescription: Option[String] = None

  /** The version of this service */
  val serviceVersion: String

  /**
   * True if this service implementation version is the default one. False by default.
   * <p> For a given serviceName, it must exist only one default version.
   */
  val isDefaultVersion: Boolean
  
  /**
   * Fully qualified service path like "proline/dps/msi/ImportResultFiles".
   */
  lazy val servicePath = serviceNamespace + '/' + serviceName //+ "@" + wsVersion
  
}

trait IDefaultServiceVersion extends IRemoteServiceIdentity {
  val serviceVersion = RemoteServiceIdentity.defaultVersion
  val isDefaultVersion = true
}

/** The Object WSIdentity contains static methods useful for Web Service identification. */
object RemoteServiceIdentity {
  val defaultVersion = "1.0"
}

/**
 * The Class RemoteServiceIdentity is a basic implementation of the IRemoteServiceIdentity interface.
 *
 * @param serviceName The name of the Remote Service.
 * @param wsVersion The version of the Remote Service.
 */
// TODO: split the fully qualified serviceName into namespace and name ???
case class RemoteServiceIdentity(
  serviceNamespace: String,
  serviceName: String,
  serviceVersion: String,
  isDefaultVersion: Boolean
) extends IRemoteServiceIdentity
