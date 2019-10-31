package javax.jms;

// Taken from by javax.jms.MessageHeader.Header (JMS specs 2.1)
// TODO: remove me when we rely on JMS specs 2.1
public interface MessageHeader {
  
  public enum Header {
    JMSCorrelationID,
    JMSCorrelationIDAsBytes,
    JMSDeliveryMode,
    JMSDeliveryTime,
    JMSDestination,
    JMSExpiration,
    JMSMessageID,
    JMSPriority,
    JMSRedelivered,
    JMSReplyTo,
    JMSTimestamp,
    JMSType,
  }
  
}
