title Sequence Repository On Demand
rem Options: -Djava.rmi.server.hostname=<host IP> -Dcom.sun.management.jmxremote.port=2194 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false
java -Xmx4G -XX:+UseG1GC -XX:+UseStringDeduplication -XX:MinHeapFreeRatio=10 -XX:MaxHeapFreeRatio=30 -cp "config;PM-SequenceRepository-${project.version}.jar;lib\*" fr.proline.module.seq.jms.RunNode
