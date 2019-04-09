title Sequence Repository Service
rem Options: -Djava.rmi.server.hostname=<host> -Dcom.sun.management.jmxremote.port=2194 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false
java -Xmx4G -XX:+UseG1GC -XX:+UseStringDeduplication -XX:MinHeapFreeRatio=10 -XX:MaxHeapFreeRatio=30 -cp "config;pm-sequence-repository-${project.version}.jar;lib\*" fr.proline.module.seq.service.RetrieveService -t 2
