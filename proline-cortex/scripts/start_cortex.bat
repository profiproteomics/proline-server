title Proline Cortex
rem Options: -Djava.rmi.server.hostname=132.168.72.129 -Dcom.sun.management.jmxremote.port=2194 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false
java -Xmx4G -XX:+UseG1GC -XX:+UseStringDeduplication -XX:MinHeapFreeRatio=10 -XX:MaxHeapFreeRatio=30 -cp "config;proline-cortex-${pom.version}.jar;lib/*" -Dlogback.configurationFile=config/logback.xml fr.proline.cortex.ProcessingNode
