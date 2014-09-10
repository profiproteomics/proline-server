#!/bin/sh

export LANG=en_US.UTF-8

# Options: -Djava.rmi.server.hostname=132.168.72.129 -Dcom.sun.management.jmxremote.port=2194 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false
java -cp "config:Proline-Cortex-${pom.version}.jar:lib/*" -Dlogback.configurationFile=config/logback.xml -Xmx4G fr.proline.cortex.ProcessingNode
