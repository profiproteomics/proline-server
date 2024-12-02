#!/bin/sh

#------------------------------------------------
# Simple shell-script to run HornetQ Cortex Ready standalone server
#------------------------------------------------
cd ./hornetq_light-2.4.0.Final/bin
chmod +x run.sh
chmod +x ../jre1.8/bin/java
./run.sh
