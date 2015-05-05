#!/bin/sh

java -cp "config:PM-SequenceRepository-${project.version}.jar:lib/*" fr.proline.module.seq.service.RetrieveService 2
