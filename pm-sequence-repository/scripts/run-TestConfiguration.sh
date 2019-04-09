#!/bin/sh

java -cp "config:pm-sequence-repository-${project.version}.jar:lib/*" fr.proline.module.seq.service.ListMatchingRules
