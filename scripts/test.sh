#!/bin/bash
sbt -J-Xmx2G "; + adjunctJS/test ; + adjunctJVM/test ; + baseJS/test ; + baseJVM/test ; + confluentJS/test ; + confluentJVM/test ; + coreJS/test ; + coreJVM/test ; + dataJS/test ; + dataJVM/test ; + expr0JS/test ; + expr0JVM/test ; + expr1JS/test ; + expr1JVM/test ; + exprJS/test ; + exprJVM/test ; + geomJS/test ; + geomJVM/test ; + lucre-bdb/test"

sbt -J-Xmx4G "; + testsJS/test ; + testsJVM/test"
