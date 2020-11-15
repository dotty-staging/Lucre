#!/bin/bash
echo "Assumed that you ran 'sbt +clean +update dependencyUpdates evicted +test:compile +test' first!"
echo ""
if grep -q SNAP build.sbt
then
   echo "There are SNAPSHOTs in the build! Aborting."
   exit 1
fi

sbt +adjunctJS/publishSigned +adjunctJVM/publishSigned +baseJS/publishSigned +baseJVM/publishSigned +confluentJS/publishSigned +confluentJVM/publishSigned +coreJS/publishSigned +coreJVM/publishSigned +dataJS/publishSigned +dataJVM/publishSigned +exprJS/publishSigned +exprJVM/publishSigned +geomJS/publishSigned +geomJVM/publishSigned +lucre-bdb/publishSigned

