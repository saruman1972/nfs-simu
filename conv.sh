#!/bin/sh

for i in `ls ~/Desktop/xsd/*Request.xsd`
do
    xsdFile=`basename $i`
    echo "converting ${xsdFile}"
    perl ./conv.pl $i > xsd/${xsdFile}
done

