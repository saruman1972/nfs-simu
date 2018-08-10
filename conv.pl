#!/usr/bin/perl
use strict;

while (<>) {
    chomp;
    if (/^.*<xs:include schemaLocation=\"servicePublic\.xsd\"\/>.*$/) {
        # filter out this line
    }
    elsif (/^.*<xs:complexType name="NF(\d+)Request">.*$/) {
        print "<xs:include schemaLocation=\"serviceDoc.xsd\"/>\n";
        print "    <xs:element name=\"SERVICE_ID\" fixed=\"${1}\" type=\"xs:string\">\n";
        print "        <xs:annotation>\n";
        print "            <xs:documentation>交易服务码</xs:documentation>\n";
        print "        </xs:annotation>\n";
        print "    </xs:element>\n";
        print "    <xs:complexType name=\"REQUEST\">\n";
    } else {
        print "$_\n";
    }
}

