#!/bin/sh
tr -d '\r' < readme.txt | sed -e "s/'/''/g"  | awk '{print "writeln(\047"$0"\047);"}'  | less > printUsage.inc
