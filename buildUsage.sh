#!/bin/sh
sed -e "s/'/''/g" readme  | awk '{print "writeln(\047"$0"\047);"}'  | less > printUsage.inc
