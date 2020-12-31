#!/bin/bash


#create names with:
#xidel cache/expath-file.xml --xquery 'for $eg in //eg group by $fun := $eg//ex:function[1] return extract($eg, "file:[^\(]+\(.*?\) as", 0, "s*") ! <f name="{substring-after($fun, ":")}" args="{extract(., "\$([^ ]+)", 1, "*")}"/> ' --xml > functions-arguments-names-file.xml 


xidel functions.xml -e @functions.xq --output-format html --trace-stack > /tmp/new
