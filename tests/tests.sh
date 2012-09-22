#!/bin/sh
tests/test.sh t1   tests/a.xml
tests/test.sh te   tests/a.xml -e //title 
#Follow all a, print all titles
tests/test.sh tfe  tests/a.xml -f //a     -e //title
#Print all titles in all files that can be followed to
tests/test.sh tef  tests/a.xml -e //title -f //a
tests/test.sh tefe tests/a.xml -e //title -f //a      -e //title

tests/test.sh direct '<a>foobar</a>' -e '<a>{.}</a>'
tests/test.sh direct_hidden_vars1 --hide-variable-names '<a>foobar</a>' -e '<a>{.}</a>' 
tests/test.sh direct_hidden_vars2 '<a>foobar</a>' --hide-variable-names -e '<a>{.}</a>' 
tests/test.sh direct_hidden_vars3 '<a>foobar</a>' -e '<a>{.}</a>' --hide-variable-names

#read title from both
tests/test.sh 2urls tests/a.xml -e //title tests/b.xml -e //title 
#not separated urls
tests/test.sh 2urls2read tests/a.xml tests/b.xml -e //title -e //title

#stdin
echo '<test>123<x/>foo<abc>bar</abc>def<x/></test>' | tests/test.sh stdin1 - -e //abc
echo //abc2 | tests/test.sh stdin2 '<test>123<x/>foo<abc2>bar2!</abc2>def<x/></test>' -e -

#multipage template
tests/test.sh multipage --extract '<action><page url="tests/a.xml"><template><title>{.}</title></template></page></action>' --extract-kind=multipage
tests/test.sh multipage2  --extract '<action><loop var="page" list='"'"'("tests/a.xml", "b.xml")'"'"'><page url="$page;"><template><title>{.}</title></template></page></loop></action>' --extract-kind=multipage

#output formats
tests/test.sh adhoc1 tests/a.xml --extract "<a>{.}</a>*" 
tests/test.sh xml1 tests/a.xml --extract "<a>{.}</a>*" --output-format xml
tests/test.sh json1 tests/a.xml --extract "<a>{.}</a>*" --output-format json 
tests/test.sh xml1b tests/a.xml --output-format xml --extract "<a>{.}</a>*" 
tests/test.sh json1b tests/a.xml --output-format json --extract "<a>{.}</a>*" 

tests/test.sh adhoc2 tests/a.xml tests/b.xml -e "<a>{.}</a>*"
tests/test.sh xml2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format xml
tests/test.sh json2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format json 
tests/test.sh xml2b tests/a.xml tests/b.xml --output-format xml -e "<a>{.}</a>*" 
tests/test.sh json2b tests/a.xml tests/b.xml --output-format json -e "<a>{.}</a>*" 

tests/test.sh adhoc3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*"  
tests/test.sh xml3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format xml
tests/test.sh json3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format json 

#Online test
tests/test.sh google http://www.google.de -e "count(//title[contains(text(),\"Google\")])"



