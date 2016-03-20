#!/bin/sh

tests/test.sh t1   tests/a.xml
tests/test.sh te   tests/a.xml -e //title 
#Follow all a, print all titles
tests/test.sh tfe  tests/a.xml -f //a     -e //title
tests/test.sh tfe2 tests/b.xml -f //a     -e //title
tests/test.sh tfe3 tests/c.xml -f //a     -e //title
tests/test.sh tfe4 tests/d.xml -f //a     -e //title
tests/test.sh tfe5 tests/dpre.xml -f //a     -e //title
#Print all titles in all files that can be followed to
tests/test.sh tef  tests/a.xml -e //title -f //a
tests/test.sh tefe tests/a.xml -e //title -f //a      -e //title

tests/test.sh direct '<a>foobar</a>' -e '<a>{.}</a>'
tests/test.sh direct_hidden_vars1 --hide-variable-names '<a>foobar</a>' -e '<a>{.}</a>' 
tests/test.sh direct_hidden_vars2 '<a>foobar</a>' --hide-variable-names -e '<a>{.}</a>' 
tests/test.sh direct_hidden_vars3 '<a>foobar</a>' -e '<a>{.}</a>' --hide-variable-names

#follow max level
tests/test.sh maxlevel0 tests/a.xml --follow-level 0 -e //title -f //A --allow-repetitions
tests/test.sh maxlevel1 tests/a.xml --follow-level 1 -e //title -f //A --allow-repetitions
tests/test.sh maxlevel2  tests/a.xml --follow-level 2 -e //title -f //A --allow-repetitions
tests/test.sh maxlevel3  tests/a.xml --follow-level 3 -e //title -f //A --allow-repetitions
tests/test.sh levelcloned tests/a.xml  [ -f '(//a)[2]' -e 'extract($url,".*[/\\\\](.*)",1)' ] -f '(//a)[1]'

#"sibling tests"
tests/test.sh sibling1a '<empty/>' -e "a:=17"  tests/a.xml -e '<a>{z:=$a + 1}</a>'
tests/test.sh sibling1b "<a/>"  -e "a:=17"  tests/a.xml -e 'a:=909'
tests/test.sh sibling1c "<a/>"  -e "a:=17"  tests/a.xml -e 'a:=909' -e 'a:=10'
tests/test.sh sibling1d "<a/>"  -e "a:=17" -e 'a:=$a+1'  tests/a.xml -e 'a:=$a+2' -e 'a:=$a+3'
tests/test.sh sibling1e "<a/>"  -e "a:=17" -e 'a:=$a+1'  tests/a.xml -e 'a:=$a+2' -e 'a:=$a+3' '<b/>' -e 'a:=$a+4' -e 'a:=$a+5'
tests/test.sh sibling2  '<a>123</a>' '<a>456</a>' -e '<a>{$x}</a>'
tests/test.sh sibling2  -e '<a>{$x}</a>' '<a>123</a>' '<a>456</a>' 
tests/test.sh sibling2b '<a>123</a>' -e '<a>{$x}</a>' '<a>456</a>' 
tests/test.sh sibling3a '<t>1</t>' -e 'concat(/t, "b")' '<t>2</t>' -e 'concat(/t, "c")'
tests/test.sh sibling3b -e 'concat(/t, "a")' '<t>1</t>' -e 'concat(/t, "b")' '<t>2</t>' #extract applied to previous data (think that is good, right?)
tests/test.sh sibling3c -e 'concat(/t, "a")' '<t>1</t>' -e 'concat(/t, "b")' '<t>2</t>' -e 'concat(/t, "c")' 
tests/test.sh sibling4 tests/a.xml  -f //a     -e //title   tests/dpre.xml -f //a     -e //title  

tests/test.sh tfe  tests/a.xml -f //a     -e //title
tests/test.sh tfe2 tests/b.xml -f //a     -e //title

#read title from both
tests/test.sh 2urls tests/a.xml -e //title tests/b.xml -e //title 
#not separated urls
tests/test.sh 2urls2read tests/a.xml tests/b.xml -e //title -e //title

#variable tests
export environ1=1
export environ2=xyz

tests/test.sh var1 '<a>hello</a>' -e 'var:=.' 
tests/test.sh var2 '<a>hello</a>' -e 'var:=.' -e 'var2 := 17'
tests/test.sh var3 '<a>hello</a>' -e 'var:=.' -e 'var2 := 17' -e '<a>{var3:=.}</a>'
tests/test.sh novar '<a>hello</a>' -e '.' 
tests/test.sh novar2 '<a>hello</a>' -e '.'  -e '<a>{.}</a>'
tests/test.sh varmix '<a>hello</a>' -e '.'  -e '<a>{temp:=.}</a>' -e '3+4' -e 'res:=$result'
tests/test.sh varmixb '<a>hello</a>' -e 'concat(">", ., "<")'  -e '<a>{temp:=$result}</a>' -e '3+4' -e 'res:=$result'
tests/test.sh varenviron --variable environ1 -e '$environ1'
tests/test.sh varenviron2 --variable environ1,environ2 -e '$environ1||$environ2'
tests/test.sh varnoenviron --variable foo=bar -e '$foo'

#stdin
echo '<test>123<x/>foo<abc>bar</abc>def<x/></test>' | tests/test.sh stdin1 - -e //abc
echo //abc2 | tests/test.sh stdin2 '<test>123<x/>foo<abc2>bar2!</abc2>def<x/></test>' -e -

#multipage template
tests/test.sh multipage --extract '<action><page url="tests/a.xml"><template><title>{.}</title></template></page></action>' --extract-kind=multipage #deprecated
tests/test.sh multipage --extract '<action><page url="tests/a.xml"/><pattern><title>{.}</title></pattern></action>' --extract-kind=multipage
tests/test.sh multipage --extract '<action><page url="tests/a.xml"/><pattern href="tests/patterntit.xml"/></action>' --extract-kind=multipage
tests/test.sh multipage2  --extract '<action><s>1+2+3</s><loop var="page" list='"'"'("tests/a.xml", "b.xml")'"'"'><page url="{$page}"><template><title>{.}</title></template></page></loop></action>' --extract-kind=multipage #deprecated
tests/test.sh multipage2  --extract '<action><loop var="page" list='"'"'("tests/a.xml", "b.xml")'"'"'><page url="{$page}"/><pattern><title>{.}</title></pattern></loop></action>' --extract-kind=multipage
tests/test.sh multipage2  --extract '<action><loop var="page" list='"'"'("tests/a.xml", "b.xml")'"'"'><page url="{$page}"/><pattern href="tests/patterntit.xml"/></loop></action>' --extract-kind=multipage
tests/test.sh multipage3 --extract '<action><page url="tests/a.xml"><template><html>{extract(., "[A-Z]+")}</html></template></page></action>' --extract-kind=multipage
tests/test.sh multipage3 --extract '<action><page url="tests/a.xml"><template><html>{extract($raw, "[A-Z]+")}</html></template></page></action>' --extract-kind=multipage
tests/test.sh multipage3 --extract '<action><page url="tests/a.xml"/><pattern><html>{extract($raw, "[A-Z]+")}</html></pattern></action>' --extract-kind=multipage
tests/test.sh multipage3 --extract-kind=multipage --extract '<action><page url="tests/a.xml"/><s>x:=//title</s></action>'  --xpath '_result := $x'
tests/test.sh multipage4  --extract '<action><loop var="page" list='"'"'("tests/a.xml", "b.xml")'"'"'><page url="{$page}"/><pattern href="tests/patterntit.xml"/><pattern href="tests/patterntit.xml"/><pattern href="tests/patterntit.xml"/></loop></action>' --extract-kind=multipage

tests/test.sh multipageVariable  --extract-kind=multipage --extract '<action><variable name="test">1+2+3</variable></action>' --xpath '$test' ##?? change to print all variables
tests/test.sh multipageChoose   --extract-kind=multipage --extract '<action><choose><when test="1=2"><variable name="result">"a"</variable></when><when test="2=2"><variable name="result">"b"</variable></when><when test="3=2"><variable name="result">"c"</variable></when></choose></action>' --xpath '$result'
tests/test.sh multipageChoose3   --extract-kind=multipage --extract '<action><choose><when test="1=2"><variable name="result">"a"</variable></when><when test="3=2"><variable name="result">"b"</variable></when><when test="3=3"><variable name="result">"c"</variable></when></choose></action>' --xpath '$result'
tests/test.sh multipageChoose4   --extract-kind=multipage --extract '<action><choose><when test="1=2"><variable name="result">"a"</variable></when><when test="3=2"><variable name="result">"b"</variable></when><when test="4=3"><variable name="result">"c"</variable></when></choose></action>' --xpath '$result' #has an error in correct output
tests/test.sh multipageChooseO1   --extract-kind=multipage --extract '<action><choose><when test="1=2"><variable name="result">"a"</variable></when><when test="3=2"><variable name="result">"b"</variable></when><when test="4=3"><variable name="result">"c"</variable></when><otherwise><variable name="result">"x"</variable></otherwise></choose></action>' --xpath '$result'
tests/test.sh multipageChooseO2   --extract-kind=multipage --extract '<action><choose><when test="1=2"><variable name="result">"a"</variable></when><when test="3=2"><variable name="result">"b"</variable></when><when test="3=3"><variable name="result">"c"</variable></when><otherwise><variable name="result">"x"</variable></otherwise></choose></action>' --xpath '$result'
tests/test.sh multipageChooseO3   --extract-kind=multipage --extract '<action><choose><otherwise><variable name="result">"x"</variable></otherwise></choose></action>' --xpath '$result'
tests/test.sh multipageIf0   --extract-kind=multipage --extract '<action><if test="false()"><variable name="result">"x"</variable></if></action>' --xpath 'get("result")'
tests/test.sh multipageIf1   --extract-kind=multipage --extract '<action><if test="true()"><variable name="result">"x"</variable></if></action>' --xpath 'get("result")'

tests/test.sh multipageShort   --extract-kind=multipage --extract '<action><s>result:=123</s></action>' --xpath 'get("result")'
tests/test.sh multipageShort   --extract-kind=multipage --extract '<action><s test="true()">result:=123</s></action>' --xpath 'get("result")'
tests/test.sh multipageShortF  --extract-kind=multipage --extract '<action><s test="false()">result:=123</s></action>' --xpath 'get("result")'

tests/test.sh multipageTry  --extract-kind=multipage --extract '<action> 
  <try><s>int("f")</s><catch errors="err:FORG0001"><s>x:=1</s></catch></try> 
  <try><s>int("f")</s><catch errors="x y z err:FORG0001"><s>x:=$x+1</s></catch></try> 
  <try><s>int("f")</s><catch errors=""/><catch errors="*:FORG0001"><s>x:=$x+1</s></catch></try> 
  <try><s>int("f")</s><catch errors="pxp:FORG0001"/><catch errors="*"><s>x:=$x+1</s></catch></try> 
  <try><s>int("f")</s><catch errors="foo bar"/><catch errors="*:*"><s>x:=$x+1</s></catch></try> 
  <try><s>int("f")</s><catch errors="FORG0001"/><catch errors="err:*"><s>x:=$x+1</s></catch></try> 
  <try><s>int("f")</s><catch><s>x:=$x+1</s></catch></try> 
  <try><s>int("f")</s><catch errors="*:FORG0001"><s>x:=$x+1</s></catch></try>  </action>'  --xpath '$x'
tests/test.sh multipageTryHttp --extract-kind=multipage --extract '<action> 
    <try><page url="http://example.org/invalid"/><catch errors="http"><s>x:=1</s></catch></try> 
    <try><page url="http://example.org/invalid"/><catch errors="http300"/><catch errors="http404"><s>x:=$x+1</s></catch><catch errors="http404"><s>x:=$x+1</s></catch></try> 
    <try><page url="http://example.org/invalid"/><catch errors="http:300"/><catch errors="http:404"><s>x:=$x+1</s></catch><catch errors="http:404"><s>x:=$x+1</s></catch></try> 
    <try><page url="http://example.org/invalid"/><catch errors="http:3*"/><catch errors="http:4*"><s>x:=$x+1</s></catch></try> 
    <try><page url="http://example.org/invalid"/><catch errors="http:3xx"/><catch errors="http:4xx"><s>x:=$x+1</s></catch></try> 
    <try><page url="http://example.org/invalid"/><catch errors="http:44*"/><catch errors="http:*"><s>x:=$x+1</s></catch></try> 
    <try><page url="http://example.org/invalid"/><catch errors="err:*"/><catch errors="pxp:*"><s>x:=$x+1</s></catch></try> 
</action>'  --xpath '$x'

#output formats
tests/test.sh adhoc1 tests/a.xml --extract "<a>{.}</a>*" 
tests/test.sh xml1 tests/a.xml --extract "<a>{.}</a>*" --output-format xml-wrapped
tests/test.sh json1 tests/a.xml --extract "<a>{.}</a>*" --output-format json-wrapped
tests/test.sh json1 tests/a.xml --extract "<a>{.}</a>*" --output-format json  #deprecated
tests/test.sh xmlraw1 tests/a.xml --extract "<a>{.}</a>*" --output-format xml
tests/test.sh htmlraw1 tests/a.xml --extract "<a>{.}</a>*" --output-format html
tests/test.sh bash1 tests/a.xml --extract "<a>{.}</a>*" --output-format bash
tests/test.sh cmd1 tests/a.xml --extract "<a>{.}</a>*" --output-format cmd
tests/test.sh xml1b tests/a.xml --output-format xml-wrapped --extract "<a>{.}</a>*" 
tests/test.sh json1b tests/a.xml --output-format json-wrapped --extract "<a>{.}</a>*" 
tests/test.sh json1b tests/a.xml --output-format json --extract "<a>{.}</a>*"  #deprecated
tests/test.sh xmlraw1b tests/a.xml --output-format xml --extract "<a>{.}</a>*" 
tests/test.sh htmlraw1b tests/a.xml --output-format html --extract "<a>{.}</a>*" 
tests/test.sh xmlraw1c tests/a.xml --output-format xml --extract "<a>{.}</a>*"  --output-declaration="<?xml>"

tests/test.sh adhoc2 tests/a.xml tests/b.xml -e "<a>{.}</a>*"
tests/test.sh xml2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format xml-wrapped
tests/test.sh json2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format json-wrapped
tests/test.sh json2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format json  #deprecated
tests/test.sh xmlraw2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format xml
tests/test.sh htmlraw2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format html
tests/test.sh bash2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format bash
tests/test.sh cmd2 tests/a.xml tests/b.xml -e "<a>{.}</a>*" --output-format cmd
tests/test.sh xml2b tests/a.xml tests/b.xml --output-format xml-wrapped -e "<a>{.}</a>*" 
tests/test.sh json2b tests/a.xml tests/b.xml --output-format json-wrapped -e "<a>{.}</a>*" 
tests/test.sh json2b tests/a.xml tests/b.xml --output-format json -e "<a>{.}</a>*"  #deprecated
tests/test.sh xmlraw2b tests/a.xml tests/b.xml --output-format xml -e "<a>{.}</a>*" 
tests/test.sh htmlraw2b tests/a.xml tests/b.xml --output-format html -e "<a>{.}</a>*" 

tests/test.sh adhoc3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*"  
tests/test.sh xml3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format xml-wrapped
tests/test.sh json3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format json-wrapped
tests/test.sh json3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format json #deprecated option
tests/test.sh xmlraw3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format xml
tests/test.sh htmlraw3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format html
tests/test.sh bash3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format bash
tests/test.sh cmd3 tests/a.xml tests/b.xml --extract "<title>{title:=.}</title><a>{.}</a>*" --output-format cmd

tests/test.sh adhoc4 -e '"<foobar>"' 
tests/test.sh xml4 -e '"<foobar>"' --output-format xml-wrapped
tests/test.sh json4 -e '"<foobar>"' --output-format json-wrapped
tests/test.sh xmlraw4 -e '"<foobar>"' --output-format xml
tests/test.sh htmlraw4 -e '"<foobar>"' --output-format html
tests/test.sh bash4 -e '"<foobar>"' --output-format bash
tests/test.sh cmd4 -e '"<foobar>"' --output-format cmd

tests/test.sh adhoc4b -e 'xquery version "1.0"; <foobar/>' 
tests/test.sh xml4b -e 'xquery version "1.0"; <foobar/>' --output-format xml-wrapped
tests/test.sh json4b -e 'xquery version "1.0"; <foobar/>' --output-format json-wrapped
tests/test.sh xmlraw4b -e 'xquery version "1.0"; <foobar/>' --output-format xml
tests/test.sh htmlraw4b -e 'xquery version "1.0"; <foobar/>' --output-format html
tests/test.sh bash4b -e 'xquery version "1.0"; <foobar/>' --output-format bash

   #testing, if text nodes are surrounded by a text and if node ares escaped
tests/test.sh adhoc5 '<x>123</x>'  -e '/x/text()' 
tests/test.sh xml5 '<x>123</x>' -e '/x/text()' --output-format xml-wrapped
tests/test.sh json5 '<x>123</x>' -e '/x/text()' --output-format json-wrapped
tests/test.sh xmlraw5 '<x>123</x>' -e '/x/text()' --output-format xml
tests/test.sh htmlraw5 '<x>123</x>' -e '/x/text()' --output-format html
tests/test.sh adhoc5 '<x>123</x>'  -e '/x' --printed-node-format text 
tests/test.sh xml5 '<x>123</x>' -e '/x' --output-format xml-wrapped --printed-node-format text
tests/test.sh json5 '<x>123</x>' -e '/x' --output-format json-wrapped --printed-node-format text
tests/test.sh xmlraw5 '<x>123</x>' -e '/x' --output-format xml --printed-node-format text
tests/test.sh htmlraw5 '<x>123</x>' -e '/x' --output-format html --printed-node-format text
tests/test.sh bash5 '<x>123</x>' -e '/x' --output-format bash
tests/test.sh adhoc5 '<x>123</x>'  -e '/x' 
tests/test.sh xml5 '<x>123</x>' -e '/x' --output-format xml-wrapped 
tests/test.sh json5 '<x>123</x>' -e '/x' --output-format json-wrapped 
tests/test.sh xmlraw5b '<x>123</x>' -e '/x' --output-format xml 
tests/test.sh htmlraw5b '<x>123</x>' -e '/x' --output-format html 
tests/test.sh adhoc5c '<x>123</x>'  -e '/x' --printed-node-format xml
tests/test.sh xml5c '<x>123</x>' -e '/x' --output-format xml-wrapped --printed-node-format xml
tests/test.sh json5c '<x>123</x>' -e '/x' --output-format json-wrapped --printed-node-format xml
tests/test.sh xmlraw5c '<x>123</x>' -e '/x' --output-format xml --printed-node-format xml
tests/test.sh htmlraw5c '<x>123</x>' -e '/x' --output-format html --printed-node-format xml

tests/test.sh adhoc5d '<x>123</x>'  -e '<x>{temp:=text()}</x>' 
tests/test.sh xml5d '<x>123</x>' -e '<x>{temp:=text()}</x>' --output-format xml-wrapped
tests/test.sh json5d '<x>123</x>' -e '<x>{temp:=text()}</x>' --output-format json-wrapped
tests/test.sh xmlraw5d '<x>123</x>' -e '<x>{temp:=text()}</x>' --output-format xml
tests/test.sh htmlraw5d '<x>123</x>' -e '<x>{temp:=text()}</x>' --output-format html
tests/test.sh bash5d '<x>123</x>' -e '<x>{temp:=text()}</x>' --output-format bash

tests/test.sh adhoc6 '<x>123</x>'  -e 'a:=1, b:=2'
tests/test.sh xml6 '<x>123</x>'  -e 'a:=1, b:=2' --output-format xml-wrapped
tests/test.sh json6 '<x>123</x>'  -e 'a:=1, b:=2' --output-format json-wrapped
tests/test.sh xmlraw6 '<x>123</x>'  -e 'a:=1, b:=2' --output-format xml
tests/test.sh htmlraw6 '<x>123</x>'  -e 'a:=1, b:=2' --output-format html
tests/test.sh bash6 '<x>123</x>'  -e 'a:=1, b:=2' --output-format bash

tests/test.sh adhoc7 '<x>&nbsp;&auml;&nbsp&uuml&xyz;&123;&</x>' -e /x
tests/test.sh xml7 '<x>&nbsp;&auml;&nbsp&uuml&xyz;&123;&</x>' -e /x --output-format xml-wrapped
tests/test.sh json7 '<x>&nbsp;&auml;&nbsp&uuml&xyz;&123;&</x>' -e /x --output-format json-wrapped
tests/test.sh xmlraw7 '<x>&nbsp;&auml;&nbsp&uuml&xyz;&123;&</x>' -e /x --output-format xml
tests/test.sh htmlraw7 '<x>&nbsp;&auml;&nbsp&uuml&xyz;&123;&</x>' -e /x --output-format html
tests/test.sh bash7 '<x>&nbsp;&auml;&nbsp&uuml&xyz;&123;&</x>' -e /x --output-format bash
tests/test.sh cmd7 '<x>&nbsp;&auml;&nbsp&uuml&xyz;&123;&</x>' -e /x --output-format cmd

tests/test.sh adhoc8 -e '(1,2)' -e 5 -e '()' -e 7 -e v:=18 
tests/test.sh xml8 --output-format xml-wrapped -e '(1,2)' -e 5 -e '()' -e 7 -e v:=18 
tests/test.sh json8 --output-format json-wrapped -e '(1,2)' -e 5 -e '()' -e 7 -e v:=18 
tests/test.sh xmlraw8 --output-format xml -e '(1,2)' -e 5 -e '()' -e 7 -e v:=18
tests/test.sh htmlraw8 --output-format html -e '(1,2)' -e 5 -e '()' -e 7 -e v:=18
tests/test.sh bash8 --output-format bash -e '(1,2)' -e 5 -e '()' -e 7 -e v:=18
tests/test.sh cmd8 --output-format bash -e '(1,2)' -e 5 -e '()' -e 7 -e v:=18


tests/test.sh adhoc9 -e '(1,2)' -e 5 -e '""' -e v:=18 
tests/test.sh xml9 --output-format xml-wrapped  -e '(1,2)' -e 5 -e '""' -e v:=18 
tests/test.sh json9 --output-format json-wrapped -e '(1,2)' -e 5 -e '""' -e v:=18 
tests/test.sh xmlraw9 --output-format xml -e '(1,2)' -e 5 -e '""' -e v:=18 
tests/test.sh htmlraw9 --output-format html -e '(1,2)' -e 5 -e '""' -e v:=18 
tests/test.sh bash9 --output-format bash -e '(1,2)' -e 5 -e '""' -e v:=18 
tests/test.sh cmd9 --output-format bash -e '(1,2)' -e 5 -e '""' -e v:=18 

tests/test.sh adhoc9b --print-type-annotations -e '(1,2)' -e 5 -e '()' -e '""' -e 7 -e v:=18 
tests/test.sh xml9b --output-format xml-wrapped --print-type-annotations -e '(1,2)' -e 5 -e '()' -e '""' -e 7 -e v:=18 
tests/test.sh json9b --output-format json-wrapped --print-type-annotations -e '(1,2)' -e 5 -e '()' -e '""' -e 7 -e v:=18 
tests/test.sh xmlraw9b --output-format xml --print-type-annotations -e '(1,2)' -e 5 -e '()' -e '""' -e 7 -e v:=18 
#tests/test.sh htmlraw9b --output-format html --print-type-annotations -e '(1,2)' -e 5 -e '()' -e '""' -e 7 -e v:=18 
tests/test.sh bash9b --output-format bash --print-type-annotations -e '(1,2)' -e 5 -e '()' -e '""' -e 7 -e v:=18 

tests/test.sh separator-adhoc --output-separator x -e '(1,2)' -e 5 -e '""' -e v:=18 -e 0 
tests/test.sh separator-xml --output-format xml --output-separator x -e '(1,2)' -e 5 -e '""' -e v:=18 -e 0 
tests/test.sh separator-html --output-format html --output-separator x -e '(1,2)' -e 5 -e '""' -e v:=18 -e 0 

tests/test.sh separator-xml2 --output-format xml --output-separator '<br>' --output-header '<div>' --output-footer '</div>' -e '(1,2)' -e 5 -e '""' -e v:=18 -e 0 
tests/test.sh separator-html2 --output-format html --output-separator '<br>' --output-header '<div>' --output-footer '</div>' -e '(1,2)' -e 5 -e '""' -e v:=18 -e 0 

tests/test.sh adhoc10  '<x><a>1</a><a>2</a><a>3</a></x>' -e '<a>{.}</a>+' -e '<a>{.}</a>' -e 7
tests/test.sh xml10  --output-format xml-wrapped  '<x><a>1</a><a>2</a><a>3</a></x>' -e '<a>{.}</a>+' -e '<a>{.}</a>' -e 7
tests/test.sh json10 --output-format json-wrapped '<x><a>1</a><a>2</a><a>3</a></x>' -e '<a>{.}</a>+' -e '<a>{.}</a>' -e 7
tests/test.sh xmlraw10 --output-format xml  '<x><a>1</a><a>2</a><a>3</a></x>' -e '<a>{.}</a>+' -e '<a>{.}</a>' -e 7

tests/test.sh adhoc11 --print-type-annotations -e '2,xs:int(17), xs:untypedAtomic("?"),3.7,xs:float(10)' -e '{"a": 1}' --xquery '<a>bc</a>'  -e '[10]'
#tests/test.sh xml11  --output-format xml-wrapped --print-type-annotations -e '2,xs:int(17), xs:untypedAtomic("?"),3.7,xs:float(10)' -e '{"a": 1}' --xquery '<a>bc</a>'  -e '[10]'
#tests/test.sh json11 --output-format json-wrapped --print-type-annotations -e '2,xs:int(17), xs:untypedAtomic("?"),3.7,xs:float(10)' -e '{"a": 1}' --xquery '<a>bc</a>'  -e '[10]'
tests/test.sh xmlraw11 --output-format xml --print-type-annotations -e '2,xs:int(17), xs:untypedAtomic("?"),3.7,xs:float(10)' -e '{"a": 1}' --xquery '<a>bc</a>'  -e '[10]'


tests/test.sh adhoc-json -e '[1,2,3,{"a": 123,"b":"c"}]'
tests/test.sh xml-json -e '[1,2,3,{"a": 123,"b":"c"}]' --output-format xml
tests/test.sh html-json -e '[1,2,3,{"a": 123,"b":"c"}]' --output-format html
tests/test.sh xmlw-json -e '[1,2,3,{"a": 123,"b":"c"}]' --output-format xml-wrapped
tests/test.sh jsonw-json -e '[1,2,3,{"a": 123,"b":"c"}]' --output-format json-wrapped
tests/test.sh bash-json -e '[1,2,3,{"a": 123,"b":"c"}]' --output-format bash


tests/test.sh bash-escape1 --xquery '"1&#xA;2"' --output-format bash
tests/test.sh bash-escape2 --xquery '"1&#xD;2"' --output-format bash
tests/test.sh bash-escape3 --xquery "concat('\"', \"'\", '\\\\')" --output-format bash
tests/test.sh bash-escape4 --xquery "concat('\"', \"'\", '\\\\', '&#xA;')" --output-format bash
tests/test.sh bash-escape5 --xquery "concat('\"', \"'\", '\\\\', '&#xD;')" --output-format bash
tests/test.sh bash-escape6 --xquery "concat('\"', \"'\", '\\\\', '&#xA;&#xD;')" --output-format bash
tests/test.sh bash-escape7 --xquery "concat('\"', \"'\", '\\\\')" --output-format bash --print-type-annotations
tests/test.sh bash-escape8 --xquery "concat('\"', \"'\", '\\\\', '&#xA;&#xD;')" --output-format bash --print-type-annotations

tests/test.sh bash-combining1 -e 1 -e '(2,3)' -e '4' --output-format bash
tests/test.sh bash-combining2 -e 1 -e '(2,3)' -e 'temp:=712' -e '4' --output-format bash
tests/test.sh bash-combining3 -e 1 -e '(2,3)' -e 'temp:=712' -e '4' -e 'temp:=189' --output-format bash

#Nesting
tests/test.sh nest0a [ ]
tests/test.sh nest0b '<empty/>'   [ ]
tests/test.sh nest0c tests/a.xml  [ ]
tests/test.sh nest1a tests/a.xml  [ -e //title ]
tests/test.sh nest1b tests/b.xml  [ -e //title ]
tests/test.sh nest1ab tests/a.xml  tests/b.xml [ -e //title ]
tests/test.sh nest1ab2 tests/a.xml  tests/b.xml -e //title [ -e //title ] 
tests/test.sh nest1ab3 tests/a.xml  tests/b.xml [ -e //title ] -e //title
tests/test.sh nest1ab4 tests/a.xml  tests/b.xml -e 'concat("X", //title)' [ -e //title ] 
tests/test.sh nest1ab5 tests/a.xml  tests/b.xml [ -e //title ] -e 'concat("Y", //title)'
tests/test.sh nest2a tests/a.xml  [ -f //a -e //title ]
tests/test.sh nest2a tests/a.xml  -f //a [ -e //title ]
tests/test.sh nest2b tests/a.xml -e //title [ -f //a -e //title ]
tests/test.sh nest2b tests/a.xml -e //title -f //a [ -e //title ]
tests/test.sh nest2c tests/a.xml  [ -f //a -e //title ] -e //title
tests/test.sh nest2d tests/a.xml -e //title [ -f //a -e //title ] -e //title
tests/test.sh nest2e tests/a.xml  -f //a [ -e //title ] -e //title
tests/test.sh nest3a [ tests/a.xml tests/b.xml ] -e //title
tests/test.sh nest3a tests/a.xml tests/b.xml -e //title
tests/test.sh nest3a tests/a.xml [ tests/b.xml -e //title ]
tests/test.sh nest3a tests/a.xml [ tests/b.xml [ -e //title ] ]
tests/test.sh nest3a -e //title tests/a.xml tests/b.xml
tests/test.sh nest3a -e //title [ tests/a.xml tests/b.xml ] 
tests/test.sh nest3a -e //title tests/a.xml [ tests/b.xml ]  
tests/test.sh nest3a tests/a.xml -e //title  [ tests/b.xml ]  #brackets prevent sibling creation (good??)
tests/test.sh nest3b tests/a.xml -e //title  tests/b.xml 
tests/test.sh nest4 -e 1+2
tests/test.sh nest4 [ -e 1+2 ]
tests/test.sh nest4 [ [ -e 1+2 ] ]
tests/test.sh nest4 [ [ [ -e 1+2 ] ] ] 
tests/test.sh nest5a [ "<a/>"  -e "a:=17" ] [  tests/a.xml -e 'a:=909'  ]
tests/test.sh nest5b [ "<a/>"  -e "a:=17" ] [  tests/a.xml -e 'a:=909'  ] [ -e '$a' ] 
tests/test.sh nest5c -e "a:=17"  tests/a.xml -e '<a>{z:=$a + 1}</a>'
tests/test.sh nest6a [ -e 1+2 ] 
tests/test.sh nest6b [ -e 1+2 ] [ -e 3+4 ]
tests/test.sh nest6c [ -e 1+2 ] [ -e 3+4 ] [ -e 5+6 ]
tests/test.sh nest7 [ tests/a.xml -f //a     -e //title ] [ tests/dpre.xml -f //a     -e //title ]
tests/test.sh nest8 tests/a.xml [ -f //a     -e //title   tests/dpre.xml -f //a     -e //title ]
tests/test.sh nest8b tests/a.xml   -f //a  [  -e //title   tests/dpre.xml -f //a     -e //title ]
tests/test.sh nest9a tests/a.xml -f //a -e //title -f //a -e //title
tests/test.sh nest9b tests/a.xml [ -f //a -e //title -f //a ] -e //title
tests/test.sh nest9c tests/a.xml [ -f //a -e //title -f //a -e //title ] -e //title
tests/test.sh nest10 tests/a.xml [ -e //title -f //a -e //title ] 
tests/test.sh nest10 tests/a.xml -e //title [ -f //a -e //title ] 
tests/test.sh nest10 [ tests/a.xml ] -e //title -f //a -e //title 
tests/test.sh nest10 [ tests/a.xml  -e //title -f //a ] -e //title 
tests/test.sh nest10b [ tests/a.xml  -e //title -f //a ] -e 'concat(//title, "x")'
tests/test.sh nest10 [ tests/a.xml  -e //title ] -f //a  -e //title 
tests/test.sh nest10b [ tests/a.xml  -e //title ] -f //a -e 'concat(//title, "x")'
tests/test.sh nest10b [ tests/a.xml ] -e //title  -f //a -e 'concat(//title, "x")'
tests/test.sh nest10b tests/a.xml [ -e //title ] -f //a -e 'concat(//title, "x")'
tests/test.sh nest10c tests/a.xml [ -e //title  -f //a ] -e 'concat(//title, "x")' # this blocks the follow to ascend to the parent (good ? yielding becomes confusing if there are many nested blocks yielding to each other (and it would still apply to a.xml) )

#type selection
tests/test.sh css '<a>hallo<b>cc</b></a>' --css b
tests/test.sh xpath1 '<a>hallo<b>cc</b></a>' --xpath b
tests/test.sh xpath2 '<a>hallo<b>cc</b></a>' --xpath //b
tests/test.sh xpath3 --xpath "'&gt;'"
tests/test.sh xpath3 -e "'&gt;'"
tests/test.sh xquery --xquery "'&gt;'"
tests/test.sh xquerypath --xquery "'&gt;'" -e "'&gt;'"

tests/test.sh xpath4 '<html>1<a class="foobar">2</a>3</html>' -e 'html'
tests/test.sh xpath5 '<html>1<a class="foobar">2</a>3</html>' -e 'a'    #make this CSS??
tests/test.sh xpath6 '<html>1<a class="foobar">2</a>3</html>' -e '//a'
tests/test.sh xpath6 '<html>1<a class="foobar">2</a>3</html>' -e '    //a   '
tests/test.sh css2 '<html>1<a class="foobar">2</a>3</html>' -e 'a.foobar'
tests/test.sh css2 '<html>1<a class="foobar">2</a>3</html>' -e '   a.foobar   '
tests/test.sh xquery4 '<html>1<a class="foobar">2</a>3</html>' -e '   let    $x := //a return $x' #xpath2 now
tests/test.sh xquery5 '<html>1<a class="foobar">2</a>3</html>' -e '   for tumbling window $x in //a start when true() return "&gt;"'
tests/test.sh xquery5 '<html>1<a class="foobar">2</a>3</html>' -e '    for sliding window $x in //a start when true() end when true() return "&gt;"'
tests/test.sh xpath7 '<html>1<a class="foobar">2</a>3</html>' -e '"&gt;"'
tests/test.sh xpath7 '<html>1<a class="foobar">2</a>3</html>' -e '     "&gt;"'
tests/test.sh template '<html>1<a class="foobar">2</a>3</html>' -e '<a class="foobar">{.}</a>'
tests/test.sh xquery6 -e '   declare     function local:abc(){"&gt;"}; local:abc()'
tests/test.sh xquery6 -e '   declare     function local:abc($arg as xs:string){"&gt;"}; local:abc("foo")'
tests/test.sh xquery6 -e '   declare variable $xyz := "a&gt;b"; substring($xyz,2,1)'
tests/test.sh xquery6 -e '  xquery version "1.0"; "&gt;"'
tests/test.sh xpath8 '<a>3</a>' -e ' 3 + . '
tests/test.sh xpath8 '<a>3</a>' -e ' . + 3 '
tests/test.sh xpath9 '<a>3</a>' -e ' . '
tests/test.sh xpath10 '<a>3</a>' -e ' . eq . '
 
#magic variables
tests/test.sh varraw '<a>3</a>' -e '$raw'
tests/test.sh varurlhostpath 'http://videlibri.sourceforge.net/xidelecho.php' -e 'concat($url, "||", $host, "||", $path)'
tests/test.sh varresult '<a>3</a>' -e '.'  -e 'concat("-", $result, "-")'

#other stuff

tests/test.sh system -e 'system("echo 123") * 8'
echo 101 | tests/test.sh read -e 'read() * 8' --strict-type-checking
echo '{"a": 1, "b": 2}' |  tests/test.sh jsonassign - -e '($json).a := 10'
tests/test.sh namespace1 '<c xmlns="foobar">def</c>' -e / --printed-node-format xml
tests/test.sh namespace2 '<c xmlns="foobar">def</c>' -e / --printed-node-format xml --ignore-namespaces
tests/test.sh repetitionoff tests/a.xml tests/a.xml -e //title
tests/test.sh repetitionon tests/a.xml tests/a.xml -e //title --allow-repetitions
tests/test.sh inputformatAutoA --input-format auto '<a>x</a>'  -e 'outer-xml(/)'
tests/test.sh inputformatAutoC --input-format auto '<c>x</c>'  -e 'outer-xml(/)'
tests/test.sh inputformatXml --input-format xml '<a>x</a>'  -e 'outer-xml(/)'
tests/test.sh inputformatHtml --input-format html '<a>x</a>'  -e 'outer-xml(/)'
tests/test.sh inputformatAutoA '<a>x</a>' --input-format auto  -e 'outer-xml(/)'
tests/test.sh inputformatXml  '<a>x</a>' --input-format xml -e 'outer-xml(/)'
tests/test.sh inputformatHtml  '<a>x</a>' --input-format html  -e 'outer-xml(/)'
tests/test.sh inputformatAutoA '<a>x</a>'  -e 'outer-xml(/)' --input-format auto
tests/test.sh inputformatAutoC '<c>x</c>'  -e 'outer-xml(/)' --input-format auto
tests/test.sh inputformatXml '<a>x</a>'  -e 'outer-xml(/)' --input-format xml
tests/test.sh inputformatHtml  '<a>x</a>'  -e 'outer-xml(/)' --input-format html
tests/test.sh inputformatAutoJson tests/data.json  -e '(($json).b, (.).c, .("d"), . ! e, . / f, . // property)' 
tests/test.sh optadhoc '<a>x</a>'  -e /
tests/test.sh optxml --xml '<a>x</a>'  -e /
tests/test.sh opthtml --html '<a>x</a>'  -e /
tests/test.sh moduleVars -e 'declare variable $a:=123; ()' -e '$a'
tests/test.sh moduleFunc1 -e 'declare variable $a:=123; declare function local:xyz(){456}; 8' -e 'local:xyz()+$a'
tests/test.sh moduleFunc2 -e 'declare variable $a:=123; declare function local:xyz(){456}; ()' -e 'declare function local:abc(){$a*1000}; local:xyz() + local:abc()'
tests/test.sh moduleFuncImport -e 'import module namespace foobar = "pseudo://test-module" at "tests/module.xq"; ()' -e '$foobar:abc'
tests/test.sh moduleFuncImport2 -e 'import module namespace rename = "pseudo://test-module" at "tests/module.xq"; ()' -e 'rename:test()'

#interpreter tests
tests/test.sh utf8  -e 'substring("äbcd",1,3)'
tests/test.sh division --xquery 'let $n := ("-INF", "-1", "-0", "NaN", "0", "1", "INF") for $a in $n return string-join( $n ! ( $a div .), " ")'
tests/test.sh multiplication --xquery 'let $n := ("-INF", "-1", "-0", "NaN", "0", "1", "INF") for $a in $n return string-join( $n ! ( $a * .), " ")'
tests/test.sh addition --xquery 'let $n := ("-INF", "-1", "-0", "NaN", "0", "1", "INF") for $a in $n return string-join( $n ! ( $a + .), " ")'
tests/test.sh subtraction --xquery 'let $n := ("-INF", "-1", "-0", "NaN", "0", "1", "INF") for $a in $n return string-join( $n ! ( $a - .), " ")'
tests/test.sh divisionInt --xquery 'let $n := ("-INF", "-1", "-0", "NaN", "0", "1", "INF") for $a in $n return string-join( $n ! ( try { ($a) idiv xs:double(.) } catch *  {$Q{http://www.w3.org/2005/xqt-errors}code} ), " ")'
tests/test.sh modulo --xquery 'let $n := ("-INF", "-1", "-0", "NaN", "0", "1", "INF") for $a in $n return string-join( $n ! ( try { $a mod . } catch *  {$Q{http://www.w3.org/2005/xqt-errors}code} ), " ")'


#parser
tests/test.sh dtd-broken tests/dtd.xml -e /
tests/test.sh dtd-working --input-format xml-strict tests/dtd.xml -e /
tests/test.sh dtd-working  '<e/>' -e '()' --input-format xml-strict tests/dtd.xml -e / #without  the -e () the last -e applies to both inputs
tests/test.sh dtd-working --input-format html '<e>'  --input-format xml-strict  -f "'tests/dtd.xml'"  -e /
tests/test.sh dtd-working --input-format html '<e>'  -f '{"data": "tests/dtd.xml", "input-format": "xml-strict"}'  -e /
tests/test.sh dtd-working --input-format html '<e>'  -f '{"data": "tests/dtd.xml", "input-format": "xml-strict", "xx--yy--non-pure-caße!": 1}'  -e /

tests/test.sh pinode-xml tests/weirdpinode.xml -e / --output-format xml
tests/test.sh pinode-xml --input-format xml tests/weirdpinode.xml -e / --output-format xml
tests/test.sh pinode-xml-strict --input-format xml-strict tests/weirdpinode.xml -e / --output-format xml
tests/test.sh pinode-html --input-format html tests/weirdpinode.xml -e / --output-format xml #this is nonsense. gigo


#Online tests
tests/test.sh google http://www.google.de -e "count(//title[contains(text(),\"Google\")])"
tests/test.sh get1  http://videlibri.sourceforge.net/xidelecho.php -e //meth
#disallowed by server?: tests/test.sh get2a --post abc --method GET http://videlibri.sourceforge.net/xidelecho.php -e //meth
#disallowed by server?: tests/test.sh get2b --post abc --method GET http://videlibri.sourceforge.net/xidelecho.php -e //raw 
tests/test.sh post1a  --post test http://videlibri.sourceforge.net/xidelecho.php -e //meth
tests/test.sh post1b  --post test http://videlibri.sourceforge.net/xidelecho.php -e //raw
tests/test.sh post2  --post "user=login&pass=password" http://videlibri.sourceforge.net/xidelecho.php -e //raw
tests/test.sh post3  --post "" http://videlibri.sourceforge.net/xidelecho.php -e //meth
tests/test.sh post3b  --post "" http://videlibri.sourceforge.net/xidelecho.php -e //raw
tests/test.sh post3c  --post "" http://videlibri.sourceforge.net/xidelecho.php --download -
tests/test.sh post4  --post "123" http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)' --post "456" http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)'
tests/test.sh post4b  --post "123" http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)'  http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)'  #duplicated requests are ignored
tests/test.sh post4c  --post "123" http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)' --method PUT http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)' #keep the data option
tests/test.sh post4d [  --post "123" http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)' ] --method GET http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)' 
tests/test.sh post4d [  --post "123" http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)' ]  http://videlibri.sourceforge.net/xidelecho.php -e '(//meth,//raw)' 
echo TEST | tests/test.sh post5  --post - http://videlibri.sourceforge.net/xidelecho.php -e //raw

tests/test.sh post6 '<foo>bar</foo>' -e 'v:=/foo' --post 'data={$v}'  http://videlibri.sourceforge.net/xidelecho.php -e //raw
tests/test.sh post6 '<x><foo>bar</foo><raw>OH</raw></x>' -e 'v:=//foo' --post 'data={$v}'  http://videlibri.sourceforge.net/xidelecho.php -e //raw 
tests/test.sh post6b '<x><foo>bar</foo><raw>OH</raw></x>' -e 'v:=//foo' [ --post 'data={$v}'  http://videlibri.sourceforge.net/xidelecho.php -e //raw ] # [ causes it to process both data. Does not make much sense, but is logical
tests/test.sh post6c '<x><foo>bar</foo><raw>OH</raw></x>' -e 'v:=//foo' [ -e "" --post 'data={$v}'  http://videlibri.sourceforge.net/xidelecho.php -e //raw ] 

tests/test.sh post7 -d "user=login" [ -d "&pass=password" http://videlibri.sourceforge.net/xidelecho.php -e / ]  [ -d "a=b" http://videlibri.sourceforge.net/xidelecho.php -e /  ]
tests/test.sh post8 -d "a=1" [ -d "" -d "b=2" http://videlibri.sourceforge.net/xidelecho.php -e / ]  [ -d "c=3" http://videlibri.sourceforge.net/xidelecho.php -e /  ] -d d=4 http://videlibri.sourceforge.net/xidelecho.php -e / -d e=5 http://videlibri.sourceforge.net/xidelecho.php -e /


tests/test.sh put1a  --method=PUT --post test http://videlibri.sourceforge.net/xidelecho.php -e //meth
tests/test.sh put1a  --method=POST --post test --method=PUT http://videlibri.sourceforge.net/xidelecho.php -e //meth    #override last
tests/test.sh put1b  --method=POST --post test --method=PUT http://videlibri.sourceforge.net/xidelecho.php -e //raw
tests/test.sh foobarmeth --method delete  http://videlibri.sourceforge.net/xidelecho.php -e //meth
echo options | tests/test.sh foobarmeth2 --method -  http://videlibri.sourceforge.net/xidelecho.php -e //meth

tests/test.sh multipart1 --form a=b http://videlibri.sourceforge.net/cgi-bin/xidelecho.pl -e //raw -e "'CT:'" -e //env/CONTENT_TYPE
tests/test.sh multipart2 --form 'a=b&c=d' --form e=f --form "f=@tests/output/post1a" --form "g=@tests/output/post1a;type=foo/bar" http://videlibri.sourceforge.net/cgi-bin/xidelecho.pl -e //raw 


tests/test.sh multipageonline --extract '<action><variable name="obj">{"url": "http://videlibri.sourceforge.net/xidelecho.php", "method": "PUT"}</variable><page url="{$obj}"><template><meth>{.}</meth></template></page></action>' --extract-kind=multipage
tests/test.sh multipageonline2 --extract '<action><variable name="obj">{"url": "http://videlibri.sourceforge.net/xidelecho.php", "method": "PUT", "post": "foobar&123"}</variable><page url="{$obj}"><template><raw>{outer-xml(.)}</raw></template></page></action>' --extract-kind=multipage

tests/test.sh regression_doconline --xquery '<a/> / doc("http://videlibri.sourceforge.net/xidelecho.php") // meth'
tests/test.sh regression_doclocal --xquery '<a/> / doc("tests/a.xml") // title'
tests/test.sh regression_doclocal --xquery 'doc("tests/a.xml") // title'

#Regressions tests for bugs that have been fixed and should not appear again
tests/test.sh regression_text1a '<r><a>1</a><a>2</a></r>' -e '<r><a>{text()}</a></r>'
tests/test.sh regression_text1b '<r><a>1</a><a>2</a></r>' -e '<r><a>{following-sibling::a/text()}</a></r>'
tests/test.sh regression_text1c '<r><a>1</a><a>2</a></r>' -e '<r><a>{following-sibling::a/(text())}</a></r>'
tests/test.sh regression_text1d '<r><a>1</a><a>2</a></r>' -e '<r><a>{following-sibling::a/concat("-",text(),"-")}</a></r>'
tests/test.sh regression_text1e '<a>1</a>' -f '<a>{object(("data", "&lt;a>2&lt;/a>"))}</a>' -e '/html/body/a/concat(">",text(),"<")'

tests/test.sh regression_entity1a '<a>&amp;</a>' -e //a
tests/test.sh regression_entity1b '<a>&amp;amp;</a>' -e //a
tests/test.sh regression_entity1c '<a>&amp;amp;amp;</a>' -e //a
tests/test.sh regression_entity2 -e '"&amp;"'
tests/test.sh regression_entity3a '<a>x</a>' -e '<a>{res := "&amp;"}</a>'
tests/test.sh regression_entity3b '<a>x</a>' -e '<a>{res := "&amp;amp;"}</a>'
tests/test.sh regression_entity3c '<a>x</a>' -e '<a>{res := "&amp;amp;amp;"}</a>'
tests/test.sh regression_entity3d '<a>x</a>' -e '<a>{res := "&amp;amp;amp;amp;"}</a>'

tests/test.sh regression_object1 -e '$x := object(("b","c"))' -e '($x).b' 
tests/test.sh regression_object1 -e '$x := {"b": "c"}' -e '($x).b'
tests/test.sh regression_object2 -e '$x := object(("b","c"))' -e '($x).a' #allow accessing undefined properties
tests/test.sh regression_object2 -e '$x := {"b": "c"}' -e '($x).a' 
tests/test.sh regression_object2 --dot-notation=on -e '$x := {"b": "c"}' -e '$x.a' 

tests/test.sh regression_multipage1  -e "<action><page url=\"tests/a.xml\"><template><title>{concat(., \"'\", '\"')}</title></template></page></action>" --extract-kind=multipage
tests/test.sh regression_multipage2 -e "<action><page url=\"tests/a.xml\"><template><title><t:read var=\"res\" source=\" concat(., &quot;'&quot;, '&quot;')\"/></title></template></page></action>" --extract-kind=multipage
tests/test.sh regression_multipage3 -e '<action><page url="http://example.org"><template><title>{resolve-uri("b.xml")}</title></template></page></action>' --extract-kind=multipage
tests/test.sh regression_multipage3b -e '<action><page url="http://videlibri.sourceforge.net/test/reddit/index.html"><template><head>{resolve-uri("../b.xml")}</head></template></page></action>' --extract-kind=multipage

tests/test.sh regression_htmlparse '<ol><li>a<li>b<li>c</ol>' -e '/ol/li'
