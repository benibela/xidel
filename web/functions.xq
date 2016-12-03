xquery version "3.0-xidel";
declare function colorize-args($args){
  if (not(starts-with($args, "("))) then ("(", <i>{$args}</i>, ")")
  else extract($args, "[$][^ ]+|,|[-a-zA-Z:0-9]+([(][-a-zA-Z:{}/.0-9*]*[)])?[?+*]?|  *|.", 0, "*") ! (
    if (starts-with(., "$")) then <span class="var">{.}</span>
    else if (. = "as") then <span class="keyword">{.}</span>
    else if (matches(., "[-a-zA-Z]")) then <span class="type">{.}</span>
    else if (. = ",") then ", "
    else .
  )
};
<html>
<title>Xidel / Internet Tools function list</title>
<style>{" 
  p { padding-left: 4em } 
  .content { padding-left: 4em; padding-top:1em } 
  code.f { margin-left: 1em; display: block; padding-top: 2em } 
  code.f2 { margin-left: 1em; display: block} 
  span.var { color: green }
  span.keyword { text-weight: bold; color: blue}
  span.type { font-style: italic; color: red}
  span.model30 { padding-left: 4em}
"}</style>
<body>	
<h1>Xidel / Internet Tools function list</h1>Complete alphabetical list grouped for modules:{
let $extensions := doc("functions-x.xml")/functions/f, $extension-content := function($f) { 
  let $data := $extensions[@name = $f/@name]
  return if (exists($data)) then (
    if ($data/@args) then <code class="f2">{<b>{(if (contains($f/@m, "pxp/extensions")) then "pxp:" else "x:") ||$f/@name}</b>, colorize-args(normalize-space($data/@args))}</code> else (),
    <div class="content">{$data/node()}</div> 
  ) else ()
}
let $jsoniqurl := "http://www.jsoniq.org/docs/JSONiqExtensionToXQuery/html-single/index.html"
let $jsoniq := (if (not(file:exists("jsoniq.html")) ) then file:write-text("jsoniq.html", unparsed-text($jsoniqurl)) else (), doc("jsoniq.html")/css(".toc a"))
let $modules := {
  "http://expath.org/ns/file": {"order": 7, "name": "EXPath Module File", "prefix": "file", 
    "content": function($f) {
      let $name := $f/@name
      return <p>See <a href="http://expath.org/spec/file#{if (ends-with($name, "separator") or $name = ("temp-dir", "base-dir", "current-dir")) then "pr" else "fn"}.{$name}" rel="nofollow">EXPath specification</a> </p> }},

  "http://jsoniq.org/function-library": {"order": 3, "name": "JSONiq library functions", "prefix": "jnlib", 
    "content": function($f){
      let $prefixname := "libjn:" || $f/@name return
      <p>See <a href="{$jsoniqurl}{$jsoniq[contains(., $prefixname)][1]/@href}" rel="nofollow">JSONiq reference</a> </p> 
  }},
  "http://jsoniq.org/functions": {"order": 2, "name": "JSONiq base functions", "prefix": "jn", "content": function($f){
      let $prefixname := "jn:" || $f/@name return
      <p>See <a href="{$jsoniqurl}{$jsoniq[contains(., $prefixname)][1]/@href}" rel="nofollow">JSONiq reference</a> </p> 
  }},
  
  ".benibela.de": {"order": 14, "name": "Extension functions (primary)", "prefix": ("pxp", "x"), "content": $extension-content},
  "http://www.benibela.de/2012/pxp/extensions": {"order": 15, "name": "Extension functions in old namespace", "prefix": "pxp", "content": $extension-content},
  "http://pxp.benibela.de": {"order": 16, "name": "Extension functions (secondary)", "prefix": "x", "content": $extension-content},
  
  "http://www.w3.org/2005/xpath-functions": {"order": 0, "name": "Standard XPath/XQuery functions", "prefix": "fn", 
    "content": function($f) { <p>See <a href="https://www.w3.org/TR/xpath-functions-30/#func-{$f/@name}" rel="nofollow">function reference</a> </p> }},
  "http://www.w3.org/2005/xpath-functions/math": {"order": 1, "name": "Standard XPath/XQuery mathematical functions", "prefix": "math", 
    "content": function($f) { <p>See <a href="https://www.w3.org/TR/xpath-functions-30/#func-math-{$f/@name}" rel="nofollow">math function reference</a> </p> }}
}
let $functions := /f
return (
<ul>{
for $f in $functions
let $m := $modules($f/@m)
group by $module-order := $m("order")
order by $module-order
let $m := $m[1]
let $prefix := $m("prefix")
let $prefixid := string-join($prefix)
return <li><a href="#module{$prefixid}">{$m("name"),  join($m("prefix"), ":*, ") || ":*" }</a><ul>
 { for $f in $f group by $a := substring($f/@name, 1, 1) return <li>{ for $n in distinct-values( $f/@name) order by $n return (<a href="#{$prefix[last()]}-{$n}">{$n}</a>, " ")}</li> }
</ul></li>}</ul>,

"A partial list grouped for certain topics: ",
<ul>{
for $cat in (
  {"name": "Elementary/sequence functions", "funcs": "count deep-equal distinct-values empty exists head index-of insert-before position remove reverse subsequence tail x:join"},
  {"name": "Data loading functions", "funcs": "doc parse-xml parse-xml-fragment unparsed-text unparsed-text-available unparsed-text-lines x:request x:json x:parse-html"},
  {"name": "XML/HTML processing functions", "funcs": "base-uri id local-name name data number root string x:css x:inner-html x:inner-xml x:outer-html x:outer-xml x:form x:resolve-html x:transform"},
  {"name": "Basic string functions", "funcs": "codepoints-to-string contains ends-with lower-case normalize-space normalize-unicode starts-with string-join string-length string-to-codepoints substring substring-after substring-before translate upper-case x:cps"},
  {"name": "Regular expressions", "funcs": "analyze-string matches replace tokenize x:extract"},
  {"name": "Boolean functions", "funcs": "false not true"},
  {"name": "Numeric functions", "funcs": "abs avg ceiling floor format-integer format-number max min round round-half-to-even sum math:acos math:asin math:atan math:atan2 math:cos math:exp math:exp10    math:log math:log10 math:pi math:pow math:sin math:sqrt math:tan x:integer x:integer-to-base x:product"},
  {"name": "Environment functions", "funcs": "available-environment-variables  environment-variable x:argc x:argv x:system x:read"},
  {"name": "URI encoding functions", "funcs": "encode-for-uri escape-html-uri iri-to-uri x:uri-decode x:uri-encode"},
  {"name": "Higher order functions", "funcs": "filter fold-left fold-right for-each for-each-pair "},
  {"name": "Date time functions", "funcs": "adjust-date-to-timezone adjust-dateTime-to-timezone adjust-time-to-timezone current-date current-dateTime current-time dateTime day-from-date day-from-dateTime days-from-duration format-date format-dateTime format-time hours-from-dateTime hours-from-duration hours-from-time minutes-from-dateTime minutes-from-duration minutes-from-time month-from-date month-from-dateTime months-from-duration seconds-from-dateTime seconds-from-duration seconds-from-time year-from-date year-from-dateTime years-from-duration"}
) return 
<li><b>{$cat("name")}</b>: {$cat("funcs")!tokenize(., " ")[.]!(<a href="#{if (contains(., ":")) then replace(., ":", "-") else "fn-" || . }">{.}</a>, " ")}</li>
}<li>JSON and file functions: See corresponding module above</li></ul>,


for $f in $functions
let $module-info := $modules($f/@m)
group by $module-order := $module-info("order")
order by $module-order
let $prefix := $module-info[1]("prefix")
let $prefixid := string-join($prefix)
return (
  <h2 id="module{$prefixid}">{$module-info[1]("name") || "  " || join($prefix, ":*, ") || ":*"}</h2>,
  "Namespace: "||$f[1]/@m/string(),
  if ($f[1]/@m/string() = ".benibela.de" ) then (<br/>,"Identical copies of these functions are available in namespace x:* and namespace pxp:*.") else (),
  if ($prefix = ("fn", "pxp")) then (<br/>, "Prefix ", $prefix[. != "x"] , " can be omitted.") else (),
  for $f in $f group by $name := $f/@name/string() order by $name return (
(:    <h3>{$name}</h3>,:)
    <code class="f" id="{$prefix[last()]}-{$f[1]/@name}">{ $f/@args!(<b>{$prefix[last()] || ":" ||$name}</b>, colorize-args(normalize-space()), if (../@version = "3.0") then <span class="model30">(: XPath/XQuery 3.0 only :)</span> else () , <br/>)}</code>,
    $module-info[1]("content")($f[1])
  )
  
))
}</body></html> 