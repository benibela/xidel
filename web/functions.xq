xquery version "3.0-xidel";
declare function colorize-args($args){
  if (not(starts-with($args, "("))) then ("(", <i>{replace($args, "65535", "unlimited")}</i>, ")")
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
<style>{'
  p { padding-left: 4em } 
  .content { padding-left: 4em; padding-top:1em } 
  .deprecated { text-decoration: line-through; padding-top: 3em }
  .deprecated::before { content: "Deprecated. Do not use."; text-decoration: none; display: inline-block; position: relative; top: -2em; height: 0px; width: 0px; overflow-wrap: unset; white-space: nowrap; overflow: visible}
  .internal { text-decoration: line-through; padding-top: 3em }
  .internal::before { content: "Internal. Do not use."; text-decoration: none; display: inline-block; position: relative; top: -2em; height: 0px; width: 0px; overflow-wrap: unset; white-space: nowrap; overflow: visible}
    
  code.f { margin-left: 1em; display: block; padding-top: 2em } 
  code.f2 { margin-left: 1em; display: block} 
  span.var { color: green }
  span.keyword { text-weight: bold; color: blue}
  span.type { font-style: italic; color: red}
  span.model30 { padding-left: 4em}
  
  ul.modlist ul { padding-bottom: 1em; padding-top: 0.3em  }
  ul.topics li { padding-bottom: 0.75em }
  ul.topics li b { line-height: 1.5em }
  ul.topics li a, ul.modlist li a { margin-left: 6px }
'}</style>
<body>	
<h1>Xidel / Internet Tools function list</h1>{
let $extensions := {|for $module in doc("functions-x.xml")/functions/module return tokenize($module/@prefixes, " ")[.]!{.:$module/f}|}
let $custom-content := function($prefix, $f) { 
  let $data := $extensions($prefix)[@name = $f/@name]
  return if (exists($data)) then (
    if ($data/@args) then <code class="f2">{<b>{(if (contains($f/@m, "pxp/extensions")) then "pxp:" else "x:") ||$f/@name}</b>, colorize-args(normalize-space($data/@args))}</code> else (),
    <div class="content{$data/(@deprecated,@internal)/concat(" ", name())}">{$data/node()!x:transform(., function($e){
      if (name($e) = "a" and empty($e/@*)) then <a href="#{replace($e, ":", "-")}">{$e/node()}</a>
      else $e
    })}</div> 
  ) else ()
}
let $jsoniqurl := "http://www.jsoniq.org/docs/JSONiqExtensionToXQuery/html-single/index.html"
let $jsoniq := (if (not(file:exists("jsoniq.html")) ) then file:write-text("jsoniq.html", unparsed-text($jsoniqurl)) else (), doc("jsoniq.html"))
let $jsoniq-toc := $jsoniq/css(".toc a")
let $jsoniq-targets := {| $jsoniq//a[@id]/{@id: .}  |}
let $jsoniq-content := function($prefixname) {
  let $id := $jsoniq-toc[contains(., $prefixname)][1]/@href
  return 
    <p>{if ($id) then ( $jsoniq-targets(substring-after($id, "#"))/ancestor::div[@class="titlepage"]/following-sibling::div[1]/node()) else ()}
       See <a href="{$jsoniqurl}{$id}" rel="nofollow">JSONiq reference</a>.</p> 
}
let $modules := {
  "http://expath.org/ns/file": {"order": 17, "name": "EXPath Module File", "prefix": "file", "unit": "xquery_module_file",
    "content": function($f) {
      let $name := $f/@name
      return <p>See <a href="http://expath.org/spec/file#{if (ends-with($name, "separator") or $name = ("temp-dir", "base-dir", "current-dir")) then "pr" else "fn"}.{$name}" rel="nofollow">EXPath specification</a>.</p> }},

  "http://jsoniq.org/functions": {"order": 12, "name": "JSONiq base functions", "prefix": "jn", "unit": "xquery_json", "content": function($f){ $jsoniq-content("jn:" || $f/@name) }},
  "http://jsoniq.org/function-library": {"order": 13, "name": "JSONiq library functions", "prefix": "jnlib", "unit": "xquery_json", 
    "content": function($f){ $jsoniq-content("libjn:" || $f/@name)  }},

  ".benibela.de": {"order": 24, "name": "Extension functions (primary)", "prefix": ("pxp", "x")},
  "http://www.benibela.de/2012/pxp/extensions": {"order": 25, "name": "Extension functions in old namespace", "prefix": "pxp"},
  "http://pxp.benibela.de": {"order": 26, "name": "Extension functions (secondary)", "prefix": "x"},
  
  "http://www.w3.org/2005/xpath-functions": {"order": 0, "name": "Standard XPath/XQuery functions", "prefix": "fn", 
    "content": function($f) { <p>See XPath/XQuery <a href="https://www.w3.org/TR/xpath-functions-30/#func-{$f/@name}" rel="nofollow">function reference</a>.</p> }},
  "http://www.w3.org/2005/xpath-functions/math": {"order": 1, "name": "Standard XPath/XQuery mathematical functions", "prefix": "math", "unit": "xquery_module_math", 
    "content": function($f) { <p>See <a href="https://www.w3.org/TR/xpath-functions-30/#func-math-{$f/@name}" rel="nofollow">math function reference</a>.</p> }},
  "http://www.w3.org/2001/XMLSchema": {"order": 9, "name": "Standard type constructors", "prefix": "xs",
    "content": function($f) { <p>Casts a value to the XML Schema  <a href="https://www.w3.org/TR/xmlschema11-2/#{$f/@name}" rel="nofollow">type {$f/@name/string()}</a>.</p> }}
}
let $functions := /f
return (
"A partial list grouped by certain topics: ",
<ul class="topics">{
for $cat in (
  {"name": "Elementary/sequence functions", "funcs": "count deep-equal distinct-values empty exists head index-of insert-before position remove reverse subsequence tail x:join"},
  {"name": "Data loading functions", "funcs": "doc parse-xml parse-xml-fragment unparsed-text unparsed-text-available unparsed-text-lines file:read-binary file:read-text x:request x:json x:parse-html"},
  {"name": "XML/HTML query functions", "funcs": "base-uri id local-name name data number root string x:css x:inner-html x:inner-xml x:outer-html x:outer-xml x:form x:resolve-html x:transform"},
  {"name": "Basic string functions", "funcs": "codepoints-to-string contains ends-with lower-case normalize-space normalize-unicode starts-with string-join string-length string-to-codepoints substring substring-after substring-before translate upper-case x:cps"},
  {"name": "Regular expressions", "funcs": "analyze-string matches replace tokenize x:extract"},
  {"name": "Boolean functions", "funcs": "false not true"},
  {"name": "Mathematical functions", "funcs": "abs avg ceiling floor format-integer format-number max min round round-half-to-even sum math:acos math:asin math:atan math:atan2 math:cos math:exp math:exp10    math:log math:log10 math:pi math:pow math:sin math:sqrt math:tan x:integer x:integer-to-base x:product"},
  {"name": "Environment functions", "funcs": "available-environment-variables  environment-variable x:argc x:argv x:system x:read"},
  {"name": "URI encoding functions", "funcs": "encode-for-uri escape-html-uri iri-to-uri resolve-uri file:resolve-path x:uri-decode x:uri-encode x:form x:resolve-html"},
  {"name": "Higher order functions", "funcs": "filter fold-left fold-right for-each for-each-pair x:transform"},
  {"name": "Date time functions", "funcs": "adjust-date-to-timezone adjust-dateTime-to-timezone adjust-time-to-timezone current-date current-dateTime current-time dateTime day-from-date day-from-dateTime days-from-duration format-date format-dateTime format-time hours-from-dateTime hours-from-duration hours-from-time minutes-from-dateTime minutes-from-duration minutes-from-time month-from-date month-from-dateTime months-from-duration seconds-from-dateTime seconds-from-duration seconds-from-time year-from-date year-from-dateTime years-from-duration"}
) return 
<li><b>{$cat("name")}</b>:<br/> {($cat("funcs")!tokenize(., " ")[.]!(<a href="#{if (contains(., ":")) then replace(., ":", "-") else "fn-" || . }">{.}</a>, ", "))[position()!=last()]}</li>
}<li>More JSON and file functions: See corresponding module below</li></ul>,

"Complete alphabetical list grouped by modules:",
<ul class="modlist">{
for $f in $functions
let $m := $modules($f/@m)
group by $module-order := $m("order")
order by $module-order
let $m := $m[1]
let $prefix := $m("prefix")
let $prefixid := string-join($prefix)
return <li><a href="#module{$prefixid}">{$m("name"),  join($m("prefix"), ":*, ") || ":*" }</a><ul>
 { for $f in $f group by $a := substring($f/@name, 1, 1) return <li>{ (for $n in distinct-values( $f/@name) order by $n return (<a href="#{$prefix[last()]}-{$n}">{$n}</a>, ", "))[position()!=last()]}</li> }
</ul></li>}
<li>The <a href="http://www.xqueryfunctions.com/" rel="nofollow">FunctX function library</a> is not included in the release, but can be separately downloaded and then imported in any query. </li>
</ul>,


for $f in $functions
let $module-info := $modules($f/@m)
group by $module-order := $module-info("order")
order by $module-order
let $module :=  $module-info[1]
let $prefix := $module("prefix")
let $prefixid := string-join($prefix)
return (
  <h2 id="module{$prefixid}">{$module("name") || "  " || join($prefix, ":*, ") || ":*"}</h2>,
  "Namespace: "||$f[1]/@m/string(),
  if ($f[1]/@m/string() = ".benibela.de" ) then (<br/>,"Identical copies of these functions are available in namespace x:* and namespace pxp:*.") else (),
  if ($prefix = ("fn", "pxp", "xs")) then (<br/>, "Prefix ", $prefix[. != "x"] , " can be omitted.") else (),
  if (exists($module("unit"))) then (<br/>,x"In the Pascal Internet Tools library the unit {$module("unit")}.pas needs to be loaded, before these functions are available.") else (),
  
  for $f in $f group by $name := $f/@name/string() order by $name return (
(:    <h3>{$name}</h3>,:)
    <code class="f" id="{$prefix[last()]}-{$f[1]/@name}">{ $f/@args!(<b>{$prefix[last()] || ":" ||$name}</b>, colorize-args(normalize-space()), if (../@version = "3.0") then <span class="model30">(: XPath/XQuery 3.0 only :)</span> else () , <br/>)}</code>,
    $custom-content($prefixid, $f[1]),
    $module("content")($f[1])
  ),
  
  if ($prefix = ("jn", "jnlib")) then <span style="font-size: 75%">The JSONiq sections are taken from the JSONiq reference and are again licensed as CC-BY-SA.</span> else ()
  
))
}</body></html> 