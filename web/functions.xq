xquery version "3.1-xidel";
declare variable $path := file:current-dir();
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
declare function cached-doc($url, $cache){
  let $cache := $path || "/cache/"||$cache return (
    if (not(file:exists($cache)) ) then (file:create-dir($path || "/cache/"), file:write-text($cache, replace(unparsed-text($url), 'xmlns="http://www\.w3\.org/1999/xhtml', '' ))) else (), 
    doc($cache)
  )
};
(:declare variable $MARKERXP30 := <span class="model30">(: XPath/XQuery 3.0+ only :)</span> ;
declare variable $MARKERXP31 := <span class="model30">(: XPath/XQuery 3.1 only :)</span> ;:)
declare variable $MARKERXP30 := <span class="model30">XP3.0+</span> ;
declare variable $MARKERXP31 := <span class="model30">XP3.1</span> ;
declare variable $MARKERXidel := <span class="model30">Xidel</span> ;
declare function marker-for-version($version){
  switch ($version)
    case "3.0" return $MARKERXP30
    case "3.1" return $MARKERXP31
    case "xidel" return $MARKERXidel
    default return ()
};
declare function format-description-text($text){
  for $t at $i in tokenize($text, "[`]")
  let $is-code := $i mod 2 = 0
  return if ($is-code) then <code>{$t}</code>
  else let $vars := tokenize($t, "[$]") 
  return ($vars[1], tail($vars) ! (
    <code>${extract(., "^[a-zA-Z0-9]+")}</code>,
    replace(., "^[a-zA-Z0-9]+", "")
  ))
};
<html>
<meta charset="utf-8"/>
<title>Xidel / Internet Tools function list</title>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<style>{'
  p { padding-left: 4em } 
  .content { padding-left: 4em; padding-top:1em; line-height: 1.4;  } 
  .content p{padding-left: 0 }
  div.examples { padding-left: 2em; padding-top: 1em; padding-bottom: 2em}
  .deprecated { text-decoration: line-through; padding-top: 3em }
  .deprecated::before { content: "Deprecated. Do not use."; text-decoration: none; display: inline-block; position: relative; top: -2em; height: 0px; width: 0px; overflow-wrap: unset; white-space: nowrap; overflow: visible}
  .internal { text-decoration: line-through; padding-top: 3em }
  .internal::before { content: "Internal. Do not use."; text-decoration: none; display: inline-block; position: relative; top: -2em; height: 0px; width: 0px; overflow-wrap: unset; white-space: nowrap; overflow: visible}
    
  code.f { margin-left: 1em; display: block; padding-top: 2em } 
  code.f2 { margin-left: 1em; display: block} 
  pre { white-space: pre-wrap;  word-wrap: break-word; word-break: keep-all  } 
  span.var { color: green }
  span.keyword { text-weight: bold; color: blue}
  span.type { font-style: italic; color: red}
  span.model30 { margin-left: 4em; background-color: #DDDDDD; padding: 2px}
  
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
    if ($data/@args) then <code class="f2" id="{replace($prefix,"pxpx","x")}-{$f/@name}">{<b>{(if (contains($f/@m, "pxp/extensions")) then "pxp:" else "x:") ||$f/@name}</b>, colorize-args(normalize-space($data/@args)), marker-for-version(($data/@version, $f/@version)[1])
    }</code> else (),
    <div class="content{$data/(@deprecated,@internal)/concat(" ", name())}">{$data/node()!x:transform(., function($e){
      if (name($e) = "a" and empty($e/@*)) then <a href="#{replace($e, ":", "-")}">{$e/node()}</a>
      else if ($e instance of text() and matches($e, "[$`]")) then format-description-text($e)
      else $e
    })}</div> 
  ) else ()
}

let $argument-names := {| 
  for $temp in (["http://www.w3.org/2005/xpath-functions", "fn"], 
                ["http://www.w3.org/2005/xpath-functions/array", "fn"], 
                ["http://www.w3.org/2005/xpath-functions/map", "fn"], 
                ["http://expath.org/ns/file", "file"]
               ) 
  let $url := $temp?1
  let $suffix := $temp?2
  return 
  {$url: {|for $f in doc("functions-arguments-names-"||$suffix||".xml")/*/f group by $name := $f/@name return {$name: $f ! [ tokenize(@args) ] } |} }
|}
let $merge-args := function($f, $args){
  if (contains($args, ";")) then
    let $args := tokenize($args, ";") ! ( if (not(matches(., "xs:|\(|^$"))) then "xs:"||. else . )
    let $args := if (count($args) = 2 and $args[1] = "") then $args[2] else $args
    let $param-count := count($args) - 1
    let $param-names := $argument-names($f/@m)($f/@name)[array:size(.) = $param-count]
    let $param-names := switch(count($param-names))
       case 0 return [ (1 to $param-count) ! ("arg" || .) ]
       case 1 return $param-names 
       default return 
          if ($param-names => count() = 1) then $param-names 
          else if (ends-with($f/@m, "array") and exists($param-names[contains(?1, "array")])) then
              $param-names[contains(?1, "array")]
          else if (ends-with($f/@m, "map") and exists($param-names[contains(?1, "map")])) then
              $param-names[contains(?1, "map")]
          else $param-names[1]
    let $param-names := $param-names?*
    return "(" || join( for $i in 1 to $param-count return "$" || $param-names[$i] || " as " || $args[$i], ", ") || ") as " || $args[last()] 
  else normalize-space($args)
}

let $xpathspecurl := "https://www.w3.org/TR/xpath-functions-31"
let $xpathspec := cached-doc($xpathspecurl, "xpathfunctions-31.xml")//a[starts-with(@id, "func-")]
let $xpathresolve := function($n) { x:transform($n, function($n){
  if (name($n) = "a" and not(contains($n/@href, "://") )) then <a href="{resolve-uri($n/@href, $xpathspecurl)}">{$n/node()}</a>
  else $n
}) }
let $xpathspecsnippet := function($id, $class) { 
  let $dl := $xpathspec[@id = $id]/../../dl,
      $examples := $dl/dt[. = "Examples"]/following-sibling::dd[1]/node()!$xpathresolve(.)
  return
  <div class="content">{$dl/dd[1]/p[1]/node()!$xpathresolve(.)} <a href="https://www.w3.org/TR/xpath-functions-31/#{$id}" rel="nofollow">More..</a>. {if ($examples) then (<div class="examples"><b>Examples:</b><br/>{$examples}</div>) else ()}</div>
}
let $expathfilespecurl := "http://expath.org/spec/file"
let $expathfile := cached-doc($expathfilespecurl || "/1.0.xml", "expath-file.xml")
let $expathfilefunctions := $expathfile//div2
let $jsoniqurl := "http://www.jsoniq.org/docs/JSONiqExtensionToXQuery/html-single/index.html"
let $jsoniq := cached-doc($jsoniqurl, "jsoniq.html")
let $jsoniq-toc := $jsoniq/css(".toc a")
let $jsoniq-targets := {| $jsoniq//a[@id]/{@id: .}  |}
let $jsoniq-content := function($prefixname) {
  let $id := $jsoniq-toc[contains(., $prefixname)][1]/@href
  return 
    <p>{if ($id) then ( $jsoniq-targets(substring-after($id, "#"))/ancestor::div[@class="titlepage"]/following-sibling::div[1]/node()) else ()}
       <a href="{$jsoniqurl}{$id}" rel="nofollow">More..</a>.</p> 
}

let $modules := {
  "http://www.w3.org/2005/xpath-functions": {"order": 0, "name": "Standard XPath/XQuery functions", "prefix": "fn",   "content": function($f) { $xpathspecsnippet("func-"||$f/@name, "") }, "license": "w3c"},
  "http://www.w3.org/2005/xpath-functions/array": {"order": 1, "name": "Standard XPath/XQuery (JSON) array functions", "prefix": "array",   "content": function($f) { $xpathspecsnippet("func-array-"||$f/@name, "") }, "license": "w3c"},
  "http://www.w3.org/2005/xpath-functions/map": {"order": 2, "name": "Standard XPath/XQuery map / JSON object functions", "prefix": "map",   "content": function($f) { $xpathspecsnippet("func-map-"||$f/@name, "") }, "license": "w3c"},
  "http://www.w3.org/2005/xpath-functions/math": {"order": 5, "name": "Standard XPath/XQuery mathematical functions", "prefix": "math", "unit": "xquery_module_math", "content": function($f) { $xpathspecsnippet("func-math-"||$f/@name, "math") }, "license": "w3c"},
  "http://www.w3.org/2001/XMLSchema": {"order": 9, "name": "Standard type constructors", "prefix": "xs",
    "content": function($f) { <p>Casts a value to the XML Schema  <a href="https://www.w3.org/TR/xmlschema11-2/#{$f/@name}" rel="nofollow">type {$f/@name/string()}</a>.</p> }},


  "http://expath.org/ns/file": {"order": 17, "name": "EXPath Module File", "prefix": "file", "unit": "xquery_module_file",
    "content": function($f) {
      let $name := $f/@name
      let $id := (if (matches($name, "separator|^(temp-dir|base-dir|current-dir|exists|last-modified|size)$|is-")) then "pr." else "fn.") || $name
      return <p>{($expathfilefunctions[@id=$id]/gitem[label="Rules"]//p)[1]/node()} <a href="{$expathfilespecurl}#{$id}" rel="nofollow">More..</a>.</p> }, "license": "w3ccla"},

  ".benibela.de": {"order": 24, "name": "Extension functions (primary)", "prefix": ("pxp", "x")},
  "http://www.benibela.de/2012/pxp/extensions": {"order": 25, "name": "Extension functions in deprecated namespace", "prefix": "pxp"},
  "http://pxp.benibela.de": {"order": 26, "name": "Extension functions (secondary)", "prefix": "x"},

  "http://jsoniq.org/functions": {"order": 32, "name": "JSONiq base functions", "prefix": "jn", "unit": "xquery_json", "content": function($f){ $jsoniq-content("jn:" || $f/@name) }, "license": "jsoniq"},
  "http://jsoniq.org/function-library": {"order": 33, "name": "JSONiq library functions", "prefix": "jnlib", "unit": "xquery_json", 
    "content": function($f){ $jsoniq-content("libjn:" || $f/@name)  }, "license": "jsoniq"}
  
}
let $functions := /f
return (
"A partial list grouped by certain topics: ",
<ul class="topics">{
for $cat in (
  {"name": "Elementary/sequence functions", "funcs": "count deep-equal distinct-values empty exists head index-of insert-before last position sort remove reverse subsequence string-join tail x:join"},
  {"name": "Data loading functions", "funcs": "parse-xml parse-json parse-xml-fragment doc json-doc unparsed-text unparsed-text-available unparsed-text-lines file:read-binary file:read-text x:request x:parse-html"},
  {"name": "XML/HTML query functions", "funcs": "base-uri id local-name name data number root string x:css x:inner-html x:inner-xml x:outer-html x:outer-xml x:form x:resolve-html x:transform x:replace-nodes"},
  {"name": "JSON array functions", "funcs": "array:append array:filter array:flatten array:fold-left array:fold-right array:for-each array:for-each-pair array:get array:head array:insert-before array:join array:put array:remove array:reverse array:size array:sort array:subarray array:tail"},
  {"name": "JSON object functions", "funcs": "map:contains map:entry map:find map:for-each map:get map:keys map:merge map:put map:remove map:size"},
  {"name": "Basic string functions", "funcs": "codepoints-to-string contains contains-token ends-with lower-case normalize-space normalize-unicode starts-with string-join string-length string-to-codepoints substring substring-after substring-before translate upper-case x:cps"},
  {"name": "Regular expressions", "funcs": "analyze-string matches replace tokenize x:extract"},
  {"name": "Mathematical functions", "funcs": "abs avg ceiling floor format-integer format-number max min random-number-generator round round-half-to-even sum math:acos math:asin math:atan math:atan2 math:cos math:exp math:exp10    math:log math:log10 math:pi math:pow math:sin math:sqrt math:tan x:integer x:integer-to-base x:product"},
  {"name": "Boolean functions", "funcs": "false not true"},
  {"name": "Environment functions", "funcs": "available-environment-variables  environment-variable x:argc x:argv x:system x:read"},
  {"name": "URI encoding functions", "funcs": "encode-for-uri escape-html-uri iri-to-uri resolve-uri file:resolve-path x:uri-decode x:uri-encode x:form x:resolve-html x:request-combine x:request-decode"},
  {"name": "Higher order functions", "funcs": "apply filter fold-left fold-right for-each for-each-pair x:transform"},
  {"name": "Date time functions", "funcs": "adjust-date-to-timezone adjust-dateTime-to-timezone adjust-time-to-timezone current-date current-dateTime current-time dateTime day-from-date day-from-dateTime days-from-duration format-date format-dateTime format-time hours-from-dateTime hours-from-duration hours-from-time minutes-from-dateTime minutes-from-duration minutes-from-time month-from-date month-from-dateTime months-from-duration seconds-from-dateTime seconds-from-duration seconds-from-time year-from-date year-from-dateTime years-from-duration parse-ietf-date x:parse-date x:parse-time x:parse-dateTime"}
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
  if ($prefix = ("fn", "pxp", "xs")) then (<br/>, "Prefix ", $prefix[. != "x"] , " can be omitted, but is deprecated.") else (),
  if ($prefix = "jnlib") then (<br/>, 'The prefix jnlib isn''t predeclared for this namespace, so --xmlns:jnlib="http://jsoniq.org/function-library" is needed in order to use these functions in Xidel.') else (),
  if (exists($module("unit"))) then (<br/>,x"In the Pascal Internet Tools library the unit {$module("unit")}.pas needs to be loaded, before these functions are available.") else (),
  
  for $f in $f group by $name := $f/@name/string() order by $name 
  let $custom-info :=  $custom-content($prefixid, $f[1])
  return (
(:    <h3>{$name}</h3>,:)
   if (empty($custom-info/descendant-or-self::code[@class="f2"]/b)) then 
     let $args := $f/@args
     let $args := if (exists($args)) then $args 
                  else $f!(if (@max-arg-count > 100) then join((1 to @min-arg-count) ! concat("$arg", .), ", ") || ", ..."
                  else for $i in @min-arg-count to @max-arg-count return
                    join((1 to $i) ! concat("$arg", .), ", ")
                  )
     return
    <code class="f" id="{$prefix[last()]}-{$f[1]/@name}">{
     $args!(<b>{$prefix[last()] || ":" ||$name}</b>, 
            colorize-args($merge-args($f, .)), 
            marker-for-version(let $i := position() return $f[$i]/@version),
      <br/>)
    }</code> else <code class="f"/>(:empty for proper padding:),
    $custom-info,
    $module("content")($f[1])
  ),
  
  switch ($module("license")) 
    case "jsoniq" return <span style="font-size: 75%">The JSONiq sections are taken from the JSONiq reference and are again licensed as CC-BY-SA.</span> 
    case "w3ccla" return ()
    case "w3c" return (
       <span style="font-size: 75%"><br/><br/>Copyright &#160;©&#160;2014&#160;<a href="http://www.w3.org/" rel="nofollow"><acronym title="World Wide Web Consortium">W3C</acronym></a><sup>®</sup>(<a href="http://www.csail.mit.edu/" rel="nofollow"><acronym title="Massachusetts Institute of Technology">MIT</acronym></a>, <a href="http://www.ercim.eu/"><acronym title="European Research Consortium for Informatics and Mathematics">ERCIM</acronym></a>,<a href="http://www.keio.ac.jp/" rel="nofollow">Keio</a>, <a href="http://ev.buaa.edu.cn/" rel="nofollow">Beihang</a>), This software or document includes material copied from or derived from XPath and XQuery Functions and Operators 3.1 (https://www.w3.org/TR/xpath-functions-31/).<br/>This document is a non-normative summary of XPath/XQuery and documentation of its implementation, the reader is not allowed to use it as technical specification for anything.</span>

    )
    default return ()
  
))
}</body></html> 
