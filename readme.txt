
================================================ Basics ================================================


The most common usage is to extract an expression from a webpage like:
  
   xidel http://www.example.org --extract //title

Instead of one or more urls, you can also pass file names or the XML data itself (xidel "<html>.." ...). 
The --extract option can be abbreviated as -e, and there are different kinds of extract expressions:
 
  1 ) XPath 3.0 and XQuery 3.0 expressions, with some extensions and additional functions.
   
  2 ) XPath 2 and XQuery 1 expressions for legacy scripts

  3 ) CSS 3 selectors
 
  4 ) Templates, a simplified version of the page which is pattern matched against the input
  
  5 ) Multipage templates, i.e. a file that contains templates for several pages
  

These different kinds except multipage templates are usually automatically detected, but 
a certain type can be forced with the extract-kind option.
Or by using the shorter --xpath "..", --xquery "..", --css ".." options.
Especially XQuery and template expressions are easily confused by the auto detector. 
(Xidel assumes templates, if the expression starts with a "<" )
If no XPath/XQuery version is given, Xidel uses the 3.0 mode.


See the sections below for a more detailed description of each kind of expression.



The next important option is --follow (abbreviated as -f) to follow links on a page. E.g: 

   xidel http://www.example.org --follow //a --extract //title 

This will print the titles of all pages that are linked from http://www.example.org.

--follow supports the same expressions as --extract. If it selects an element, it will 
go to the data referenced by that element (e.g. in a @href or @src attribute).
Non standard elements are converted to a string, which is interpreted as an url.
It will also follow forms, and the form-function can be used to fill-in some values in the form, e.g.:

  xidel http://www.example.org -f "form((//form)[1], {'username': 'foobar', 'password': 'XXX'})" -e //title

to submit the first form on a page to login somewhere. 


==============================  Recursion / Argument order and grouping ===============================


You can specify multiple --extract (-e) and --follow (-f) arguments to extract values from a page, 
e.g. follow the links to the next pages and extract values from there as well ...
Thereby it is important in which order the arguments are given, so e.g. it extracts before following, 
and not the other way around.  
You can usually read it left-to-right like an English sentence, extracting from the current page,
or following to a new one, which will then become the next current page.
For example:

a) xidel http://site1  -e "select content 1"  http://site2  -e "select content 2" 
  
    This will extract "content 1" from site 1 and "content 2" from site 2

b) xidel http://site1 http://site2 -e "select content 1"  -e "select content 2"

    This will extract "content 1" and "content 2" from site 1 as well as from site 2

c) xidel http://site1  -e "select content 1"     -f "//a (: i.e. select links:)"  -e "select content 2"

    This will extract "content 1" from site1, and "content 2" from all sites the first site has links to. 
    
d) xidel http://site1  -f "//a" -e "select content 1"      -e "select content 2"
 
    This will extract "content 1" and "content 2" from all sites the first site links to, and will not 
    extract anything from site1.    

e) xidel http://site1  -e "select content 1"     -e "select content 2"      -f "//a"

   This  is some kind of special case. Since -f is the last option, it will repeat the previous operation, i.e.
   it will extract content 1 and 2 from site1 and ALL sites that can be reached by a selected link on site1 
   or any other of the processed sites. 
   Only if there were another -e after -f, it would extract that from the first set of followed links and stop.
 
In some kinds of extract expression you can create new variables, if you assign values to a variable called
"_follow", that value will be included in the next follow expression. 
If you assign an object to _follow, its properties will override the command line parameters with the same
value. 

Generally an option modifier (like --extract-kind) affects all succeeding options, unless there are none, 
then it affects the immediate preceding option.




You can always override the argument order by using [ and ] to group the options.
For example:

f) xidel http://site1 [ -f "//a (:i.e. select links:)" -e "select content 1" ]  -e "select content 2"
  
   This will extract content 1 from all sites linked by site1 and content 2 from site1 itself.
   I.e. the extract of content 2 is not affected by the follow-option within the [..] brackets.

g) xidel http://site1 [ -f //a[@type1] --download type1/ ]   
                      [ -f //a[@type2] --download type2/ ]   
                      [ -f //a[@type3] --download type3/ ] 
   
   This will download all links of type 1 in a directory type1, all links of type2 in directory type2...
   (if written on one line)
 
[ and ] must be surrounded by a space.

The environment variable XIDEL_OPTIONS can be used to set Xidel's default options, for example
 XIDEL_OPTIONS="--silent --color=never"
to disable some output and coloring.



=========================== XPath 2.0 / XPath 3.0 / XQuery 1.0 / XQuery 3.0 ============================

XPath expressions provide an easy and Turing-complete way to extract calculated values from X/HTML.
XQuery is a superset language that can e.g. create new XML/HTML elements and documents.  
See http://en.wikipedia.org/wiki/XPath_3.0 and https://en.wikipedia.org/wiki/XQuery for a quick summary,
or https://www.w3.org/TR/xpath-30/ , https://www.w3.org/TR/xquery-30/ and 
https://www.w3.org/TR/xpath-functions-30/ for all the details.

Xidel also supports JSONiq and some custom extensions. 
If the query begins with a version declaration like xquery version "3.0"; all extensions are disabled (then it can pass 99.6% of the test cases in the QT3 XQuery Test Suite). If you use version codes like "3.0-xidel" or "3.0-jsoniq", all or some extensions are enabled. Without any version declaration the extensions are enabled, unless disabled by command-line parameters.

Important extensions are:

  Variable assignment:                                         $var := value
 
    adds $var to a set of global variables, which can be created and accessed 
    everywhere.
    (Xidel prints the value of all variables to stdout, unless you use the --extract-exclude option)

  JSONiq literals                                           true, false, null
  
    true and false are evaluated as true(), false(), null becomes jn:null()
  
  JSONiq arrays:                                                     [a,b,c]

     Arrays store a list of values and can be nested within each other and 
     within sequences.
     jn:members converts an array to a sequence.

  JSONiq objects:                                      {"name": value, ...}
  
     Objects store a set of values as associative map. The values can be 
     accessed similar to a function call, e.g.: {"name": value, ...}("name").
     Xidel also has {"name": value, ..}.name and {"name": value, ..}/name
     as an additional, proprietary syntax to access properties.
     jn:keys or $object() returns a sequence of all property names, 
     libjn:values a sequence of values.
     Used with global variables, you can copy an object with obj2 := obj 
     (objects are immutable, but properties can be changed with 
     obj2.foo := 12, which will create a new object with the changed property)
    
  Extended strings:                                                x"..{..}.."

    If a string is prefixed by an "x", all expressions inside {}-parentheses 
    are evaluated, like in the value of a direct attribute constructor.
    E.g. x"There are {1+2+3} elements" prints "There are 6 elements".
    
  Special string comparison:
   
    All string comparisons are case insensitive, and "clever", e.g.: 
            '9xy' = '9XY' < '10XY' < 'xy'
   This is more useful for HTML (think of @class = 'foobar'), but can be 
   disabled by passing collation urls to the string functions. 
   
   Dynamic typing:
   
     Strings are automatically converted to untypedAtomic, so 'false' = false() is true, and 1+"2" is 3. 
     
   Local namespace prefix resolving:

     Unknown namespace prefixes are resolved with the namespace bindings of the 
     input data. 
     Therefore //a always finds all links, independent of any xmlns-attributes.

  Certain additional functions:
  
    jn:*, libjn:* The standard JSONiq and JSONlib functions
    file:*        The functions of the EXPath file module
    json("str.")  Parses a string as json, or downloads json from an url.(only use with trusted input)
    serialize-json(value) 
                  Serializes a value as JSON, i.e. converts the value to JSON and converts that to a string
    extract("string","regex"[,<match>,[<flags>]])
                  This applies the regex "regex" to "string" and returns only the matching part. 
                  If the <match> argument is used, only the <match>-th submatch will be returned.
                  <match> can be a sequence of numbers to select multiple matches.
    css("sel")    This returns the nodes below the context node matched by the specified css 3 selector.
                  You can use this to combine css and XPath, like in 'css("a.aclass")/@href'.
    join(sequence) or join(sequence, separator)
                  This is basically the same as string-join, but can be used for non-string values.
                  E.g. join((1,2,3)) returns the string "1 2 3".
    eval("xpath") This will evaluate the string "xpath" as an XPath/XQuery expression
    system("..")  Runs a certain program and returns its stdout result as string
    read()        Reads a line from stdin
    inner-html()  This is the HTML content of node ., like innerHTML in javascript.  
    outer-html()  This is the same as inner-html, but includes the node itself
    inner-xml()   This is the XML content of node, similar to inner-html()
    outer-xml()   Like outer-html(), but XML-serialized
    form(form, [overridden parameters = ()])
                  Converts a HTML form to an http request, by url encoding all inputs descendants
                  of the given form node. You can give a sequence of parameters to  override.
                  e.g. form(//form[1], "foo=bar&xyz=123") returns a request for the first form,
                  with the foo and xyz parameters overridden by bar and 123.
                  You can also use a JSON object to set the override parameters, e.g.
                  {"foo": "bar", "xyz": 123}, in tis case they are automatically url encoded.
                  It returns an object with .url, .method and .post properties.
    request-combine(request, [overridden parameters = ()])
                  This function can be used to modify the object returned by form. (preliminary)
                  The second parameter behaves like that parameter of form.
    match(<template>, <node>)
                  Performs pattern matching between the template (see below for template documentation) 
                  and the nodes, and returns a list or an object of matched values.
                  For example match(<a>{{.}}</a>, <x><a>FOO</a><a>BAR</a></x>) returns <a>FOO</a>, and
                  match(<a>*{{.}}</a>, <x><a>FOO</a><a>BAR</a></x>) returns (<a>FOO</a>, <a>BAR</a>).
                  It is also possible to use named variables in the template, in which case an object 
                  is returned, e.g:
                    match(<x><a>{{first:=.}}</a><a>{{second:=.}}</a></x>, <x><a>FOO</a><a>BAR</a></x>)
                  returns an object with two properties "first" and "bar", containing respectively
                    <a>FOO</a> and <a>BAR</a>.
                  The template can be a node or a string. Written as string the above example would be
                    match("<a>{.}</a>", <x><a>FOO</a><a>BAR</a></x>).
    x:call-action($name)
                  Calls the action of a multipage template.
    transform(node, transformer-function)
                  This function can perform an arbitrary transformation of a document, by calling the 
                  transformer-function for every descendant node and replacing the node with the value returned by the function.
                  E.g. transform(/, function($x) { if (name($x) = "a") then <a>{$x/@*, <b>{$x/node()}</b>}</a> else $x } )
                  will make every link bold.
    garbage-collect() 
                  Frees unused memory
    x:request($request)
                  Sends an HTTP request. The request parameters are the same as the value accepted by --follow.
                  It returns a JSON object with properties "url", "headers", "raw" corresponding to the default
                  variables listed below. The property "type" contains the content-type, and either "json" or "doc"
                  a JSON or X/HTML document.
    x:argc(), x:argv($i)
                  Return command line arguments.
    x:integer($input, [$base])
                  Converts a string to an integer. It accepts base-prefixes like 0x or 0b, e.g 0xABCDEF
    x:integer-to-base($i, $base)
                  Converts an integer to a certain base
    x:clear-log([$name]) 
                  Removes variables.
    x:get($name, [$default])
                  Gets the value of a variable, or $default if the variable does not exist.
    x:get-log([$name]) 
                  Gets every value a variable had.
                 
    Additional functions without prefix are in the pxp: namespace, which is also set as default namespace.

The pasdoc documentation of my XPath / XQuery 3.0 library explains more details and lists more functions:
http://www.benibela.de/documentation/internettools/xquery.TXQueryEngine.html

Future versions might drop the JSONiq object/array syntax in favor of the new XQuery 3.1 map/array syntax.

Xidel also defines the following global default variables:
 
   $raw         Unparsed input text
   $url         Url the input was retrieved from (past redirect)
   $host, $path Respective part of the url
   $json        Parsed JSON input, if it was JSON 
   $headers     All HTTP headers, including status code




========================================== CSS 3.0 Selectors ==========================================


CSS 3 Selectors are fully supported, except some pseudoclasses like :hover and ::before that do not 
make sense in a gui less, reading-only application.
(It is however not much tested, since I personally only use XPath)

The easiest way to use CSS selectors with the command line is to write it like --extract "css('selector')"
(the "-quotes are necessary to escape the '-quotes.) 

Alternatively you can use --extract-kind=css --extract="your selector", or --css="your selector"



============================================== Patterns ==============================================

Patterns (formely called templates) are a very easy way to extract complex data from a webpage. 
Each pattern is basically a stripped-down excerpt of the webpage, in which the relevant parts have 
been annotated.

The best way to describe patterns is with a real world example:

The following is the HTML of an entry of one of the recommended videos you can always see at the right
side of an youtube video: (skipped the image part for clarity)

  <li class="video-list-item">
    <a href="/watch?v=F6SeX76_F5Q&amp;feature=related" class="related-video yt-uix-contextlink yt-uix-sessionlink" 
       data-sessionlink="ved=CBQQzRooEQ%3D%3D&amp;ei=CIDyscip97ECFcWw3godn3H0ug%3D%3D&amp;feature=related">
      <!-- skipped -->
      <span dir="ltr" class="title" title="Idras Sunhawk Lyrics">Idras Sunhawk Lyrics</span>
      <span class="stat attribution">by <span class="yt-user-name " dir="ltr">FolkAndPaganSongs</span></span>
      <span class="stat view-count">5.634 views</span>
    </a>
  </li>

As you see there are actual interesting values like the url/title/username/view texts, and irrelevant, 
changing values like the session url.
If you now remove the irrelevant parts, and annotate the interesting values as {name:=value}, 
you get the following:

  <li class="video-list-item">
    <a>
      <span dir="ltr" class="title">{$title:=.}</span>
      <span class="stat attribution"><span class="yt-user-name " dir="ltr">{$username:=.}</span></span>
      <span class="stat view-count">{$views:=extract(., "[0-9.]+")}</span>
      {url := @href}
    </a>
  </li>+
  
(:=. is optional and thus could be omitted in the first two {}-expressions to get a shorter pattern)

This pattern can directly passed as an extract-expression, applied to the page of an youtube video,
and will return all recommended/related videos.
More precisely, it will return four (interleaved) arrays "title", "username", "views", "url" each 
containing the relevant values.

A basic pattern as above consists of three different kind of expressions:

 <li .../>   A normal HTML element will be matched to the processed HTML page.
             This means it will search the first element on the page, that has the same node name,
             all the attributes with the same values, and whose children match the children of the 
             pattern element.             
 
 {..}        A {} marker will execute the contained XPath expression, once the corresponding 
             place in the HTML page has been found. 
             The context node . will refer to the surrounding element, and you can use my extended 
             XPath syntax (var := value) to create a variable. (see XPath above)
             Often you want to read the entire matched element in a variable with $name, which
             can be written as {$name := .} or further abbreviated as {$name} .             
             It can also be used within attributes, like <a x="{.}"/> to read the attribute value.
             (the parentheses can be also replaced by <template:s>..</template:s> or <t:s>..</t:s>)
             
 
  +          Finally the loop marker will repeat the matching of the previous element as long as 
             possible (an similar syntax is <t:loop>..</t:loop> or <t:loop>..</t:loop>).
            
            

This is sufficient for most basic scraping operations, but you can also use the following things in a 
pattern:
 
 textnodes                Textnodes are matched like HTML element nodes.
                          A textnode in the webpage is considered a valid match, if it starts
                          with the same text as the text node in the pattern.
                          (but you can change this behavior to ends-with/exact/regex-comparisons with 
                          the  <t:meta [default-text-matching="??"] [default-case-sensitive="??"]/>
                          command)
 
 <t:if test="??"/>        All children of a template:if-tag are ignored if the test-XPath-expressions 
                          evaluates to false()
 
 <t:switch [value="??"]>  Only one of the child elements will be used for matching

 <t:switch-prioritized>   Same a t:switch, but it will choose the earliest pattern child that has a match.
 
 t:optional="true"        HTML nodes can be marked as optional, and they will be ignored, if no possible
                          match can be found
 
 t:condition="??"         A XPath expression that c be The context node (.) refers to a potential match.
 
 *                        Like +, but it can also match none
 
 {min,max} or {count}     Matches between [min,max] or {count}-many occurrences of the previous element
 
 
 <t:loop min=.. max=../>  The same as above. However, t:loop will repeat all its children, while a marker 
                          like + can only repeat the single, previous element.
                          
 ?                        Short notation for t:optional. 
 
(see http://www.benibela.de/documentation/internettools/extendedhtmlparser.THtmlTemplateParser.html 
 for  more detailed explanations)
 

There is also a Greasemonkey script to create pattern directly by just selecting the text on the 
corresponding webpage.



========================================= Multipage templates ==========================================

Multipage templates collect several single page patterns in a XML file.
They are basically just a list of <page/> nodes with <post/> data and associated <template/>s
E.g.

  <action>
    <page url="http://www.example.org/url">
      <post name="var">unescaped post data</post>
      <post>your=escaped&post=data&...</post>
    </page>
    <pattern> 
      <a>{alink:=.}</a>* 
    </pattern>
    
    <page ...> ... </page>
    
    ...
  </action>

All pages are downloaded with GET or respectively POST requests, and processed with the given pattern.
The page-node also accepts a "test" attribute, which gives an XPath expression that needs to be true, 
if the page element should be used.
In the attributes and the text of post-nodes, everything enclosed in {..} parentheses is evaluated
as xpath expression. (like in an extended x".." string, see above)

Since this would be cumbersome to pass directly to --extract, you can also specify the containing file
with the --template-file argument.

You can also have multiple <action/>s in a multipage template (surrounded by a parent element with 
name <actions>), and call the later actions with <call action=".."/> from another action.
If a template with multiple actions is passed to Xidel it will always perform the first action,
unless the --template-action parameter specifies another action to run. 

There are also <variable>-elements to declare variables and <loop>-elements to repeat other elements, 
see http://www.benibela.de/documentation/internettools/multipagetemplate.TMultiPageTemplate.html
for more details.

=========================================== Input formats =============================================

Xidel supports HTML and XML input, and the option input-format can be used to set the parsing behavior:
  
auto:           Automatically detect the format 
  
html:           The input will be parsed as HTML. 
                Missing tags like head, body, tbody are automatically created.
                (beware that this means table/tr is never valid, and either table//tr or table/tbody/tr
                has to be used)
 
xml:            The input will be parsed as XML. 
                However, it still uses the HTML parser, so it will correct missing end tags and not
                support DTDs.
                
xml-strict:     The input will be parsed as strict XML. 
                This uses the standard validating XML parser of FreePascal.
                
json:           The input will be parsed as JSON and stored in . and the $json variable.
                It can be changed by assigning to $json(..)(..).. := 

json-strict:    Like json, but invalid JSON will be rejected.

XPath/JSONiq/Xidel also provide functions to load data explicitly within expressions:

fn:doc                          Loads an HTML/XML document from an url
fn:unparsed-text                Loads a text document from an url
fn:parse-xml                    Parses an XML document from a string
fn:parse-html                   Parses an HTML document from a string
pxp:json or jn:json-doc         Loads a JSON file. The following options can be set as map in the 2nd arg: 
                                      jsoniq-multiple-top-level-items: allow multiple items
                                      liberal: allow invalid JSON
                                      trailing-comma: allow trailing commas
file:read-text                  Loads a local text file

=========================================== Output formats =============================================

Xidel has several different output formats, which can be chosen with the output-format option:

adhoc:          A very simple format, it will just print all values (default)

xml:            The output will be serialized as XML

html:           The output will be serialized as HTML

xml-wrapped:    It will print a XML-based machine readable output.
                Sequences will become <seq><e>value 1</e><e>value 2</e>...</seq>
                Objects will become <object><property-1>value 1</property-1><prop-2>..</prop-2>..</object>
                (so in contrast to XML, it will keep variable names and type information intact)

json-wrapped:   It will print a json-based machine readable output.
                Sequences become arrays [ ... ].
                Objects become objects. {"prop-1": "value 1", "prop-2": "value 2", ... }  
                (This converts non-JSON to JSON. It is not required to create JSON directly)

bash:           Prints a bash script that sets the internal variables as bash variables.
                The variables can be imported in the current script with, e.g:
                  eval $(xidel http://data -e 'title:=//title' -e 'links:=//a')
                can be used to set the bash variable $title to the title of a page and the
                variable $links to a bash array of all links on the page.

cmd:            Like bash, but for Windows cmd.exe
                The variables can be set in the current script with a for statement like:
                    FOR /F "delims=" %A IN ('xidel --output-format^=cmd ... ') DO %A

Generally it prints a sequence of all processed pages (i.e. each page a single sequence element), 
and the variables defined as global variables or read by a template become variables or 
object properties.
There is a special rule for json-wrapped  output, if the template assigns multiple values to the same
variable: Xidel will collect all these values in an array. I.e. (a:=1, b:=2, a:=3, c:=4)
becomes "a": [1, 3], "b": 2. "c": 4

Only the string value of elements is printed, unless the --printed-node-format is set to XML or HTML.
(E.g. <a>bc</a> only prints "bc")

The separators between elements can be set with --output-separator. 

Output parameter should be given before any extract parameter.
(as future versions may allow to use a different output format for every extract expression)

======================================== Modifying HTTP requests ===========================================

These options can be used to modify HTTP requests:

--post, -d

    You can use --post (-d) to specify data that should be transmitted. 
    E.g. xidel -d "a=b" -d "c=d" http://example.org will send a POST request "a=b&c=d".
       
    Different requests will use different data, if given, 
    so xidel  -d "a=b" http://example.org/1 -d "c=d" http://example.org/2
    will send a=b to /1 and c=d /2.
    However, if c=d was not given, it would send a=b to both urls.
    
    You can prepend & to always keep the previous data.
    E.g. xidel -d "a=b" http://example.org/1 -d "&c=d" http://example.org/2
    will send "a=b&c=d" to /2.
    
    An empty argument can be used to clear the data.
    E.g. xidel -d "a=b" http://example.org/1 -d "" http://example.org/2
    will send a GET request without any data to /2.

--method
    
    The method can be set with --method, e.g. POST or PUT.
    E.g. xidel -d "a=b" --method PUT http://example.org
    
--header, -H
    
    A header can be given with --header (-H).
    E.g. xidel -H "Content-Type: text/html" http://example.org
    
    Headers use the same rules for concatenating as post data. (The & will be replaced by CRLF)
    
--form, -F

   --form behaves similarly to --post, but it will send the data as multipart/form-data.
    
   It can also be used for file uploading, e.g. xidel -F a=@filename http://example.org
   will upload the file filename in the parameter a. 
   You can use -F "a=<filename"  to just upload the content of the file and not the file itself.

   You can use ;type=... and ;filename=... to set the Content-Type and filename of the data.

   See also the documentation of the XPath function pxp:form for creating multipart requests in -f.

