
================================================ Basics ================================================


The trivial usage is to extract an expression from a webpage like:
  
   xidel http://www.example.org --extract //title

Instead of one or more urls, you can also pass file names or the xml data itself (xidel "<html>.." ...). 
The --extract option can be abbreviated as -e, and there are five different kind of extract expressions:
 
  1 ) XPath 2 expressions, with some changes and additional functions.
  
  2 ) XQuery 1 expressions
  
  3 ) CSS 3 selectors. 
 
  4 ) Templates, a simplified version of the page which is pattern matched against the input
  
  5 ) Multipage templates, i.e. a file that contains templates for several pages

The different kinds except multipage templates are usually automatically detected, but 
a certain type can be forced with the extract-kind option.
Or by using the shorter --xpath "..", --xquery "..", --css ".." options.
Especially XQuery and template expressions are easily confused by the auto detector. 
(Xidel assumes templates, if the expression starts with a "<" )

See the sections below for a more detailed description of each expression kind.



The next important option is --follow (abbreviated as -f) to follow links on a page. E.g: 

   xidel http://www.example.org --follow //a --extract //title 

This will print the titles of all pages that are linked from http://www.example.org.

--follow supports the same expressions as --extract, and it will follow the href or src attributes of the 
usual elements, or the contained text if there are no such attributes.



==============================  Recursion / Argument order and grouping ===============================


You can specify multiple --extract (-e) and --follow (-f) arguments to extract values from one page, 
follow the links to the next pages and extract values from there as well ...
Then it becomes important in which order the arguments are given, so it extracts before following, 
or the other way around.  
You can usually read it left-to-right like an English sentence, extracting from the current page,
or following to a new one, which will then become the next current page.
For example:

a) xidel http://site1  -e "select content 1"  http://site2  -e "select content 2" 
  
    This will extract content 1 from site 1 and content 2 from site 2

b) xidel http://site1 http://site2 -e "select content 1"  -e "select content 2"

    This will extract content 1 and 2 from site 1 as well as from site 2

c) xidel http://site1  -e "select content 1"     -f "//a (:select links:)"  -e "select content 2"

    This will extract the "content 1" from site1, and "content 2" from all sites the first site has links to. 
    
d) xidel http://site1  -f "//a (:select links:)" -e "select content 1"      -e "select content 2"
 
    This will extract "content 1" and "content 2" from all sites the first site links to, and will not 
    extract anything from site1.    

e) xidel http://site1  -e "select content 1"     -e "select content 2"      -f "//a (:select links:)"

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

f) xidel http://site1 [ -f "//a (:select links:)" -e "select content 1" ]  -e "select content 2"
  
   This will extract content 1 from all sites linked by site1 and content 2 from site1 itself.
   I.e. the extract of content 2 is not affected by the follow-option within the [..] brackets.

g) xidel http://site1 [ -f //a[@type1] --download type1/ ]   
                      [ -f //a[@type2] --download type2/ ]   
                      [ -f //a[@type3] --download type3/ ] 
   
   This will download all links of type 1 in a directory type1, all links of type2 in directory type2...
   (if written on one line)
 
[ and ] must be surrounded by a space.






========================================== XPath 2.0 / XQuery ===========================================

XPath expressions provide an easy way to extract calculated values from x/html. 
See http://en.wikipedia.org/wiki/XPath_2.0 for details.

Xidel also supports JSONiq and some custom extensions, so it deviates in a few ways from the standard. 
However, you can disable this differences with the respective options (see link below or the
command line parameter listing printed by --help).
Switched to full standard compatible mode, its implementation passes 99.3% of the XPath 2 only tests and 
97.8% of the XQuery 1 tests in the XQuery Testsuite (skipping tests for invalid input queries)

However, in the default mode, there are the following important extensions:

  Syntax:
  
    Variable assignment:                                         $var := value
   
      adds $var to a set of global variables, which can be created and accessed 
      everywhere

    JSONiq literals                                           true, false, null
    
      true and false are evaluated as true(), false(), null becomes jn:null()
    
    JSONiq arrays:                                                     [a,b,c]

       Arrays store a list of values and can be nested with each other and 
       within sequences.
       jn:members converts an array to a sequence.

    JSONiq objects:                                      {"name": value, ...}
    
       Object stores a set of values as associative map. The values can be 
       accessed similar to a function call, e.g.: {"name": value, ...}("name").
       Xidel also has {"name": value, ..}.name as an additional syntax to 
       access properties.
       jn:keys returns a sequence of all property names, libjn:values a sequence
       of values.
       Used with global variables, you can copy an object with obj2 := obj 
       (objects are immutable, but properties can be changed with 
       obj2.foo := 12, which will create a new object with the changed property)
      
    Extended strings:                                                x"..{..}.."
  
      If a string is prefixed by an "x", all expressions inside {}-parentheses 
      are evaluated, like in the value of a direct attribute constructor.
      (Warning: This was changed in Xidel 0.7. Xidel <= 0.6 used 
                "foo$var;bar" without prefix for this)
      
       
  Semantic:
     
     All string comparisons are case insensitive, and "clever", e.g.: 
              '9xy' = '9XY' < '10XY' < 'xy'
     This is more useful for html (think of @class = 'foobar'), but can be 
     disabled by passing collation urls to the string functions. 
     
     Everything is weakly typed, e.g 'false' = false() is true, and 1+"2" is 3. 

     Unknown namespace prefixes are resolved with the namespace bindings of the 
     input data. 
     Therefore //a always finds all links, independent of any xmlns-attributes.
     (however, if you explicitly declare a namespace like 
     'declare default element namespace "..."' in XQuery, it will only find 
     elements in that namespace)

     XML Schemas, error codes and static type checking are not supported.

  Certain additional functions:
  
    jn:*, libjn:* The standard JSONiq and JSONlib functions
    json("str.")  Parses a string as json, or downloads json from an url.(only use with trusted input)
    serialize-json(value) 
                  Converts a value to JSON
    extract("string","regex"[,<match>,[<flags>]])
                  This applies the regex "regex" to "string" and returns only the matching part. 
                  If the <match> argument is used, only the <match>-th submatch will be returned.
                  (this function used to be called "filter")
    css("sel")    This returns the nodes below the context node matched by the specified css 3 selector.
                  You can use this to combine css and XPath, like in 'css("a.aclass")/@href'.
    eval("xpath") This will evaluate the string "xpath" as a XPath expression
    system("..")  Runs a certain program and returns its stdout result as string
    deep-text()   This is the concatenated plain text of the every tag inside the current text. 
                  You can also pass a separator like deep-text(' ') to separate text of different nodes.
    inner-html()  This is the html content of node ., like innerHTML in javascript.  
    outer-html()  This is the same as inner-html, but includes the node itself
    inner-xml()   This is the xml content of node, similar to inner-html()
    outer-xml()   Like outer-html(), but xml-serialized
    split-equal("list", "string"[, "sep" = " "])
                  Treats the string "list" as a list of strings separated by "sep" and tests if 
                  "string" is contained in that list. (just like css class matching)
    form(form, [overridden parameters = ()])
                  Converts a html form in a http request, by url encoding all inputs descendants
                  of the given form node. You can give a sequence of parameters to  override.
                  e.g. form(//form[1], "foo=bar&xyz=123") returns a request for the first form,
                  with the foo and xyz parameters overriden by bar and 123.
                  You can also use a JSON object to set the override parameters, e.g.
                  {"foo": "bar", "xyz": 123}, in that case they are url encoded.
                  It returns an object with .url, .method and .post properties.
    match(<template>, <node>)
                  Performs pattern matching between the template (see below for template documentation) 
                  and the nodes, and returns a list or an object of matched values.
                  For exmple match(<a>{{.}}</a>, <x><a>FOO</a><a>BAR</a></x>) returns <a>FOO</a>, and
                  match(<a>*{{.}}</a>, <x><a>FOO</a><a>BAR</a></x>) returns (<a>FOO</a>, <a>BAR</a>).
                  It is also possible to use named variables in the template, in which case an object 
                  is returned, e.g:
                    match(<x><a>{{first:=.}}</a><a>{{second:=.}}</a></x>, <x><a>FOO</a><a>BAR</a></x>)
                  returns an object with two properties "first" and "bar", containing respectively
                    <a>FOO</a> and <a>BAR</a>.
                  The template can be a node or a string. Written as string the above example would be
                    match("<a>{.}</a>", <x><a>FOO</a><a>BAR</a></x>).
                 
    All additional functions except the jn/libjn functions are in the pxp: namespace, which is also set
    as default namespace.

The pasdoc documentation of my XPath 2 / XQuery library explains more details and lists more functions:
http://www.benibela.de/documentation/internettools/xquery.TXQueryEngine.html



========================================== CSS 3.0 Selectors ==========================================


CSS 3 Selectors are fully supported, except some pseudoclasses like :hover and ::before that do not 
make sense in a gui less, reading-only application.
(It is however not much tested, since I personally only use XPath)

The easiest way to use CSS selectors with the command line is to write it like --extract "css('selector')"
(the "-quotes are necessary to escape the '-quotes.) 

Alternatively you can use --extract-kind=css --extract="your selector", or --css="your selector"



============================================== Templates ==============================================

Templates are a very easy way to extract complex data from a webpage. 
Each template is basically a stripped-down excerpt of the webpage, in which the relevant parts have 
been annotated.

The best way to describe templates is with a real world example:

The following is the html of an entry of one of the recommended videos you can always see at the right
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
      <span dir="ltr" class="title">{title:=.}</span>
      <span class="stat attribution"><span class="yt-user-name " dir="ltr">{username:=.}</span></span>
      <span class="stat view-count">{views:=filter(., "[0-9.]+")}</span>
      {url := @href}
    </a>
  </li>+

This template can directly passed as an extract-expression, applied to the page of an youtube video,
and will return all recommended/related videos.
More precisely, it will return four (interleaved) arrays "title", "username", "views", "url" each 
containing the relevant values.

A basic template as above consists of three different kind of expressions:

 <li .../>   A normal html element will be matched to the processed html page.
             This means it will search the first element on the page, that has the same node name,
             all the attributes with the same values, and whose children match the children of the 
             template element.             
 
 {..}        A {} marker will execute the contained XPath expression, once the corresponding 
             place in the html page has been found. 
             The context node . will refer to the surrounding element, and you can use my extended 
             XPath syntax (var := value) to create a variable. (see XPath above)
             Often you want to read the entire matched element in a variable with $name, which
             can be written as {$name := .} or further abbreviated as {$name} .             
             It can also be used within attributes, like <a x="{.}"/> to read the attribute value.
             (the parentheses can be also replaced by <template:s>..</template:s> or <t:s>..</t:s>)
             
 
  +          Finally the loop marker will repeat the matching of the previous element as long as 
             possible (an similar syntax is <t:loop>..</t:loop> or <t:loop>..</t:loop>).
            
            

This is sufficient for most basic scraping operations, but you can also use the following things in a 
template:
 
 textnodes                Textnodes are matched like html element nodes.
                          A textnode in the webpage is considered a valid match, if it starts
                          with the same text as the text node in the template.
                          (but you can change this behavior to ends-with/exact/regex-comparisons with 
                          the  <t:meta [default-text-matching="??"] [default-case-sensitive="??"]/>
                          command)
 
 <t:if test="??"/>        All children of a template:if-tag are ignored if the test-XPath-expressions 
                          evaluates to false()
 
 <t:switch [value="??"]>  Only one of the child elements will be used for matching

 <t:switch-prioritized>   Same a t:switch, but it will choose the earliest template child that has a match.
 
 t:optional="true"        Html nodes can be marked as optional, and they will be ignored, if no possible
                          match can be found
 
 t:condition="??"         A XPath expression that c be The context node (.) refers to a potential match.
 
 *                        Like +, but it can also match none
 
 {min,max} or {count}     Matches between [min,max] or {count}-many occurrences of the previous element
 
 
 <t:loop min=.. max=../>  The same as above. However, t:loop will repeat all its children, while a marker 
                          like + can only repeat the single, previous element.
                          
 ?                        Short notation for t:optional. 
 
(see http://www.benibela.de/documentation/internettools/extendedhtmlparser.THtmlTemplateParser.html 
 for  more detailed explanations)
 

There is also a Greasemonkey script to create templates directly by just selecting the text on the 
corresponding webpage.



========================================= Multipage templates ==========================================

Multipage templates collect several single page templates in a xml file.
They are basically just a list of <page/> nodes with <post/> data and associated <template/>s
E.g.

  <action>
    <page url="http://www.example.org/url">
      <post name="var">unescaped post data</post>
      <post>your=escaped&post=data&...</post>
      <template> 
        <a>{alink:=.}</a>* 
      </template>
    </page>
    
    <page ...> ... </page>
    
    ...
  </action>

All pages are downloaded with GET or respectively POST requests, and processed with the given template.
The page-node also accepts a "test" attribute, which gives an XPath expression that needs to be true, 
if the page element should be used.
In the attributes and the text of post-nodes, everything enclosed in {..} parentheses is evaluated
as xpath expression. (like in an extended x".." string, see above)

Since this would be cumbersome to pass directly to --extract, you can also specify the containing file
with the --template-file argument.

You can also have multiple <action/>s in a multipage template (surrounded by a parent element with 
name <actions>), and call the later actions with <call action=".."/> from another action.
If a template with multiple actions is passed to Xidel it will always perform the first action,
unless the --template-action parameter specifies another action to run. (in Xidel > 0.5)

There are also <variable>-elements to declare variables and <loop>-elements to repeat other elements, 
see http://www.benibela.de/documentation/internettools/multipagetemplate.TMultiPageTemplate.html
for more details.

=========================================== Input formats =============================================

Xidel supports html and xml input, and the option input-format can be used to set the parsing behaviour:
  
auto:           Automatically switch between html and xml
  
html:           The input will be parsed as html. 
                Missing tags like head, body, tbody are automatically created.
                (beware that this means table/tr is never valid, and either table//tr or table/tbody/tr
                has to be used)
 
xml:            The input will be parsed as xml. 
                However, it still uses the html parser, so it will correct missing end tags and not
                support DTDs.
                
xml-strict:     The input will be parsed as strict xml. 
                This uses the standard fpc, validating xml parser.
                
You can also use json files, by loading them explicitly with pxp:json() or jn:json-doc() within a
XPath/XQuery expression.

=========================================== Output formats =============================================

Xidel has several different output formats, which can be chosen with the output-format option:

adhoc:          A very simple format, it will just print all values (default)

xml:            The output will be serialized as xml

html:           The output will be serialized as html

xml-wrapped:    It will print a xml-based machine readable output.
                Sequences will become <seq><e>value 1</e><e>value 2</e>...</seq>
                Objects will become <object><property-1>value 1</property-1><prop-2>..</prop-2>..</object>
                (so in contrast to xml, it will keep variable names and type information intact)

json-wrapped:   It will print a json-based machine readable output.
                Sequences become arrays [ ... ].
                Objects become objects. {"prop-1": "value 1", "prop-2": "value 2", ... }       
                (this was called json before Xidel 0.7)

bash:           Prints a bash script that sets the internal variables as bash variables.
                E.g.
                eval $(xidel http://data -e 'title:=//title' -e 'links:=//a')
                can be used to set the bash variable $title to the title of a page and the
                variable $links to a bash array of all links on the page.

cmd:            Like bash, but for Windows cmd.exe

Generally it prints a sequence of all processed pages (i.e. each page a single sequence element), 
and the variables defined as global variables or read by a template become variables or 
object properties.
There is a special rule for json-wrapped  output, if the template assigns multiple values to the same
variable: Xidel will collect all these values in an array. I.e. (a:=1, b:=2, a:=3, c:=4)
becomes "a": [1, 3], "b": 2. "c": 4





