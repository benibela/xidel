
================================================ Basics ================================================


The trivial usage is to extract an expression from a webpage like:
  
   xidel http://www.example.org --extract //title

Instead of one or more urls, you can also pass file names or the xml data itself (xidel "<html>.." ...). 
The --extract option can be abbreviated as -e, and there are five different kind of extract expressions:
 
  1*) XPath 2 expressions, with some changes and additional functions.
  
  2 ) XQuery 1 expressions
  
  3 ) CSS 3 selectors. 
 
  4*) Templates, a simplified version of the page, in which the values you want to extract are annotated
  
  5 ) Multipage templates, i.e. a file that contains templates for several pages

The kinds marked with a * are automatically detected, the other ones have to be activated with the 
extract-kind option. 
CSS Selectors are also "autodetected", if they are written as css("..."),
XQuery expressions are detected, if they start with "xquery version "1.0"; ".
See the sections below for a more detailed description of each expression kind.



The next important option is --follow (abbreviated as -f) to follow links on a page. E.g: 

   xidel http://www.example.org --follow //a --extract //title 

This will print the titles of all pages that are linked from http://www.example.org.

--follow supports the same expressions as --extract, and it will follow the href or src attributes of the 
usual elements, or the contained text if there are none.



=====================================  Recursion / Argument order ======================================


You can specify multiple --extract (-e) and --follow (-f) arguments to extract values from one page, 
follow the links to the next pages and extract values from there as well ...
Then it becomes important in which order the arguments are given, so it extracts before following, 
or the other way around. 
For example:

a) xidel http://site1  -e "select content 1"  http://site2  -e "select content 2" 
  
    This will extract content 1 from site 1 and content 2 from site 2

b) xidel http://site1 http://site2 -e "select content 1"  -e "select content 2"

    This will extract content 1 and 2 from site 1 as well as from site 2

c) xidel http://site1  -e "select content 1"  -f "select links"     -e "select content 2"

    This will extract the "content 1" from site1, and "content 2" from all sites the first site has links to. 
    
d) xidel http://site1  -f "select links"      -e "select content 2" -e "select content 1"
 
    This will extract "content 1" and "content 2" from all sites the first site links to, and will not 
    extract anything from site1.    

e) xidel http://site1  -e "select content 2"  -e "select content 1" -f "select links"

   This  is some kind of special case. Since -f is the last option, it will repeat the previous operation, i.e.
   it will extract content 1 and 2 from site1 and ALL sites that can be reached by an selected link on site1 
   or any other of the processed sites. 
   Only if there were another -e after -f, it would extract that from the first set of followed links and stop.
 
In some kinds of extract expression you can create new variables, if you assign values to a variable called
"_follow", that value will be included in the next follow expression. 
If you assign an object to _follow, its properties will override the command line parameters with the same
value. 

Generally an option modifier (like --extract-kind) affects all succeeding options, unless there are none, 
then it affects the immediate preceding option.


========================================== XPath 2.0 / XQuery ===========================================

XPath expressions provide an easy way to extract calculated values from x/html. 
See http://en.wikipedia.org/wiki/XPath_2.0 for details.

In the default configuration the XPath/XQuery implementation of Xidel deviates in a few ways from the 
standard. However, you can disable this differences with the respective options (see link below or the
command line parameter listing printed by --help).
The most important changes are:

  Syntax:
  
    Within double-quoted strings variables are replaced, so "$var;xyz" is the same as concat($var, "xyz")
    
    You can assign values to variables like:                           var := value       
      
    You can use objects with properties within the XPath expressions:  var := object(),  var.foo := "bar"
    The properties can then be accessed by $var.foo, and are copied by object assignments, i.e.:
    after obj2 := var, the value of $obj2.foo is $var.foo is "bar".
    Different variables always have different objects, i.e. there are no pointers, and changing $obj2.foo 
    in the example above, will not change $var.foo.
    The object-constructor can take a sequence with initial parameters, e.g. obj:=object(("a", 1, "b", 2)) 
    will create an object $obj, with $obj.a = 1 and $obj.b = 2.

  
  Semantic:
     
     All string comparisons are case insensitive, and "clever", e.g.:  '9xy' = '9XY' < '10XY' < 'xy'
     This is more useful for html (think of class = 'foobar'), but can be disabled by passing collation urls
     to the string functions. 
     
     Everything is weakly typed, e.g 'false' = false() is true, and 1 + "2" is 3. 

     Unknown namespace prefixes are resolved with the namespace bindings of the input data. 
     Therefore //a always finds all links, independent of any xmlns=".." attributes.
     (however, if you explicitely declare a namespace like 'declare default element namespace "..."' in XQuery, 
     it will only find -elements in that namespace)

     XML Schemas, error codes and static type checking are not supported.

  Additional functions:
 
    filter("string","regex"[,<match>,[<flags>]])
                  This applies the regex "regex" to "string" and returns only the matching part. 
                  If the <match> argument is used, only the <match>-th submatch will be returned.
    css("sel")    This returns the nodes below the context node matched by the specified css 3 selector.
                  You can use this to combine css and XPath, like in 'css("a.aclass")/@href'.
    eval("xpath") This will evaluate the string "xpath" as an XPath expression
    deep-text()   This is the concatenated plain text of the every tag inside the current text. 
                  You can also pass a separator like deep-text(' ') to separate text of different nodes.
    inner-xml()   This is the xml content of node ., like innerHTML in javascript.  
    outer-xml()   This is the same as inner-xml, but includes the node itself
    split-equal("list", "string"[, "sep" = " "])
                  Treats the string "list" as a list of strings separated by "sep" and tests if 
                  "string" is contained in that list. (just like css class matching)
    form(form, [overridden parameters = ()])
                  Converts a html form in a http request, by url encoding all inputs descendants
                  of the given form node. You can give a sequence of parameters to  override.
                  e.g. form(//form[1], "foo=bar&xyz=123") returns a request for the first form,
                  with the foo and xyz parameters overriden by bar and 123.
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
                 


The pasdoc documentation of my XPath 2 / XQuery library explains more details:
http://www.benibela.de/documentation/internettools/xquery.TXQueryEngine.html


========================================== CSS 3.0 Selectors ==========================================


CSS 3 Selectors are fully supported, except some pseudoclasses like :hover and ::before that do not 
make sense in a gui less, reading-only application.
(It is however not much tested, since I personally only use XPath)

The easiest way to use CSS selectors with the command line is to write it like --extract "css('selector')"
(the "-quotes are necessary to escape the '-quotes.) 

Alternatively you can use --extract-kind=css --extract=selector. 


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
 
 t:condition="??"         An XPath expression that c be The context node (.) refers to a potential match.
 
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
You can use $..; variables in the attributes and post-nodes. 

Since this would be cumbersome to pass directly to --extract, you can also specify the containing file
with the --template-file argument.

You can also have multiple <action/>s in a multipage template (surrounded by a parent element with 
name <actions>), and call the later actions with <call action=".."/> from another action.
If a template with multiple actions is passed to Xidel it will always perform the first action,
unless the --template-action parameter specifies another action to run. (in Xidel > 0.5)

There are also <variable>-elements to declare variables and <loop>-elements to repeat other elements, 
see http://www.benibela.de/documentation/internettools/multipagetemplate.TMultiPageTemplate.html
for more details.

=========================================== Output formats =============================================

Xidel has three different output formats, which can be chosen with the output-format option:

adhoc:  A very simple format, it will just print all values (default)

json:   It will print valid json. 
        Sequences become arrays [ ... ].
        Objects become objects. {"prop-1": "value 1", "prop-2": "value 2", ... }       

xml:    It will print valid xml.
        Sequences will become <seq><e>value 1</e><e>value 2</e>...</seq>
        Objects will become <object><property-1>value 1</property-1><prop-2>..</prop-2>..</object>

Generally it prints a sequence of all processed pages (i.e. each page a single sequence element), 
and variables read by template become object properties.
There is a special rule for json template output, if the template assigns multiple values to the same
variable: Xidel will collect all these values in an array. I.e. (a:=1, b:=2, a:=3, c:=4)
becomes "a": [1, 3], "b": 2. "c": 4





