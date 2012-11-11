<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<meta name="description" content="Xidel is a command line tool to download html/xml pages and extract data from them using CSS 3 selectors, XPath 2 expressions or pattern-matching templates." />
<meta name="author" content="Benito van der Zander" />
<meta name="keywords" content="Xidel,  xpath, xpath 2, css, css 3, template, pattern matching, xml, html, download" />
<link rel="stylesheet" type="text/css" href="all.css"/>
<link rel="stylesheet" type="text/css" href="xidel.css"/>
<title>Xidel - HTML/XML data extraction tool </title></head>
<body>
<h1><a name="home">Xidel - HTML/XML data extraction tool</a></h1>

<ul id="navigation">
  <li style="border-left: 1px solid blue;"><a href="#home">Start</a></li>
  <li><a href="#features">Features</a></li>
  <li><a href="#examples">Examples</a></li>
  <li><a href="#downloads">Downloads</a></li>
  <li><a href="#script">Xidelscript</a></li>
  <li><a href="#contact">Contact</a></li>
  
  <!---<li style="right: 1em; position:absolute"><a href="index.html">Deutsche Version</a></li>-->
</ul>

<p>
Xidel is a command line tool to download and extract data from html/xml pages.<br><br>

<h2><div><a name="news">News</a></div></h2>
<dl>
<dt><b>2012-11-06</b>: New release</dt>
<dd style="margin-top:8px; margin-bottom: 8px">The 0.6 release adds XQuery support, the <code>form</code> and <code>match</code> functions, improves the Windows commandline interface, merges the two old cgi services to a single one and fixes several interpreter bugs</dd>
<dt><b>2012-09-05</b>: Initial release</dt>
<dd style="margin-top:8px; margin-bottom: 8px">First release of the VideLibri backend as stand-alone cli tool without VideLibri</dd>
</dl> 
<br>

<h2><div><a name="features">Features</a></div></h2>

It supports:
 
<ul class="xidelfeatures">
<li>Extract expressions:<ul>
  <li>CSS 3 Selectors: to extract simple elements</li>
  <li>XPath 2: to extract values and calculate things with them</li>
  <li>XQuery 1: to  create new documents from the extracted values</li> 
  <li>Templates: to extract several expressions in an easy way using a annotated version of the page for pattern-matching</li>
</ul></li>
<li>Following:<ul>
  <li>HTTP Codes: Redirections like 30x are automatically followed, while keeping things like cookies </li>
  <li>Links: It can follow all links on a page as well as some extracted values </li>
</ul></li>
<li>Output formats:<ul>
  <li>Adhoc: just prints the data in a human readable format</li>
  <li>JSON: encodes the data in a json (as array of all pages of objects with the extracted values as properties ) </li>
  <li>XML: encodes the data similarly in a xml  </li>
</ul></li>
<li>Connections: HTTP / HTTPS as well as local files or stdin</li>
<li>Systems: Windows (using wininet), Linux (using synapse+openssl), Mac (? with synapse, not tested) </li>
</ul>

<h2><div><a name="examples">Examples</a></div></h2>

<ol class="examples">
<li>
Print all urls found by a google search.<br><br>
  <code>xidel http://www.google.de/search?q=test --extract "//a/filter(@href, 'url[?]q=([^&]+)&', 1)[. != '']"</code>
</li>
<li>
Print the title of all pages found by a google search and download them: <br><br>
  <code>xidel http://www.google.de/search?q=test --follow "//a/filter(@href, 'url[?]q=([^&]+)&', 1)[. != '']" --extract //title --download '$host;/'</code>
</li>
<li>
Generally follow all links on a page and print the titles of the linked pages:<ul>
  <li>With CSS: <code>xidel http://example.org -f "css('a')" -e "css('title')"</code> </li>
  <li>With XPath: <code>xidel http://example.org -f //a -e //title</code> </li>
  <li>With Templates: <code>xidel http://example.org -f "&lt;a>{.}&lt;/a>*" -e "&lt;title>{.}&lt;/title>"</code> </li>
</ul></li>
<li>Another template example: <br><br>
   If you have an example.xml file like <code>&lt;x>&lt;foo>ood&lt;/foo>&lt;bar>IMPORTANT!&lt;/bar>&lt;/x></code><br>
   You can read the imporant part like: <code>xidel example.xml -e "&lt;x>&lt;foo>ood&lt;/foo>&lt;bar>{.}&lt;/bar>&lt;/x>"</code><br>
   (and this will also check, if the part with the ood is there, and fail otherwise)
</li>
<li>Calculate something with XPath: <br><br>
  <code>xidel -e "(1 + 2 + 3) * 4 + 5 + 6 + 7"</code>
</li>
<li>Print all newest stackoverflow questions with title and url:<br><br>

   <code>xidel http://stackoverflow.com -e "&lt;A class='question-hyperlink'>{title:=text(), url:=@href}&lt;/A>*"</code>
<li>Print all reddit comments of an user, with html and url:<br><br>
   <code>xidel "http://www.reddit.com/user/username/" --extract "&lt;t:loop>&lt;div class='usertext-body'>&lt;div>{outer-xml(.)}&lt;/div>&lt;/div>&lt;ul class='flat-list buttons'>&lt;a>&lt;t:s>link:=@href&lt;/t:s>permalink&lt;/a>&lt;/ul>&lt;/div>&lt;/div>&lt;/t:loop>" --follow "&lt;a rel='nofollow next'>{.}&lt;/a>?"</code>
</li>
<li>Check if your reddit letter is red; combining CSS, XPath and automatically form evaluation:<br><br>
    <code>xidel http://reddit.com -f "form(css('form.login-form')[1], 'user=$your_username&passwd=$your_password')" -e "css('#mail')/@title"</code>
<li>Use XQuery, to create a html table of odd and even numbers: <br><br>
   Windows: <code>xidel -e "xquery version '1.0'; &lt;table>{for $i in 1 to 1000 return &lt;tr>&lt;td>{$i}&lt;/td>&lt;td>{if ($i mod 2 = 0) then 'even' else 'odd'}&lt;/td>&lt;/tr>}&lt;/table>" --printed-node-format xml </code><br>
   Linux: <code>xidel -e 'xquery version "1.0"; &lt;table>{for $i in 1 to 1000 return &lt;tr>&lt;td>{$i}&lt;/td>&lt;td>{if ($i mod 2 = 0) then "even" else "odd"}&lt;/td>&lt;/tr>}&lt;/table>' --printed-node-format xml</code><br>
   <span style="font-size:75%">(xidel itself supports ' and "-quotes on all platforms, but ' does not escape &lt;&gt; in Windows cmd, and " does not escape $ in the Linux shells)</span>  </li>
</ol>
<br>
You may also want to read the <a href="xidel_readme.txt">readme file of Xidel</a>, the documentation of my <a href="http://benibela.de/documentation/internettools/extendedhtmlparser.THtmlTemplateParser.html">template language</a> and <a href="http://www.benibela.de/documentation/internettools/xquery.TXQueryEngine.html"> XPath 2/XQuery library</a>.



<h2><div><a name="downloads">Downloads</a></div></h2>

The following Xidel downloads are available on the <a href="http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/">sourceforge download page</a>: <br><br>

<table class="downloadTable">
                        <tr><th>Operating System</th><th>Filename</th><th>Size</th></tr>
                        <tr><td>Windows: 32 Bit</td><td><a href="http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/xidel-0.6.win32.zip/download">
                        xidel-0.6.win32.zip</a></td><td>394.6 kB</td></tr><tr><td>Universal Linux: 64 Bit</td><td><a href="http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/xidel-0.6.linux64.tar.gz/download">
                        xidel-0.6.linux64.tar.gz</a></td><td>815.3 kB</td></tr><tr><td>Universal Linux: 32 Bit</td><td><a href="http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/xidel-0.6.linux32.tar.gz/download">
                        xidel-0.6.linux32.tar.gz</a></td><td>684.4 kB</td></tr><tr><td>Source:</td><td><a href="http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/xidel-0.6.src.tar.gz/download">
                        xidel-0.6.src.tar.gz</a></td><td>1.1 MB</td></tr><tr><td>Debian: 64 Bit</td><td><a href="http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/xidel_0.6-1_amd64.deb/download">
                        xidel_0.6-1_amd64.deb</a></td><td>795.7 kB</td></tr><tr><td>Debian: 32 Bit</td><td><a href="http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/xidel_0.6-1_i386.deb/download">
                        xidel_0.6-1_i386.deb</a></td><td>673.6 kB</td></tr></table>


                        
                        <!-- Table generated by: 
                        xidel http://sourceforge.net/projects/videlibri/files/Xidel/Xidel%200.6/  --extract-kind=xquery  -e 'concat("The following Xidel downloads are available on the <a href=&quot;$url;&quot;>sourceforge download page</a>: <br><br>")' -e 'declare function verboseName($n){ concat ( if (contains($n, "win")) then "Windows: " else if (contains($n, "linux")) then "Universal Linux: " else if (contains($n, ".deb")) then "Debian: " else if (contains($n, "src")) then "Source:" else "", if (contains($n, "32") or contains($n, "386")) then "32 Bit" else if (contains($n, "64"))then "64 Bit" else ""  )   };            
                        <table class="downloadTable">
                        <tr><th>Operating System</th><th>Filename</th><th>Size</th></tr>
                        { for $link in match(<TABLE id="files_list"><t:loop><TR class="file warn"><TH><A class="name">{{link := object(), link.verboseName := verboseName(.), link.a := .}}</A></TH><td/><td>{{link.size := .}}</td></TR></t:loop></TABLE>, /).link 
                          order by $link.verboseName descending 
                          return <tr><td>{$link.verboseName}</td><td><a href="{$link.a/@href}">{$link.a/text()}</a></td><td>{$link.size/text()}</td></tr>}</table>'     --printed-node-format xml -->
<br>Usually you can just extract the zip/deb and call Xidel, or copy it somewhere in your PATH, <br>
because it consists of a single binary without any external dependencies, except the standard system libraries (i.e. Windows API or respectively libc).<br>
However, for https connections on Linux openssl and libcrypto (or even openssl-dev?) are also required.
<br><br>

You can also test it <a href="/cgi-bin/xidelcgi">online on a webpage</a> or directly by sending a request to the cgi service like <a href="/cgi-bin/xidelcgi?data=&lt;html>&lt;title>foobar&lt;/title>&lt;/html>&extract=//title&raw=true" rel="nofollow"><code>http://videlibri.sourceforge.net/cgi-bin/xidelcgi?data=&lt;html>&lt;title>foobar&lt;/title>&lt;/html>&extract=//title&raw=true</code></a>.<br><br>

The source is stored in a <a href="http://videlibri.hg.sourceforge.net/hgweb/videlibri/videlibri/">mercurial repository</a> together with the VideLibri source.<br>
The  program is written in FreePascal/Lazarus, so you need these to compile it. First open the internettools.lpk (in the components/pascal directory) in Lazarus, compile and install it there. On Linux that also requires the synapse library, if you do not have it already installed, you should add the components/pascal/import/synapse directory to the package search path.    Then open xidel.lpi (in the programs/internet/xidel directory),  select possibly your platform/OS in the project settings, and compile it.<br>
It should be possible to compile it on Mac.
<br>

<h2><div><a name="script">Xidelscript</a></div></h2>

<p>There is also a Greasemonkey script to automatically generate templates by selecting the interesting values on a webpage. The script intercepts the selection and marks the elements as shown in the screenshots below: 
<br>
<center><a href="img/reddit.png"><img src="img/reddit_tiny.png" title="Selections that create a template to read the reddit frontpage (extracting title/author/time of each link)" alt="Selections that create a template to read the reddit frontpage (extracting title/author/time of each link)"/></a> 
<a href="img/stackoverflow.png"><img src="img/stackoverflow_tiny.png" title="Selections that create a template to read the newest stackoverflow question. (votes/title/author are extracted for each of the questions) " alt="Selections that create a template to read the newest stackoverflow question. (votes/title/author are extracted for each of the questions) "/></a></center>
<br>
<p>You can find the script in the mercurial repository or on <a href="http://userscripts.org/scripts/show/144991">userscripts.org</a> with a detailed description.

<br><br><br><br>



<h2><div><a id="contact">Contact</a></div></h2>
Autor: Benito van der Zander, <a href="benito_NOSPAM_benibela.de">benito_NOSPAM_benibela.de</a>, <a href="http://www.benibela.de/index_en.html">www.benibela.de</a><br>
<div id="sf-logo"><a href="http://sourceforge.net/projects/videlibri"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=359854&amp;type=1" width="125" height="37" border="0" alt="SourceForge.net Logo" /></a></div>

<!-- Piwik -->
<script type="text/javascript">
var pkBaseURL = (("https:" == document.location.protocol) ? "https://sourceforge.net/apps/piwik/videlibri/" : "http://sourceforge.net/apps/piwik/videlibri/");
document.write(unescape("%3Cscript src='" + pkBaseURL + "piwik.js' type='text/javascript'%3E%3C/script%3E"));
</script><script type="text/javascript">
try {
var piwikTracker = Piwik.getTracker(pkBaseURL + "piwik.php", 1);
piwikTracker.trackPageView();
piwikTracker.enableLinkTracking();
} catch( err ) {}
</script><noscript><p><img src="http://sourceforge.net/apps/piwik/videlibri/piwik.php?idsite=1" style="border:0" alt=""/></p></noscript>
<!-- End Piwik Tag -->
</body>
</html>
