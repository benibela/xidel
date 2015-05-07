program xidelsoap;

{$mode objfpc}{$H+}

uses
  xidelbase, simplehtmltreeparser,
  rcmdlinecgi, utf8tools, sysutils, strutils, bbutils
  { you can add units after this };

const ExampleHTML: string = '<html><body>'#13#10+
                            '<table id="t1"><tbody>'#13#10+
                            '<tr><td>Hello</td></tr>'#13#10+
                            '</tbody></table>'#13#10+
                            '<table id="t2"><tbody>'#13#10+
                            '<tr><td>123</td><td>other</td></tr>'#13#10+
                            '<tr><td>foo</td><td>columns</td></tr>'#13#10+
                            '<tr><td>bar</td><td>are</td></tr>'#13#10+
                            '<tr><td>xyz</td><td>ignored</td></tr>'#13#10+
                            '</tbody></table>'#13#10+
                            '</body></html>';


    ExampleTemplate:string = '<table id="t2">'#13#10+
                             '<template:loop>'#13#10+
                             '<tr><td>{col:=text()}</td></tr>'#13#10+
                             '</template:loop>'#13#10+
                             '</table>';

    ExampleCSS: string = '#t2 tr td:first-child';

    ExampleXPath: string = 'id("t2") / tbody / tr / td[1]';

    ExampleXQuery: string = 'xquery version "1.0";'#13#10'declare function local:test($table as element()){'#13#10 +
                              '  $table / tbody / tr / td[1]'#13#10'};'#13#10+
                              'local:test(id("t2"))';

    ExampleTemplateResult: string =
      'col: 123'#13#10 +
      'col: foo'#13#10 +
      'col: bar'#13#10 +
      'col: xyz';


    ExampleOtherResult: string =
      '123'#13#10 +
      'foo'#13#10 +
      'bar'#13#10 +
      'xyz';


var
  wasRaw: Boolean = false;
  permalink, rawpermalink: String;

procedure w(const s: string);
begin
  writeln(s);
end;

function extractKindToString(kind: TExtractionKind): string;
begin
  case kind of
    ekAuto: exit('auto');
    ekXPath2: exit('xpath2');
    ekXPath3: exit('xpath3');
    ekTemplate: exit('template');
    ekCSS: exit('css');
    ekXQuery1: exit('xquery1');
    ekXQuery3: exit('xquery3');
    else exit('auto');
  end;
end;

type

{ TCommandLineReaderBreaker }

 TCommandLineReaderBreaker = class(TCommandLineReaderCGI)
  procedure setString(const n,v: string);
  procedure setFlag(const n: string; v: boolean);
end;

var firstExtractionKind: string;

procedure printPre(extractionKind: TExtractionKind);
  function example(t: string): string;
  begin
    if (t = mycmdline.readString('extract-kind')) and (mycmdline.readString('extract') <> '') then
      exit(mycmdline.readString('extract'));
    case t of
    'xpath', 'xpath2', 'xpath3': exit(ExampleXPath);
    'xquery', 'xquery1', 'xquery3': exit(ExampleXQuery);
    'css': exit(ExampleCSS);
    {'template', 'auto':} else exit(ExampleTemplate);
    end;
  end;

  function kind(t, n: string): string;
  begin
    result := '<input type="radio" name="extract-kind" value="'+t+'"';
    if (mycmdline.readString('extract-kind') = t) or
       ((mycmdline.readString('extract-kind') = 'xpath') and (t = 'xpath2')) or
       ((mycmdline.readString('extract-kind') = 'xquery') and (t = 'xquery1'))  then result += ' checked';
    result += ' onclick="changeexample('''+t+''', '''  +  StringsReplace(example(t), ['\', #13#10, '''', '&', '"',  '<', '>'], ['\\', '\n', '\''', '&amp', '&quot;', '&lt;', '&gt;'], [rfReplaceAll]) +  '''); update();"';
    result += '/> '+ n;
  end;
  function checkbox(t, n: string): string;
  begin
    result := '<input type="checkbox" name="'+t+'" value="true"';
    if mycmdline.readFlag(t) then result += ' checked';
    result += '/> '+ n;
  end;
  function select(t, n: string; list: array of string): string;
  var
    cur: String;
    i: Integer;
    s: Integer;
  begin
    if n <> '' then n += ': ';
    result := n + '<select name="'+t+'"/>';
    cur := mycmdline.readString(t);

    s := -1;
    for i := 0 to high(list) do if list[i] = cur then begin s := i; break; end;
    if s = -1 then
      for i := 0 to high(list) do if lowercase(list[i]) = lowercase(cur) then begin s := i; break; end; //useless

    for i := 0 to high(list) do result += '<option value="'+list[i]+'"'+ifthen(i = s, ' selected') +'>'+list[i]+'</option>' + '<!-- ' + cur + ' -->';
    result += '</select> ';
  end;

begin
  if (mycmdline.readFlag('case-sensitive')) then
    xqueryDefaultCollation:='http://www.w3.org/2005/xpath-functions/collation/codepoint';

  if mycmdline.readFlag('raw') then begin
    case mycmdLine.readString('output-format') of
      'xml', 'xml-wrapped': w('Content-Type: application/xml');
      'html': w('Content-Type: text/html');
      'json', 'json-wrapped': w('Content-Type: application/json');
      {'adhoc':} else w('Content-Type: text/plain');
    end;
    w('Xidel-Detected-Extraction-Kind: '+extractKindToString(extractionKind));
    w('');
    wasRaw := true;
    exit;
  end;


  if mycmdline.readString('extract-kind') <> 'auto' then firstExtractionKind := mycmdline.readString('extract-kind')
  else if mycmdline.readString('extract') <> '' then firstExtractionKind := extractKindToString(extractionKind)
  else firstExtractionKind:='';


  w('Content-Type: text/html');
  w('');

  w('<html><head>');
  w('<title>Template / XPath 2.0 / XQuery / CSS 3 Selector / JSONiq Online Tester</title>');
  w('<link rel="stylesheet" href="../codemirror/codemirror.css">');
  w('<link rel="stylesheet" href="http://code.jquery.com/ui/1.10.2/themes/smoothness/jquery-ui.css">');
  w('<link rel="stylesheet" type="text/css" href="../cgi.css" />');
  w('<link rel="stylesheet" type="text/css" href="cgi.css" />');
  w('<script src="../cgi.js"></script>');
  w('<script src="cgi.js"></script>');
  w('</head><body onload="init()">');
  w('<h1>Template / XPath 2.0 / XQuery / CSS 3 Selector / JSONiq Online Tester</h1>');
  w('(You can find the documentation below)<br><br>');
  w('<form method="POST" action="./xidelcgi">');
  w('<div id="html">'+select('input-format', 'HTML/XML-Input file', ['auto', 'html', 'xml', 'xml-strict'])
    + '<br><textarea name="data" rows="18" cols="80"  >'+xmlStrEscape(IfThen(mycmdline.readString('data') <> '', mycmdline.readString('data'), ExampleHTML))+'</textarea></div>');
  w('<div id="template">'+kind('template', 'Template')+kind('xpath2', 'XPath 2.0')+kind('xquery1', 'XQuery 1.0')+kind('css', 'CSS 3.0 selectors')+kind('auto', 'Autodetect'));
  w('<br><textarea name="extract" rows=18 cols=80 >');
  if mycmdline.readString('extract') <> '' then w(xmlStrEscape(mycmdline.readString('extract')))
  else w(example(mycmdline.readString('extract-kind')));
  w('</textarea></div>');
  w('<br><br><input type="submit"></input> '+checkbox('no-auto-update', 'disable auto refresh')+' <span >' {id="codemirrorspan"} + checkbox('no-highlighting', 'disable syntax highlighting') +'</span>');
  w('<br><span class="options"><b>Output Options</b>: ');
  w(  select('printed-node-format', 'Node format:', ['text', 'xml', 'html']) +  select('output-format', 'Output format:', ['adhoc', 'html', 'xml', 'xml-wrapped', 'json-wrapped', 'bash', 'cmd']));
  w(checkbox('print-type-annotations', 'Show types') + checkbox('hide-variable-names', 'Hide variable names') );
  w('<br><b>Compatibility</b>: '+select('compatibility', '', ['Standard XQuery', 'Standard XQuery+JSONiq', 'Enable all extensions', 'Custom'])
    + '<span id="compatibilityOptions">'+ checkbox('no-extended-strings', 'Disable extended strings (e.g. x"{$varname}") ')
    + checkbox('no-json', 'Disable JSONiq (e.g. {"a": 1}("a"))') + checkbox('no-json-literals', 'Disable JSONiq literals (true,false,null)')
    + checkbox('only-json-objects', 'Only JSON types in objects (e.g. {"a": null} != {"a": ()})')
    + select('dot-notation', '&nbsp;&nbsp;&nbsp; Allow dot notation (e.g. {"a": 1}.a): ', ['off', 'unambiguous', 'on'])
    + checkbox('strict-type-checking', 'Strict type checking') + checkbox('strict-namespaces', 'Strict namespaces')
    + checkbox('case-sensitive', 'case sensitive'));
  w('</span>');
  w('<br>Incomplete languages/Work in progress: '+kind('xpath3', 'XPath 3')+kind('xquery3', 'XQuery 3'));

  w('</span></form>');

 { w('<script src="../codemirror/codemirror.js"></script>');
  w('<script src="../codemirror/javascript/javascript.js"></script>');
  w('<script src="../codemirror/css/css.js"></script>');
  w('<script src="../codemirror/xml/xml.js"></script>');
  w('<script src="../codemirror/htmlmixed/htmlmixed.js"></script>');}
  w('<script src="../codemirror/codemirror-compressed-js-html-xml-css.js"></script>');
  w('<script src="../codemirror/xquery/xquery.js"></script>');
  w('<script src="../codemirror/jquery-1.9.1.js"></script>');
  w('<script src="../codemirror/jquery-ui-1.10.2.custom.min.js"></script>');

  w('<hr>');
  w('Result of the above expression applied to the above html file:<br>');
  w('<textarea id="result" rows="30" cols="100">');

  if  (mycmdline.readString('data') = '') and (mycmdline.readString('extract') = '') then
    case mycmdline.readString('extract-kind') of
    'template', 'auto', '':  w(ExampleTemplateResult);
    else w(ExampleOtherResult);
    end;

  permalink := 'http://videlibri.sourceforge.net/cgi-bin/xidelcgi?'+TCommandLineReaderCGI(mycmdline).urlEncodeParams;
  rawpermalink := 'http://videlibri.sourceforge.net/cgi-bin/xidelcgi?raw=true&'+TCommandLineReaderCGI(mycmdline).urlEncodeParams;


  flush(stdout);


end;

var compatibiltiyOptionsOn: array[1..3] of string =
    ('no-extended-strings;no-json;no-json-literals;only-json-objects;strict-type-checking;strict-namespaces;case-sensitive',
     'no-extended-strings;only-json-objects;strict-type-checking;strict-namespaces;case-sensitive',
     ''
    );
    compatibiltiyOptionsOff: array[1..3] of string =
    ('',
     'no-json;no-json-literals',
     'no-extended-strings;no-json;no-json-literals;only-json-objects;strict-type-checking;strict-namespaces;case-sensitive'
    );
    compatibiltiyOptionsChange: array[1..3] of string =
    ('dot-notation=off',
     'dot-notation=off',
     'dot-notation=unambiguous'
    );

procedure printPost;
function link(ref, title: string; desc: string = ''): string;
begin
  result := '&nbsp;&nbsp;&nbsp;&nbsp;<a href="'+ref+'">'+title+'</a>'+desc+'.<br>';
end;

begin
  w('</textarea><br>');
  w('<a id="permalink" href="'+permalink+'">permalink</a>, ');
  w('<a id="rawpermalink" href="'+rawpermalink+'">result-only</a>');
  w('</div>');

  w('<br><br><br><hr>');

  //w(cgi.QueryString);

  w('<h2>What is this about?</h2>');
  w('Here you can test html templates, CSS 3 selectors, standard XPath 2 / XQuery and JSONiq expressions.<br>');
  w('It is an example for my Pascal Internet Tools library written for VideLibri and implementing these queries.<br>');
  //w('The template example shows the two most basic template commands (read/loop) and copies the first column of a table.'+' .<br>');

  w('<br>You can find more details in the corresponding unit documentation:<br>');
  w(link('http://benibela.de/documentation/internettools/xquery.TXQueryEngine.html', 'Documentation of the XQuery / XPath / CSS 3 selector implementation'));
  w(link('http://benibela.de/documentation/internettools/extendedhtmlparser.THtmlTemplateParser.html', 'Documentation of the template syntax'));
  w('<br>Other related links:<br>');
  w(link('http://www.benibela.de/sources_en.html#internettools', 'Internet Tools library', ', the library page'));
  w(link('http://www.benibela.de/documentation/internettools/internettoolsxqts.html', 'XQuery Testsuite Results', ', (and <a href="http://www.benibela.de/documentation/internettools/internettoolsxqts_path.html">XPath only results</a>)'));
  w(link('http://videlibri.sourceforge.net/xidel.html', 'Xidel command line tool', ', a litte tool using this library for web page downloading / scraping'));
  w(link('https://sourceforge.net/p/videlibri/code/ci/tip/tree/', 'Source repository'));


  w('<script>lastQueryEditMode="'+firstExtractionKind+'"; ');
  w('compatibilityOn = ["'+compatibiltiyOptionsOn[1]+'", "'+compatibiltiyOptionsOn[2]+'", "'+compatibiltiyOptionsOn[3]+'"];');
  w('compatibilityOff = ["'+compatibiltiyOptionsOff[1]+'", "'+compatibiltiyOptionsOff[2]+'", "'+compatibiltiyOptionsOff[3]+'"];');
  w('compatibilityChange = ["'+compatibiltiyOptionsChange[1]+'", "'+compatibiltiyOptionsChange[2]+'", "'+compatibiltiyOptionsChange[3]+'"];');
  w('activateCodeMirrors(); </script>');

  w('<div id="sf-logo"><a href="http://sourceforge.net/projects/videlibri"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=359854&amp;type=1" width="125" height="37" border="0" alt="SourceForge.net Logo" /></a></div>');
  w('<!-- Piwik -->');
  w('<script type="text/javascript">');
  w('var pkBaseURL = (("https:" == document.location.protocol) ? "https://videlibri.sourceforge.net/piwik/" : "http://videlibri.sourceforge.net/piwik/");');
  w('document.write(unescape("%3Cscript src=''" + pkBaseURL + "piwik.js'' type=''text/javascript''%3E%3C/script%3E"));');
  w('</script><script type="text/javascript">');
  w('try {');
  w('var piwikTracker = Piwik.getTracker(pkBaseURL + "piwik.php", 3);');
  w('piwikTracker.trackPageView();');
  w('piwikTracker.enableLinkTracking();');
  w('} catch( err ) {}');
  w('</script><noscript><p><img src="http://videlibri.sourceforge.net/piwik/piwik.php?idsite=3" style="border:0" alt="" /></p></noscript>');
  w('<!-- End Piwik Tracking Code -->');

  {sl := tstringlist.create;
  cgi.AddResponseLn('reqvar:');
  cgi.GetRequestVarList(sl);
  for i:=0 to sl.Count-1 do
    cgi.AddResponseLn(sl[i]+'<br>');

  cgi.AddResponseLn('cgivar:');
  cgi.GetCGIVarList(sl);
  for i:=0 to sl.Count-1 do
    cgi.AddResponseLn(sl[i]+'<br>');
  sl.free;}

  w('</body></html>');
end;

{ TCommandLineReaderBreaker }



procedure onPostParseCmdLine;
var
  onn: String;
  off: String;
  change: string;
  temp: TStringArray;
  i: Integer;
begin
  i := 1;
  case lowercase(mycmdline.readString('compatibility')) of
    'standard xquery': i := 1;
    'standard xquery+jsoniq': i := 2;
    'enable all extensions': i := 3;
    else exit; //'Custom'
  end;

  onn := compatibiltiyOptionsOn[i];
  off := compatibiltiyOptionsOff[i];
  change := compatibiltiyOptionsChange[i];

  temp := strSplit(onn, ';', false);
  for i := 0 to high(temp) do TCommandLineReaderBreaker(mycmdline).setFlag(temp[i],true);
  temp := strSplit(off, ';', false);
  for i := 0 to high(temp) do TCommandLineReaderBreaker(mycmdline).setFlag(temp[i],false);
  temp := strSplit(change, ';', false);
  for i := 0 to high(temp) do TCommandLineReaderBreaker(mycmdline).setString(strSplit(temp[i], '=')[0],strSplit(temp[i], '=')[1]);
end;

procedure TCommandLineReaderBreaker.setString(const n, v: string);
begin
  findProperty(n)^.strvalue:=v;
end;

procedure TCommandLineReaderBreaker.setFlag(const n: string; v: boolean);
begin
  findProperty(n)^.flagvalue:=v;
end;

begin
  xidelbase.cgimode := true;
  xidelbase.allowInternetAccess := false;
  xidelbase.mycmdline := TCommandLineReaderCGI.create;




  mycmdline.beginDeclarationCategory('CGI Only options');
  mycmdline.declareFlag('raw', 'Only prints the output of the expression');
  mycmdline.declareFlag('no-auto-update', 'No automatical javascript based autoupdate');
  mycmdline.declareFlag('no-highlighting', 'No syntax highlighting');
  mycmdline.declareFlag('case-sensitive', 'Case sensitive');
  mycmdline.declareString('compatibility', 'XQuery compatibility options', 'Enable all extensions');

  xidelbase.onPostParseCmdLine := @onPostParseCmdLine;
  xidelbase.onPreOutput := @printPre;

  xidelbase.perform;

  if not wasRaw then printPost;
end.

