program xidelcgi;

{$mode objfpc}{$H+}

uses
  xidelbase, simplehtmltreeparser,
  rcmdlinecgi, {utf8tools, }sysutils, strutils, math, bbutils, extendedhtmlparser, xquery.internals.common, xidelcrt,
  baseunix
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


    ExampleTemplate:string = '<!--'#13#10+
                             '   example for pattern matching'#13#10+
                             '   (finding one table with all its rows)'#13#10+
                             '-->'#13#10+
                             '<table id="t2">'#13#10+
                             '<tr><td>{$col:=.}</td></tr>+'#13#10+
                             '</table>';


    ExampleCSS: string = '#t2 tr td:first-child';

    ExampleXPath: string = 'id("t2") / tbody / tr / td[1]';

    ExampleXQuery1: string = 'xquery version "1.0";'#13#10'declare function local:test($table as element()){'#13#10 +
                              '  $table / tbody / tr / td[1]'#13#10'};'#13#10+
                              'local:test(id("t2"))';
    ExampleXQuery3_0: string = 'xquery version "3.0";'#13#10'declare function local:test($table as element()){'#13#10 +
                              '  $table / tbody / tr / td[1]'#13#10'};'#13#10+
                              'local:test(id("t2"))';
    ExampleXQuery3_1: string = 'xquery version "3.1";'#13#10'declare function local:test($table as element()){'#13#10 +
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

    BASEURL = 'https://www.videlibri.de/';
    CANONICALURL  = BASEURL+'cgi-bin/xidelcgi';

var
  wasRaw: Boolean = false;
  permalink, rawpermalink: String;

procedure w(const s: string);
begin
  xidelcrt.wln(s);
end;

function extractKindToString(kind: TExtractionKind): string;
begin
  case kind of
    ekAuto: exit('auto');
    //ekDefault: exit('default');
    ekXPath2: exit('xpath2');
    ekXPath3_0: exit('xpath3');
    ekXPath3_1: exit('xpath3_1');
    ekXPath4_0: exit('xpath4_0');
    ekPatternHTML: exit('html-pattern');
    ekPatternXML: exit('xml-pattern');
    ekCSS: exit('css');
    ekXQuery1: exit('xquery1');
    ekXQuery3_0: exit('xquery3');
    ekXQuery3_1: exit('xquery3_1');
    ekXQuery4_0: exit('xquery4_0');
    else exit('auto');
  end;
end;

var
  oldinoutfunc, oldflushfunc: CodePointer;

Procedure HTMLEscapedFileWriteFunc(var t:TextRec);
type
  FileFunc = Procedure(var t : TextRec);
  procedure writeEscaped;
  var helper: TXHTMLStrBuilder;
    buffer: string;
    bufferptr, bufferend: PChar;
    size: integer;
  begin
    helper.init(@buffer, 2*t.bufpos);
    helper.appendHTMLText(@t.bufptr^[0], t.bufpos);
    helper.final;
    bufferptr := pchar(buffer);
    bufferend := bufferptr + length(buffer);
    while bufferptr < bufferend do begin
      size := min(t.bufsize, bufferend - bufferptr);
      move(bufferptr^, t.bufptr^, size);
      t.bufpos := size;
      FileFunc(oldinoutfunc)(t);
      bufferptr += size;
    end;
  end;

var
  needescape: Boolean;
  i: Integer;
begin
  needescape := false;
  for i := 0 to t.bufpos - 1 do begin
    needescape := t.bufptr^[i] in ['<','>','&'];
    if needescape then break;
  end;
  //writeln(stderr, needescape, ' ', t.bufpos);
  if not needescape then FileFunc(oldinoutfunc)(t)
  else writeEscaped;
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
    'xpath', 'xpath2', 'xpath3', 'xpath3.0', 'xpath3.1', 'xpath4.0': exit(ExampleXPath);
    'xquery1': exit(ExampleXQuery1);
    'xquery', 'xquery3', 'xquery3.0': exit(ExampleXQuery3_0);
    'xquery3.1', 'xquery4.0': exit(ExampleXQuery3_1);
    'css': exit(ExampleCSS);
    {'template', 'auto':} else exit(ExampleTemplate);
    end;
  end;

  function kind(t, n: string): string;
  begin
    result := '<input type="radio" name="extract-kind" value="'+t+'"';
    if (mycmdline.readString('extract-kind') = t) or
       ((mycmdline.readString('extract-kind') = 'xpath') and (t = 'xpath3.0')) or
       ((mycmdline.readString('extract-kind') = 'xquery') and (t = 'xquery3.0'))  then result += ' checked';
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
  outputHeader := '';
  setOutputFileName('stdout:///', mycmdline);

  if (mycmdline.readFlag('case-sensitive')) then
    xqueryDefaultCollation:='http://www.w3.org/2005/xpath-functions/collation/codepoint';

  if mycmdline.readFlag('raw') then begin
    case mycmdLine.readString('output-format') of
      //'xml', 'xml-wrapped': w('Content-Type: application/xml');
      //'html': w('Content-Type: text/html');
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

  w('<!DOCTYPE html><html><head>');
  w('<title>XPath 3.1 / XQuery 3.1 / CSS 3 Selector / JSONiq Online Tester</title>');
  w('<meta name="description" content="Here you can run an XPath/XQuery 3.1 query against some HTML/XML/JSON data. It supports standard queries and extensions like JSONiq or pattern matching.">');
  w('<link rel="stylesheet" href="../codemirror/codemirror.css">');
  w('<link rel="stylesheet" href="https://code.jquery.com/ui/1.10.2/themes/smoothness/jquery-ui.css">');
  w('<link rel="stylesheet" type="text/css" href="'+BASEURL+'/cgi.css" />');
  w('<link rel="canonical" href="'+CANONICALURL+'" />');
  w('<meta charset="utf-8" /> ');
  w('<script src="'+BASEURL+'/cgi.js"></script>');
  w('</head><body onload="init()">');
  w('<h1>Pattern matching / XPath 3.1 / XQuery 3.1 / CSS 3 Selector Online Tester</h1>');
  w('(You can find the documentation below)<br><br>');
  w('<form method="POST" action="./xidelcgi">');
  w('<div id="html"><div class="pretextoptions">'+select('input-format', 'HTML/XML/JSON-Input file', ['auto', 'html', 'xml', 'xml-strict', 'json'])+'</div>'
    + '<textarea name="data" rows="18" cols="80"  >'+xmlStrEscape(IfThen(mycmdline.readString('data') <> '', mycmdline.readString('data'), ExampleHTML))+'</textarea></div>');
  w('<div id="template"><div class="pretextoptions" style="position:relative"><div style="position: absolute; bottom: 5px">'+kind('template', 'Pattern matching')+kind('xpath3.1', 'XPath 3.1') + kind('xquery3.1', 'XQuery 3.1')+kind('css', 'CSS 3.0 selectors')+kind('auto', 'Autodetect')+'</div></div>');
  w('<textarea name="extract" rows=18 cols=80 >');
  if mycmdline.readString('extract') <> '' then w(xmlStrEscape(mycmdline.readString('extract')))
  else w(example(mycmdline.readString('extract-kind')));
  w('</textarea></div>');
  w('<br><br><input type="submit"> '+checkbox('no-auto-update', 'disable auto refresh')+' <span >' {id="codemirrorspan"} + checkbox('no-highlighting', 'disable syntax highlighting') +'</span>');
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

  w('</span>');
  w('<br>Old languages: ' + kind('xpath2', 'XPath 2.0')+kind('xpath3.0', 'XPath 3.0')+kind('xquery1', 'XQuery 1.0')+kind('xquery3.0', 'XQuery 3.0')+'<br>');
  w('<br>New languages: ' + kind('xpath4.0', 'XPath 4.0')+kind('xquery4.0', 'XQuery 4.0')+'<br>');
  w('</form>');

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
  w('Result of the above expression applied to the above input file:<br>');
  w('<textarea id="result" rows="30" cols="100">');

  if  (mycmdline.readString('data') = '') and (mycmdline.readString('extract') = '') then
    case mycmdline.readString('extract-kind') of
    'template', 'auto', '':  w(ExampleTemplateResult);
    else w(ExampleOtherResult);
    end;

  permalink := CANONICALURL+'?'+TCommandLineReaderCGI(mycmdline).urlEncodeParams;
  rawpermalink := CANONICALURL+'?raw=true&'+TCommandLineReaderCGI(mycmdline).urlEncodeParams;


  flush(xidelOutputFile);

  oldinoutfunc := TextRec(xidelOutputFile).inoutfunc;
  oldflushfunc := TextRec(xidelOutputFile).flushfunc;
  TextRec(xidelOutputFile).inoutfunc := @HTMLEscapedFileWriteFunc;
  TextRec(xidelOutputFile).flushfunc := @HTMLEscapedFileWriteFunc;
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
function link(ref, title: string; desc: string = ''; attribs: string = ''): string;
begin
  result := '&nbsp;&nbsp;&nbsp;&nbsp;<a href="'+ref+'"'+attribs+'>'+title+'</a>'+desc+'.<br>';
end;

begin
  //this is not needed
  flush(xidelOutputFile);
  TextRec(xidelOutputFile).inoutfunc := oldinoutfunc;
  TextRec(xidelOutputFile).flushfunc := oldflushfunc;

  //rest of the page
  w('</textarea><br>');
  w('<a id="permalink" href="'+permalink+'">permalink</a>, ');
  w('<a id="rawpermalink" href="'+rawpermalink+'">result-only</a>');
  //w('</div>');

  w('<br><br><br><hr>');

  //w(cgi.QueryString);

  w('<h2>What is this about?</h2>');
  w('Here you can test pattern matching, CSS 3 selectors, standard XPath/XQuery 3.1, and JSONiq expressions.<br>');
  w('XPath, XQuery, and CSS Selectors are W3C standardized query languages. The new XPath versions, from XPath 2.0 on, should not be confused with the old XPath 1.0. While old XPath can only process sets of XML nodes, XPath 2 can process any kind of data sequence. Modern XPath is a Turing-complete functional programming language.<br>'+
    'JSONiq was a proposed extension to XQuery to query JSON, but with XPath 3.1 supporting JSON, it is rarely needed.<br>'+
    'Pattern matching (also called templates) is my own extension: every node in the pattern is searched in the input. The {} text nodes in the pattern contain XPath expressions that are evaluated with the result being assigned to variables.<br>');
  w('It is a nearly standalone implementation of the standards into the Pascal Internet Tools library written for the VideLibri app.<br>');
  //w('The template example shows the two most basic template commands (read/loop) and copies the first column of a table.'+' .<br>');

  w('<br>You can find more details in the corresponding Pascal unit documentation:<br>');
  w(link('https://benibela.de/documentation/internettools/xquery.TXQueryEngine.html', 'Documentation of the XQuery / XPath / CSS 3 selector implementation'));
  w(link('https://benibela.de/documentation/internettools/extendedhtmlparser.THtmlTemplateParser.html', 'Documentation of the pattern syntax'));
  w('<br>Other related links:<br>');
  w(link('https://www.videlibri.de/xidel.html', 'Xidel', ', the command line version of this XQuery processor for web page downloading/scraping and any kind of XML/HTML/JSON processing'));
  w(link('https://www.benibela.de/documentation/internettools/xqts.html', 'XQuery Test Suite Results'));
  w(link('https://www.benibela.de/sources_en.html#internettools', 'Internet Tools library', ', the Pascal library page'));
  w(link('https://sourceforge.net/p/videlibri/code/ci/tip/tree/', 'Source repository', '', ' rel="nofollow"'));
  w(link('https://github.com/benibela/xidel', 'Github mirror (Xidel excluding library)', '', ' rel="nofollow"'));
//  w(link('https://bitbucket.org/benibela/xidel', 'Bitbucket mirror (Xidel excluding library)', '', ' rel="nofollow"'));


  w('<script>lastQueryEditMode="'+firstExtractionKind+'"; ');
  w('compatibilityOn = ["'+compatibiltiyOptionsOn[1]+'", "'+compatibiltiyOptionsOn[2]+'", "'+compatibiltiyOptionsOn[3]+'"];');
  w('compatibilityOff = ["'+compatibiltiyOptionsOff[1]+'", "'+compatibiltiyOptionsOff[2]+'", "'+compatibiltiyOptionsOff[3]+'"];');
  w('compatibilityChange = ["'+compatibiltiyOptionsChange[1]+'", "'+compatibiltiyOptionsChange[2]+'", "'+compatibiltiyOptionsChange[3]+'"];');
  w('activateCodeMirrors(); </script>');

  w('<div id="sf-logo"><a href="https://sourceforge.net/projects/videlibri"><img src="https://sflogo.sourceforge.net/sflogo.php?group_id=359854&amp;type=1" width="125" height="37" border="0" alt="SourceForge.net Logo" /></a></div>');

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



Function  FPC_SYSC_OPEN       (path : pChar; flags : cInt; Mode: TMode):cInt; external name 'FPC_SYSC_OPEN';
Function  FPC_SYSC_ACCESS (pathname : pChar; aMode : cInt): cInt; external name 'FPC_SYSC_ACCESS';
Function  FPC_SYSC_OPENDIR    (dirname : pChar): pDir;  external name 'FPC_SYSC_OPENDIR';
Function  FPC_SYSC_MKDIR      (path : pChar; Mode: TMode):cInt;  external name 'FPC_SYSC_MKDIR';
Function  FPC_SYSC_UNLINK     (path : pChar): cInt;  external name 'FPC_SYSC_UNLINK';
Function  FPC_SYSC_RMDIR      (path : pChar): cInt; external name 'FPC_SYSC_RMDIR';
Function  FPC_SYSC_RENAME     (old  : pChar; newpath: pChar): cInt;   external name 'FPC_SYSC_RENAME';
Function  FPC_SYSC_FSTAT      (fd : cInt; var sb : stat): cInt; external name 'FPC_SYSC_FSTAT';
Function  FPC_SYSC_STAT       (path: pChar; var buf : stat): cInt;  external name 'FPC_SYSC_STAT';

function fpOpenOverride: integer;
begin
  fpseterrno(ESysEACCES);
  result := -1;
end;


procedure lockdownFileAccess;
  procedure patchExecutable(oldFunction, newFunction: pointer);
  const PAGESIZE = 4096;
  var page: pointer;
    data: array[1..12]  of char = #$48#$b8#$77#$77#$77#$77#$77#$77#$77#$77#$ff#$e0 ; //mov rax, 7*; jmp rax
     //#$48#$c7#$c0#$01#$00#$00#$00#$C3; mov 1, rax; ret
  begin
    move(newFunction, data[3], 8);
    page := pointer(ptruint(oldFunction) and not (PAGESIZE-1));
    Fpmprotect(page, PAGESIZE, PROT_WRITE or PROT_READ or PROT_EXEC );
    Move(data[1], oldFunction^, sizeof(data));
    Fpmprotect(page, PAGESIZE, PROT_EXEC or PROT_READ );
  end;

begin
  patchExecutable(@FPC_SYSC_OPEN, @fpOpenOverride);   //this is the important one. all file reading/writing I know goes through it.
  patchExecutable(@FPC_SYSC_ACCESS, @fpOpenOverride); //blocking this turns "permission denied" to "file not found"
  //do not know if anything uses those functions:
  patchExecutable(@FPC_SYSC_OPENDIR, @fpOpenOverride);
  patchExecutable(@FPC_SYSC_MKDIR  , @fpOpenOverride);
  patchExecutable(@FPC_SYSC_UNLINK , @fpOpenOverride);
  patchExecutable(@FPC_SYSC_RMDIR  , @fpOpenOverride);
  patchExecutable(@FPC_SYSC_RENAME , @fpOpenOverride);
  patchExecutable(@FPC_SYSC_FSTAT  , @fpOpenOverride);
  patchExecutable(@FPC_SYSC_STAT   , @fpOpenOverride);
  patchExecutable(@FpExecv, @fpOpenOverride);
  patchExecutable(@FpExecve, @fpOpenOverride);
end;

begin
  lockdownFileAccess;

  //writeln(output,'Content-Type: text/plain');
  //writeln(output,'');
  //flush(output);
  xidelbase.cgimode := true;
  xidelbase.allowInternetAccess := false;
  xidelcrt.allowFileAccess := false;
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

