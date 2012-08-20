unit xidelbase;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  extendedhtmlparser,  pseudoxpath, sysutils, bbutils, simplehtmltreeparser, multipagetemplate,
  internetaccess,
  rcmdline
  ;

var cgimode: boolean = false;
    allowInternetAccess: boolean = true;
    mycmdline: TCommandLineReader;
    defaultUserAgent: string = 'Mozilla compatible';

    onPrepareInternet: function (const useragent, proxy: string): tinternetaccess;
    onRetrieve: function (const url, postdata: string): string;

procedure perform;

implementation


type TOutputFormat = (ofAdhoc, ofJson, ofXML);
var outputFormat: TOutputFormat;
    firstExtraction: boolean = true;
    outputArraySeparator: array[toutputformat] of string = ('', ', ', '</e><e>');

function joined(s: array of string): string;
var
  i: Integer;
begin
  if length(s) = 0 then exit('');
  result := s[0];
  for i:=1 to high(s) do result := result + LineEnding + s[i];
end;



type

{ TProcessingRequest }

{ TExtraction }

TExtraction = object
 extract: string;
 extractExclude, extractInclude: TStringArray;
 extractKind: (ekAuto, ekXPath, ekTemplate, ekCSS, ekMultipage);

 defaultName: string;
 printVariables: set of (pvLog, pvCondensedLog, pvFinal);
 printTypeAnnotations,  hideVariableNames, printNodeXML: boolean;

 quiet: boolean;

 procedure setExtractKind(v: string);

 procedure initFromCommandLine(cmdLine: TCommandLineReader);
 procedure mergeWithObject(obj: TPXPValueObject);

 procedure setVariables(v: string);

 procedure printStatus(s: string);

 procedure printExtractedValue(value: TPXPValue);
 procedure printExtractedVariables(vars: TPXPVariableChangeLog; state: string);
 procedure printExtractedVariables(parser: THtmlTemplateParser);

 procedure pageProcessed(unused: TTemplateReader; parser: THtmlTemplateParser);
end;

 TProcessingRequest = record
  urls: TStringArray;
  urlsLevel: bbutils.TLongintArray;

  extractions: array of TExtraction;

  follow: string;
  followExclude, followInclude: TStringArray;
  stepLevel, followMaxLevel: integer;

  wait: Extended;
  userAgent: string;
  proxy: string;
  post: string;

  quiet: boolean;

  outputEncoding: TEncoding;
  printVariablesTime: set of (pvtImmediate, pvtFinal);

  procedure printStatus(s: string);

  procedure initFromCommandLine(cmdLine: TCommandLineReader; level: integer);
  procedure mergeWithObject(obj: TPXPValueObject);

  procedure setVariablesTime(v: string);

  procedure deleteUrl0;
  procedure addBasicValueUrl(dest: tpxpvalue; baseurl: string);

end;

type EInvalidArgument = Exception;

{ TExtraction }

procedure TExtraction.setExtractKind(v: string);
begin
  if extract = '' then exit;
  if striEqual(v, 'auto') then extractKind := ekAuto
  else if striEqual(v, 'xpath') then extractKind:=ekXPath
  else if striEqual(v, 'css') then extractKind:=ekCSS
  else if striEqual(v, 'template') then extractKind:=ekTemplate
  else if striEqual(v, 'multipage') then extractKind:=ekMultipage
  else raise Exception.Create('Unknown kind for the extract expression: '+v);
end;


procedure TExtraction.initFromCommandLine(cmdLine: TCommandLineReader);
var s: string;
begin
  if cmdLine.readString('extract-file') <> '' then extract := strLoadFromFile(cmdLine.readString('extract-file'))
  else extract := cmdLine.readString('extract');
  extract := trim(extract);
  extractExclude := strSplit(cmdLine.readString('extract-exclude'), ',', false);
  extractInclude := strSplit(cmdLine.readString('extract-include'), ',', false);

  setExtractKind(cmdLine.readString('extract-kind'));
  if cmdLine.readString('template-file') <> '' then begin
    extract := strLoadFromFile(cmdLine.readString('template-file'));
    extractKind := ekMultipage;
  end;


  defaultName := cmdLine.readString('default-variable-name');
  printTypeAnnotations:=cmdLine.readFlag('print-type-annotations');
  hideVariableNames := cmdLine.readFlag('hide-variable-names');

  quiet := cmdLine.readFlag('quiet');

  setVariables(cmdLine.readString('print-variables'));

  if cmdLine.readString('printed-node-format') <> '' then begin
    if cmdLine.readString('printed-node-format') = 'xml' then printNodeXML:=true
    else if cmdLine.readString('printed-node-format') = 'text' then printNodeXML:=false
    else raise EInvalidArgument.create('Unknown option: '+cmdLine.readString('printed-node-format'));
  end;
end;

procedure TExtraction.mergeWithObject(obj: TPXPValueObject);
var
  temp: TPXPValue;
begin
  if obj.hasProperty('extract-file', @temp) then extract := temp.asString
  else if obj.hasProperty('extract', @temp) then extract := temp.asString;
  if obj.hasProperty('extract-exclude', @temp) then extractExclude := strSplit(temp.asString, ',', false);
  if obj.hasProperty('extract-include', @temp) then extractInclude := strSplit(temp.asString, ',', false);
  if obj.hasProperty('extract-kind', @temp) then setExtractKind(temp.asString);
  if obj.hasProperty('template-file', @temp)  then begin
    extract := strLoadFromFile(temp.asString);
    extractKind := ekMultipage;
  end;

  if obj.hasProperty('default-variable-name', @temp) then defaultName := temp.asString;
  if obj.hasProperty('print-type-annotations', @temp) then printTypeAnnotations:=temp.asBoolean;
  if obj.hasProperty('hide-variable-names', @temp) then hideVariableNames := temp.asBoolean;

  if obj.hasProperty('print-variables', @temp) then setVariables(temp.asString);

  if obj.hasProperty('quiet', @temp) then quiet := temp.asBoolean;
end;

procedure TExtraction.setVariables(v: string);
var
  tempSplitted: TStringArray;
begin
  printVariables:=[];
  tempSplitted := strSplit(v, ',');
  if arrayIndexOf(tempSplitted, 'log') >= 0 then include(printVariables, pvLog);
  if arrayIndexOf(tempSplitted, 'condensed-log') >= 0 then include(printVariables, pvCondensedLog);
  if arrayIndexOf(tempSplitted, 'final') >= 0 then include(printVariables, pvFinal);
end;

{ TProcessingRequest }

procedure TProcessingRequest.printStatus(s: string);
begin
  if length(extractions) = 0 then writeln(stderr, s)
  else extractions[high(extractions)].printStatus(s);
end;

procedure TProcessingRequest.initFromCommandLine(cmdLine: TCommandLineReader; level: integer);
var
  tempSplitted: TStringArray;
  i: Integer;
  s: string;
begin
  if length(extractions) > 0 then
    extractions[high(extractions)].initFromCommandLine(cmdLine);

  for i:=0 to high(extractions) do with extractions[i] do begin
    if extract = '-' then begin
      extract:='';
      while not EOF(Input) do begin
        ReadLn(s);
        extract:=extract+s+LineEnding;
      end;
    end;
    if extractKind = ekAuto then
      if (extract <> '') and (extract[1] = '<') then extractKind:=ekTemplate
      else extractKind:=ekXPath;
  end;

  if cmdLine.readString('follow-file') <> '' then follow := strLoadFromFile(cmdLine.readString('follow-file'))
  else follow := cmdLine.readString('follow');
  follow := trim(follow);
  followExclude := strSplit(cmdLine.readString('follow-exclude'), ',', false);
  followInclude := strSplit(cmdLine.readString('follow-include'), ',', false);
  stepLevel:=level;
  followMaxLevel := cmdLine.readInt('follow-level');

  if allowInternetAccess then begin
    wait := cmdLine.readFloat('wait');
    userAgent := cmdLine.readString('user-agent');
    proxy := cmdLine.readString('proxy');
    post := cmdLine.readString('post');
  end;

  setVariablesTime(cmdLine.readString('print-variables-time'));
  outputEncoding:=strEncodingFromName(cmdLine.readString('output-encoding'));

  urls:=cmdLine.readNamelessFiles();

  if (length(extractions) > 0) and (extractions[high(extractions)].extractKind = ekMultipage) and (length(urls) = 0) then
    arrayAdd(urls, '<empty/>');

  if cmdLine.readString('data') <> '' then arrayAdd(urls, cmdLine.readString('data'));

  setlength(urlsLevel, length(urls));
  for i:= 0 to high(urlsLevel) do urlsLevel[i] := stepLevel;
end;

procedure TProcessingRequest.mergeWithObject(obj: TPXPValueObject);
var
  temp: TPXPValue;
  tempSplitted: TStringArray;
begin
  if length(extractions) > 0 then
    extractions[high(extractions)].mergeWithObject(obj);

  if obj.hasProperty('follow-file', @temp) then follow := strLoadFromFile(temp.asString)
  else if obj.hasProperty('follow', @temp) then follow := temp.asString;
  if obj.hasProperty('follow-exclude', @temp) then followExclude := strSplit(temp.asString, ',', false);
  if obj.hasProperty('follow-include', @temp) then followInclude := strSplit(temp.asString, ',', false);
  if obj.hasProperty('follow-level', @temp) then followMaxLevel := temp.asInteger;

  if obj.hasProperty('wait', @temp) then wait := temp.asDecimal;
  if obj.hasProperty('user-agent', @temp) then userAgent := temp.asString;
  if obj.hasProperty('proxy', @temp) then proxy := temp.asString;
  if obj.hasProperty('post', @temp) then post := temp.asString;

  if obj.hasProperty('print-variables-time', @temp) then setVariablesTime(temp.asString);
  if obj.hasProperty('output-encoding', @temp) then outputEncoding:=strEncodingFromName(temp.asString);


  setlength(urls, 0);
  if obj.hasProperty('follow', @temp) then
    addBasicValueUrl(temp, '');

  if (length(extractions) > 0) and (extractions[high(extractions)].extractKind = ekMultipage) and (length(urls) = 0) then begin
    arrayAdd(urls, '<empty/>');
    arrayAdd(urlsLevel, stepLevel);
  end;
end;

procedure TProcessingRequest.setVariablesTime(v: string);
var
  tempSplitted: TStringArray;
begin
  printVariablesTime:=[];
  tempSplitted := strSplit(v, ',');
  if arrayIndexOf(tempSplitted, 'immediate') >= 0 then include(printVariablesTime, pvtImmediate);
  if arrayIndexOf(tempSplitted, 'final') >= 0 then include(printVariablesTime, pvtFinal);
end;




procedure TProcessingRequest.deleteUrl0;
var
  i: Integer;
begin
  for i:=1 to high(urls) do begin
    urls[i-1] := urls[i];
    urlsLevel[i-1]:=urlsLevel[i];
  end;
  setlength(urls,length(urls)-1);
  setlength(urlsLevel,length(urlsLevel)-1);
end;

procedure TProcessingRequest.addBasicValueUrl(dest: tpxpvalue; baseurl: string);
  procedure activateNewUrl;
  begin
    if length(urlsLevel) > 0 then arrayAdd(urlsLevel, urlsLevel[0])
    else arrayAdd(urlsLevel, stepLevel);
    if (guessType(baseurl) = rtRemoteURL) and (guessType(urls[high(urls)]) = rtFile) then
      urls[high(urls)] := resolveURI(urls[high(urls)], baseurl);
  end;

var
  n: TTreeElement;
  i: Integer;
begin
  case dest.kind of
    pvkUndefined: exit;
    pvkNode: begin
      n := dest.asNode;
      if n = nil then exit;
      if n.typ <> tetOpen then arrayAdd(urls, dest.asString)
      else if SameText(n.value, 'a') then arrayAdd(urls, n.getAttribute('href', ''))
      else if SameText(n.value, 'frame') or SameText(n.value, 'iframe') then arrayAdd(urls, n.getAttribute('src', ''))
      else arrayAdd(urls, n.deepNodeText());
      activateNewUrl;
    end;
    pvkSequence:
      for i:=0 to TPXPValueSequence(dest).seq.Count-1 do
        addBasicValueUrl(TPXPValueSequence(dest).seq[i], baseurl);
    else begin
      arrayAdd(urls, dest.asString);
      activateNewUrl;
    end;
  end;
end;

procedure TExtraction.printStatus(s: string);
begin
  if not quiet then writeln(stderr, s);
end;

procedure TExtraction.printExtractedValue(value: TPXPValue);
var
  i: Integer;
  temp: TPXPValueObject;
begin
  case outputFormat of
    ofAdhoc: begin
      if printTypeAnnotations then write(value.typeName+': ');
      if value is TPXPValueSequence then begin
        for i:=0 to TPXPValueSequence(value).seq.Count-1 do begin
          printExtractedValue(TPXPValueSequence(value).seq[i]);
          if i <> TPXPValueSequence(value).seq.count-1 then writeln;
        end;
      end else if printNodeXML and (value is TPXPValueNode) then
        write(value.asNode.outerXML())
      else if value is TPXPValueObject then begin
        temp := TPXPValueObject(value.clone);
        write('{');
        if temp.values.count > 0 then begin
          write(temp.values.getVariableName(0),': '); printExtractedValue(temp.values.getVariableValue(0));
          for i:=1 to temp.values.count-1 do begin
            write(', ', temp.values.getVariableName(i), ': ');
            printExtractedValue(temp.values.getVariableValue(i));
          end;
          temp.free;
        end;
        write('}');
      end
      else write(value.asString);
    end;
    ofJson: begin
      write(value.jsonSerialize(not printNodeXML));
    end;
    ofXML: begin
      write(value.xmlSerialize(not printNodeXML, 'seq', 'e', 'object'));
    end;
  end;
end;

procedure TExtraction.printExtractedVariables(parser: THtmlTemplateParser);
begin
  if pvFinal in printVariables then
    printExtractedVariables(parser.variables, '** Current variable state: **');

  if pvLog in printVariables then
    printExtractedVariables(parser.variableChangeLog, '** Current variable state: **');

  if pvCondensedLog in printVariables then
    printExtractedVariables(parser.VariableChangeLogCondensed, '** Current variable state: **');
end;

procedure followTo(dest: TPXPValue); forward;

procedure TExtraction.pageProcessed(unused: TTemplateReader; parser: THtmlTemplateParser);
var
  i: Integer;
begin
  if firstExtraction then begin
    firstExtraction := false;
    if outputFormat = ofXML then writeln('<e>');
  end else writeln(outputArraySeparator[outputFormat]);

  printExtractedVariables(parser);

  for i := 0 to parser.variableChangeLog.count-1 do
    if parser.variableChangeLog.getVariableName(i) = '_follow' then
      followTo(parser.variableChangeLog.getVariableValue(i));
end;


procedure TExtraction.printExtractedVariables(vars: TPXPVariableChangeLog; state: string);
  function acceptName(n: string): boolean;
  begin
    result := ((length(extractInclude) = 0) and (arrayIndexOf(extractExclude, n) = -1)) or
              ((length(extractInclude) > 0) and (arrayIndexOf(extractInclude, n) > -1));
  end;

var
  i: Integer;
  tempUsed: array of boolean;
  first: boolean;
  values: TPXPValueArray;
  j: Integer;
begin
  printStatus(state);
  case outputFormat of
    ofAdhoc: begin
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if not hideVariableNames then write(vars.Names[i] + ': ');
           printExtractedValue(vars.getVariableValue(i));
           writeln;
         end;
    end;
    ofJson:
      if hideVariableNames then begin
        write('[');
        first := true;
        for i:=0 to vars.count-1 do begin
          if acceptName(vars.Names[i]) then begin
            if first then first := false
            else writeln(', ');
            printExtractedValue(vars.getVariableValue(i));
          end;
        end;
        writeln(']');
      end else begin
        first := true;
        writeln('{');
        setlength(tempUsed, vars.count);
        FillChar(tempUsed[0], sizeof(tempUsed[0])*length(tempUsed), 0);
        for i:=0 to vars.count-1 do begin
          if tempUsed[i] then continue;
          if acceptName(vars.Names[i]) then begin
            if first then first := false
            else writeln(',');
            write(jsonStrEscape(vars.Names[i]) + ': ');
            values := vars.getAllVariableValues(vars.Names[i]);
            if length(values) = 1 then printExtractedValue(values[0])
            else begin
              write('[');
              printExtractedValue(values[0]);
              for j:=1 to high(values) do begin
                write(', ');
                printExtractedValue(values[j]);
              end;
              write(']');
            end;
          end;
          for j := i + 1 to vars.count-1 do
            if vars.Names[i] = vars.Names[j] then tempUsed[j] := true;
        end;
        writeln();
        writeln('}');
    end;
    ofXML: begin
      if hideVariableNames then begin
        write('<seq>');
        first := true;
        for i:=0 to vars.count-1 do begin
          if acceptName(vars.Names[i]) then begin
            if first then begin first := false; write('<e>');end
            else write('</e><e>');
            printExtractedValue(vars.getVariableValue(i));
          end;
        end;
        if not first then write('</e>');
        writeln('</seq>');
      end else begin
        writeln('<object>');
        for i:=0 to vars.count-1 do
           if acceptName(vars.Names[i])  then begin
             write('<'+vars.Names[i] + '>');
             printExtractedValue(vars.getVariableValue(i));
             writeln('</'+vars.Names[i] + '>');
           end;
        writeln('</object>');
      end;
    end;
  end;
end;

var
  baserequests, requests: array of TProcessingRequest;

procedure followTo(dest: TPXPValue);
var
  n: TTreeElement;
begin
  case dest.kind of
    pvkObject: begin //this can't imho be in addBasicValueUrl, because you can't reliable call a method on a  record in an array that will be modified
      SetLength(requests, length(requests) + 1);
      requests[high(requests)] := requests[0];
      requests[high(requests)].mergeWithObject(dest as TPXPValueObject);
    end;
    else begin
      if (length(requests) > 1) and (requests[0].stepLevel = requests[1].stepLevel - 1) then
        requests[1].addBasicValueUrl(dest, requests[0].urls[0])
       else
        requests[0].addBasicValueUrl(dest, requests[0].urls[0]);
    end;
  end;
end;



type

{ THtmlTemplateParserBreaker }

 THtmlTemplateParserBreaker = class(THtmlTemplateParser)
  procedure parseHTMLSimple(html,uri: string);
end;

 TTemplateReaderBreaker = class(TTemplateReader)
   constructor create();
   procedure setTemplate(atemplate: TMultiPageTemplate);
   procedure perform;
   procedure selfLog(sender: TTemplateReader; logged: string; debugLevel: integer);
 end;

var htmlparser:THtmlTemplateParserBreaker;
    i: Integer;
    temp: TStringArray;
    j: Integer;
    data: string;

    alreadyProcessed: TStringList;
    xpathparser: TPseudoXPathParser;
    multipage: TTemplateReaderBreaker;
    multipagetemp: TMultiPageTemplate;

{ TMultiPageTemplateBreaker }


{ THtmlTemplateParserBreaker }

procedure THtmlTemplateParserBreaker.parseHTMLSimple(html, uri: string);
begin
  FHTML.trimText := FTrimTextNodes = ttnWhenLoading;
  FHtmlTree := FHTML.parseTree(html, uri);

  //encoding trouble
  FHtmlTree.setEncoding(outputEncoding,true,true);

  if FTrimTextNodes = ttnWhenLoadingEmptyOnly then
    FHTML.removeEmptyTextNodes(true);
end;

constructor TTemplateReaderBreaker.create;
begin
  onLog:=@selfLog;
end;

procedure TTemplateReaderBreaker.setTemplate(atemplate: TMultiPageTemplate);
begin
  if atemplate = nil then begin
    atemplate.free;
    template:=nil;
    exit;
  end;
  inherited setTemplate(atemplate);
end;

procedure TTemplateReaderBreaker.perform;
begin
  if length(template.actions) = 0 then raise Exception.Create('Template contains no actions!'+LineEnding+'A Multipage template should look like <action>  <page url="..."> <post> post data </post> <template> single page template </template> </page> </action> ');
  performAction(template.actions[0]);
end;

procedure TTemplateReaderBreaker.selfLog(sender: TTemplateReader; logged: string; debugLevel: integer);
begin
  if debugLevel <> 0 then exit;
  writeln(stderr, logged);
end;

procedure displayError(e: EHTMLParseException);
  procedure sayln(s: string);
  begin
    if cgimode then writeln(s)
    else writeln(stderr, s);
  end;
  procedure say(s: string);
  begin
    if cgimode then write(s)
    else write(stderr, s);
  end;

begin
  case outputFormat of
    ofJson: begin
      sayln('{"_error": {');
      sayln('"_message": '+jsonStrEscape(e.Message)+', ');
      sayln('"_partial-matches": [');
      temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
      if length(temp) > 0 then
        say(jsonStrEscape(temp[j]));
      for j := 1 to high(temp) do  say (', '+LineEnding+jsonStrEscape(temp[j]));
      sayln(']}');
      sayln('}');
      if cgimode then
        sayln(']');
    end;
    ofXML: begin
      sayln('<error>');
      sayln('<message>'+xmlStrEscape(e.Message)+'</message>');
      sayln('<partial-matches><![CDATA[');
      temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
      for j := 0 to high(temp) do  sayln( temp[j]);
      sayln(']]></partial-matches>');
      sayln('</error>');
      if cgimode then
        sayln('</seq>');
    end;
    ofAdhoc: begin
      sayln( 'Parsing error:');
      sayln( e.Message);
      sayln( 'Partial matches:');
      temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
      for j := 0 to high(temp) do  sayln( temp[j]);
    end;
  end;
  if cgimode then flush(StdOut)
  else flush(stderr);
end;

type

{ TCommandLineReaderBreaker }

TCommandLineReaderBreaker = class(TCommandLineReader)
  procedure clearNameless;
end;

{ TCommandLineReaderBreaker }

procedure TCommandLineReaderBreaker.clearNameless;
begin
  SetLength(nameless, 0);
end;

procedure variableRead(pseudoself: TObject; sender: TObject; const name, value: string);
begin
  if (name = 'follow') or ((name = '') and (length(requests[high(requests)].extractions) > 0))  then begin
    writeln(stderr,name,'=',value);
    requests[high(requests)].initFromCommandLine(TCommandLineReader(sender), length(requests) - 1);
    TCommandLineReaderBreaker(sender).clearNameless;
    SetLength(requests, length(requests) + 1);
  end else if name = 'extract' then begin
    SetLength(requests[high(requests)].extractions, length(requests[high(requests)].extractions) + 1);
    requests[high(requests)].extractions[high(requests[high(requests)].extractions)].initFromCommandLine(TCommandLineReader(sender));
  end;
end;

procedure perform;
begin
  //normalized formats (for use in unittests)
  DecimalSeparator:='.';
  ThousandSeparator:=#0;
  ShortDateFormat:='YYYY-MM-DD';
  LongDateFormat:='YYYY-MM-DD';

  mycmdline.onOptionRead:=TOptionReadEvent(procedureToMethod(TProcedure(@variableRead)));
  mycmdline.allowOverrides:=true;

  mycmdLine.declareString('data', 'Data to process (--data= prefix can be omitted)');

  mycmdLine.beginDeclarationCategory('Extraction options:');

  mycmdLine.declareString('extract', joined(['Expression to extract from the data.','If it starts with < it is interpreted as template, otherwise as XPath 2 expression']));
  mycmdline.addAbbreviation('e');
  mycmdLine.declareString('extract-exclude', 'Comma separated list of variables ignored in an extract template. (black list) (default _follow)', '_follow');
  mycmdLine.declareString('extract-include', 'If not empty, comma separated list of variables to use in an extract template (white list)');
  mycmdLine.declareFile('extract-file', 'File containing an extract expression (for longer expressions)');
  mycmdLine.declareString('extract-kind', 'How the extract expression is evaluated. Can be auto (automatically choose between xpath/template), xpath, css, template or multipage', 'auto');
  mycmdLine.declareFile('template-file', 'Abbreviation for --extract-kind=multipage --extract-file=...');

  mycmdLine.beginDeclarationCategory('Follow options:');

  mycmdLine.declareString('follow', joined(['Expression extracting links from the page which will be followed.', 'If the expression extracts a sequence, all elements are followed.', 'If the value is an "a" node, its @href attribute is followed, if it is a "i/frame" node its @src attribute is followed, otherwise its text().', 'If it is an object, its url properties and its other properties can override command line arguments','Otherwise, the string value is treated as url.']));
  mycmdline.addAbbreviation('f');
  mycmdLine.declareString('follow-exclude', 'Comma separated list of variables ignored in an follow template. (black list)');
  mycmdLine.declareString('follow-include', 'Comma separated list of variables used in an foloow template. (white list)');
  mycmdLine.declareFile('follow-file', 'File containing an follow expression (for longer expressions)');
  mycmdLine.declareInt('follow-level', 'Maximal recursion deep', 99999);

  mycmdLine.beginDeclarationCategory('Extraction options:');


  if allowInternetAccess then begin
    mycmdLine.beginDeclarationCategory('HTTP connection options:');

    mycmdLine.declareFloat('wait', 'Wait a certain count of seconds between requests');
    mycmdLine.declareString('user-agent', 'Useragent used in http request', defaultUserAgent);
    mycmdLine.declareString('proxy', 'Proxy used for http/s requests');
    mycmdLine.declareString('post', 'Post request to send (url encoded)');
  end;

  mycmdLine.beginDeclarationCategory('Output options:');

  mycmdLine.declareFlag('quiet','Do not print status information to stderr', 'q');
  mycmdLine.declareString('default-variable-name', 'Variable name for values read in the template without explicitely given variable name', 'result');
  mycmdLine.declareString('print-variables', joined(['Which of the separate variable lists are printed', 'Comma separated list of:', '  log: Prints every variable value', '  final: Prints only the final value of a variable, if there are multiple assignments to it', '  condensed-log: Like log, but removes assignments to object properties(default)']), 'condensed-log');
  mycmdLine.declareString('print-variables-time', joined(['When the template variables are printed. ', 'Comma separated list of:', '  immediate: Prints the variable values after processing each file (default)', '  final: Print the variable values after processing all pages']), 'immediate');
  mycmdLine.declareFlag('print-type-annotations','Prints all variable values with type annotations (e.g. string: abc, instead of abc)');
  mycmdLine.declareFlag('hide-variable-names','Do not print the name of variables defined in an extract template');
  mycmdLine.declareString('printed-node-format', 'Format of an extracted node: text or xml');
  mycmdLine.declareString('output-format', 'Output format: adhoc (simple human readable), json or xml', 'adhoc');
  mycmdLine.declareString('output-encoding', 'Character encoding of the output. utf8 (default), latin1, or input (no encoding conversion)', 'utf8');

  SetLength(requests,1);

  mycmdLine.parse();

  requests[high(requests)].initFromCommandLine(mycmdLine, length(requests) - 1);

  baserequests := requests;
  setlength(baserequests, length(baserequests));

  if mycmdLine.readString('output-format') = 'adhoc' then outputFormat:=ofAdhoc
  else if mycmdLine.readString('output-format') = 'json' then outputFormat:=ofJson
  else if mycmdLine.readString('output-format') = 'xml' then outputFormat:=ofXML
  else raise EInvalidArgument.Create('Unknown output format: ' + mycmdLine.readString('output-format'));

  if cgimode then begin
    case outputFormat of
      ofAdhoc: writeln('Content-Type: text/plain'); //writeln(...) instead of writeln(stdout, ...) doesn't work, because on the sf server text written by writeln(stdout, ..) appears before text written by writeln(..) independent of actual order of the calls
      ofXML: writeln('Content-Type: text/html');
      ofJson: writeln('Content-Type: application/json');
    end;
    writeln();
  end;

  if outputFormat = ofJson then writeln('[')
  else if outputFormat = ofXML then writeln('<?xml version="1.0" encoding="UTF-8"?>'+LineEnding+'<seq>');


  htmlparser:=THtmlTemplateParserBreaker.create;
  htmlparser.TemplateParser.parsingModel:=pmHTML;
  htmlparser.KeepPreviousVariables:=kpvKeepValues;
  if allowInternetAccess then begin
    multipage := TTemplateReaderBreaker.create();
    multipage.parser:=htmlparser;
  end;
  xpathparser := htmlparser.createPseudoXPathParser('');
  alreadyProcessed := TStringList.Create;


  try
    while length(requests) > 0 do begin
      while (length(requests[0].urls) > 0)
            and  ((alreadyProcessed.indexOf(requests[0].urls[0]+#1+requests[0].post) >= 0) or
                 (requests[0].urlsLevel[0] > requests[0].followMaxLevel)) do
        requests[0].deleteUrl0;

      if length(requests[0].urls) = 0 then begin
        for i:=1 to high(requests) do
          requests[i-1] := requests[i];
        setlength(requests,length(requests)-1);
        continue;
      end;

      with requests[0] do begin
        urls[0] := htmlparser.replaceVars(trim(urls[0]));
        if urls[0] = '' then begin deleteUrl0; continue; end;

        data := urls[0];
        if not cgimode and allowInternetAccess then begin
          if assigned(onPrepareInternet) then onPrepareInternet(userAgent, proxy);
          printStatus('**** Retrieving:'+urls[0]+' ****');
          if assigned(onRetrieve) then data := onRetrieve(urls[0], post);
        end;

        alreadyProcessed.Add(urls[0]+#1+post);

        printStatus('**** Processing:'+urls[0]+' ****');
        for j := 0 to high(extractions) do begin
          htmlparser.OutputEncoding := outputEncoding;

          case extractions[j].extractKind of
            ekTemplate: begin
              htmlparser.UnnamedVariableName:=extractions[j].defaultName;
              htmlparser.parseTemplate(extractions[j].extract); //todo reuse existing parser
              htmlparser.parseHTML(data, urls[0]);
              extractions[j].pageProcessed(nil,htmlparser);
            end;
            ekXPath, ekCSS: begin
              if firstExtraction then begin
                firstExtraction := false;
                if outputFormat = ofXML then writeln('<e>');
              end else writeln(outputArraySeparator[outputFormat]);

              htmlparser.parseHTMLSimple(data, urls[0]);
              xpathparser.RootElement := htmlparser.HTMLTree;
              xpathparser.ParentElement := xpathparser.RootElement;
              xpathparser.StaticBaseUri := urls[0];
              if extractions[j].extractKind = ekCSS then xpathparser.parse('css("'+StringReplace(extractions[j].extract,'"','""',[rfReplaceAll])+'")')
              else xpathparser.parse(extractions[j].extract);
              extractions[j].printExtractedValue(xpathparser.evaluate());
              writeln;
            end;
            ekMultipage: if assigned (onPrepareInternet) then begin
              multipage.onPageProcessed:=@extractions[j].pageProcessed;
              multipage.internet := onPrepareInternet(userAgent, proxy);;
              multipagetemp := TMultiPageTemplate.create();
              multipagetemp.loadTemplateFromString(extractions[j].extract);
              multipage.setTemplate(multipagetemp);
              multipage.perform();
              multipage.setTemplate(nil);
            end
            else raise Exception.Create('Impossible');
          end;
        end;

        if follow <> '' then begin
          htmlparser.OutputEncoding := outputEncoding; //todo correct encoding?

          if follow[1] = '<' then begin //assume my template
            htmlparser.parseTemplate(follow); //todo reuse existing parser
            htmlparser.parseHTML(data, urls[0]);
            for i:=0 to htmlparser.variableChangeLog.count-1 do
              if ((length(followInclude) = 0) and (arrayIndexOf(followExclude, htmlparser.variableChangeLog.getVariableName(i)) = -1)) or
                 ((length(followInclude) > 0) and (arrayIndexOf(followInclude, htmlparser.variableChangeLog.getVariableName(i)) > -1)) then
                followTo(htmlparser.variableChangeLog.getVariableValue(i));
          end else begin
            //assume xpath
            htmlparser.parseHTMLSimple(data, urls[0]);
            xpathparser.RootElement := htmlparser.HTMLTree;
            xpathparser.ParentElement := xpathparser.RootElement;
            xpathparser.StaticBaseUri := urls[0];
            xpathparser.parse(follow);
            followTo(xpathparser.evaluate());
          end;
        end;

        deleteUrl0;
        if wait > 0.001 then Sleep(trunc(wait * 1000));
      end;
    end;
  except
    on e: EHTMLParseException do begin
      displayError(e);
      if cgimode then halt
      else raise;
    end;
    on e: EHTMLParseMatchingException do begin
      displayError(e);
      if cgimode then halt
      else raise;
    end;
  end;
  xpathparser.free;
  if allowInternetAccess then multipage.Free
  else htmlparser.free;
  mycmdLine.free;
  alreadyProcessed.Free;

  if outputFormat = ofJson then writeln(']')
  else if outputFormat = ofXML then begin
    if not firstExtraction then Writeln('</e>');
    writeln('</seq>');
  end;

end;


end.

