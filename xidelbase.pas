{Copyright (C) 2012  Benito van der Zander

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

unit xidelbase;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  extendedhtmlparser,  xquery, sysutils, bbutils, simplehtmltreeparser, multipagetemplate,
  internetaccess,
  rcmdline
  ;

var cgimode: boolean = false;
    allowInternetAccess: boolean = true;
    allowFileAccess: boolean = true;
    mycmdline: TCommandLineReader;
    defaultUserAgent: string = 'Mozilla/3.0 (compatible; Xidel)';

    majorVersion: integer = 0;
    minorVersion: integer = 5;

    onPrepareInternet: function (const useragent, proxy: string): tinternetaccess;
    onRetrieve: function (const url, postdata: string): string;
    onPreOutput: procedure ();

procedure perform;

implementation

//{$R xidelbase.res}

type TOutputFormat = (ofAdhoc, ofJson, ofXML);
var outputFormat: TOutputFormat;
    firstExtraction: boolean = true;
    outputArraySeparator: array[toutputformat] of string = ('', ', ', '</e><e>');
    internet: TInternetAccess;
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
 extractKind: (ekAuto, ekXPath, ekTemplate, ekCSS, ekXQuery, ekMultipage);

 templateActions: TStringArray;

 defaultName: string;
 printVariables: set of (pvLog, pvCondensedLog, pvFinal);
 printTypeAnnotations,  hideVariableNames, printNodeXML: boolean;

 quiet: boolean;

 procedure setExtractKind(v: string);

 procedure initFromCommandLine(cmdLine: TCommandLineReader);
 procedure mergeWithObject(obj: TXQValueObject);

 procedure setVariables(v: string);

 procedure printStatus(s: string);

 procedure printExtractedValue(value: IXQValue);
 procedure printExtractedVariables(vars: TXQVariableChangeLog; state: string);
 procedure printExtractedVariables(parser: THtmlTemplateParser);

 procedure pageProcessed(unused: TMultipageTemplateReader; parser: THtmlTemplateParser);
end;

 TProcessingRequest = record
  urls: TStringArray;
  urlsLevel: bbutils.TLongintArray;

  extractions: array of TExtraction;
  downloads: TStringArray;

  follow: string;
  followExclude, followInclude: TStringArray;
  stepLevel, followMaxLevel: integer;

  wait: Extended;
  userAgent: string;
  proxy: string;
  post: string;
  printReceivedHeaders: boolean;

  quiet: boolean;

  outputEncoding: TEncoding;
  printVariablesTime: set of (pvtImmediate, pvtFinal);


  compatibilityNoExtendedStrings,compatibilityNoObjects, compatibilityStrictTypeChecking, compatibilityStrictNamespaces: boolean;

  procedure printStatus(s: string);

  procedure initFromCommandLine(cmdLine: TCommandLineReader; level: integer);
  procedure mergeWithObject(obj: TXQValueObject);

  procedure setVariablesTime(v: string);

  procedure deleteUrl0;
  procedure addBasicValueUrl(dest: IXQValue; baseurl: string);

end;

type EInvalidArgument = Exception;

{ TExtraction }

procedure TExtraction.setExtractKind(v: string);
begin
  if extract = '' then exit;
  if striEqual(v, 'auto') then extractKind := ekAuto
  else if striEqual(v, 'xpath') then extractKind:=ekXPath
  else if striEqual(v, 'xquery') then extractKind:=ekXQuery
  else if striEqual(v, 'css') then extractKind:=ekCSS
  else if striEqual(v, 'template') then extractKind:=ekTemplate
  else if striEqual(v, 'multipage') then extractKind:=ekMultipage
  else raise Exception.Create('Unknown kind for the extract expression: '+v);
end;


procedure TExtraction.initFromCommandLine(cmdLine: TCommandLineReader);
var s: string;
begin
  extract := cmdLine.readString('extract');
  extract := trim(extract);
  extractExclude := strSplit(cmdLine.readString('extract-exclude'), ',', false);
  extractInclude := strSplit(cmdLine.readString('extract-include'), ',', false);

  setExtractKind(cmdLine.readString('extract-kind'));
  templateActions := strSplit(cmdLine.readString('template-action'), ',', false);


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

function strLoadFromFileChecked(const fn: string): string;
begin
  result := strLoadFromFile(fn);
  if Result = '' then raise Exception.Create('File '+fn+' is empty.');
end;

procedure TExtraction.mergeWithObject(obj: TXQValueObject);
var
  temp: TXQValue;
begin
  if obj.hasProperty('extract-file', @temp) then extract := temp.toString
  else if obj.hasProperty('extract', @temp) then extract := temp.toString;
  if obj.hasProperty('extract-exclude', @temp) then extractExclude := strSplit(temp.toString, ',', false);
  if obj.hasProperty('extract-include', @temp) then extractInclude := strSplit(temp.toString, ',', false);
  if obj.hasProperty('extract-kind', @temp) then setExtractKind(temp.toString);
  if obj.hasProperty('template-file', @temp)  then begin
    extract := strLoadFromFileChecked(temp.toString);
    extractKind := ekMultipage;
  end;
  if obj.hasProperty('template-action', @temp) then templateActions := strSplit(temp.toString, ',', false);

  if obj.hasProperty('default-variable-name', @temp) then defaultName := temp.toString;
  if obj.hasProperty('print-type-annotations', @temp) then printTypeAnnotations:=temp.toBoolean;
  if obj.hasProperty('hide-variable-names', @temp) then hideVariableNames := temp.toBoolean;

  if obj.hasProperty('print-variables', @temp) then setVariables(temp.toString);

  if obj.hasProperty('quiet', @temp) then quiet := temp.toBoolean;
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

function strReadFromStdin: string;
var s:string;
begin
  result:='';
  while not EOF(Input) do begin
    ReadLn(s);
    result+=s+LineEnding;
  end;
end;

procedure TProcessingRequest.initFromCommandLine(cmdLine: TCommandLineReader; level: integer);
  procedure flagToBoolean(var b: boolean; n: string);
  begin
    b := cmdLine.readFlag(n);
  end;

var
  tempSplitted: TStringArray;
  i: Integer;
  s: string;
begin
  if length(extractions) > 0 then
    extractions[high(extractions)].initFromCommandLine(cmdLine);

  for i:=0 to high(extractions) do with extractions[i] do begin
    if extract = '-' then extract:=strReadFromStdin;
    if extractKind = ekAuto then
      if (extract <> '') and (extract[1] = '<') then extractKind:=ekTemplate
      else if strBeginsWith(extract, 'xquery') then extractKind:=ekXQuery
      else extractKind:=ekXPath;
  end;

  if cmdLine.readString('follow-file') <> '' then follow := strLoadFromFileChecked(cmdLine.readString('follow-file'))
  else begin
    follow := cmdLine.readString('follow');
    if follow = '-' then follow :=strReadFromStdin;
  end;
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
    printReceivedHeaders := cmdLine.readFlag('print-received-headers');
  end;

  setVariablesTime(cmdLine.readString('print-variables-time'));
  outputEncoding:=strEncodingFromName(cmdLine.readString('output-encoding'));


  flagToBoolean(compatibilityNoExtendedStrings, 'no-extended-strings');
  flagToBoolean(compatibilityNoObjects, 'no-objects');
  flagToBoolean(compatibilityStrictTypeChecking, 'strict-type-checking');
  flagToBoolean(compatibilityStrictNamespaces, 'strict-namespaces');


  urls:=cmdLine.readNamelessFiles();

  if (length(extractions) > 0) and (extractions[high(extractions)].extractKind = ekMultipage) and (length(urls) = 0) then
    arrayAdd(urls, '<empty/>');

  if cmdLine.readString('data') <> '' then arrayAdd(urls, cmdLine.readString('data'));

  setlength(urlsLevel, length(urls));
  for i:= 0 to high(urlsLevel) do urlsLevel[i] := stepLevel;
end;

procedure TProcessingRequest.mergeWithObject(obj: TXQValueObject);
  procedure flagToBoolean(var b: boolean; n: string);
  var
    temp: TXQValue;
  begin
    if obj.hasProperty(n, @temp) then b := temp.toBoolean;
  end;
var
  temp: TXQValue;
  tempSplitted: TStringArray;
begin
  if length(extractions) > 0 then
    extractions[high(extractions)].mergeWithObject(obj);

  if obj.hasProperty('download', @temp) then arrayAdd(downloads, temp.toString);

  if obj.hasProperty('follow-file', @temp) then follow := strLoadFromFileChecked(temp.toString)
  else if obj.hasProperty('follow', @temp) then follow := temp.toString;
  if obj.hasProperty('follow-exclude', @temp) then followExclude := strSplit(temp.toString, ',', false);
  if obj.hasProperty('follow-include', @temp) then followInclude := strSplit(temp.toString, ',', false);
  if obj.hasProperty('follow-level', @temp) then followMaxLevel := temp.toInt64;

  if obj.hasProperty('wait', @temp) then wait := temp.toDecimal;
  if obj.hasProperty('user-agent', @temp) then userAgent := temp.toString;
  if obj.hasProperty('proxy', @temp) then proxy := temp.toString;
  if obj.hasProperty('post', @temp) then post := temp.toString;
  if obj.hasProperty('print-received-headers', @temp) then printReceivedHeaders := temp.toBoolean;

  if obj.hasProperty('print-variables-time', @temp) then setVariablesTime(temp.toString);
  if obj.hasProperty('output-encoding', @temp) then outputEncoding:=strEncodingFromName(temp.toString);

  flagToBoolean(compatibilityNoExtendedStrings, 'no-extended-strings');
  flagToBoolean(compatibilityNoObjects, 'no-objects');
  flagToBoolean(compatibilityStrictTypeChecking, 'strict-type-checking');
  flagToBoolean(compatibilityStrictNamespaces, 'strict-namespaces');

  setlength(urls, 0);
  if obj.hasProperty('follow', @temp) then
    addBasicValueUrl(temp, '');
  if obj.hasProperty('url', @temp) then
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

procedure TProcessingRequest.addBasicValueUrl(dest: IXQValue; baseurl: string);
  procedure activateNewUrl;
  begin
    if length(urlsLevel) > 0 then arrayAdd(urlsLevel, urlsLevel[0])
    else arrayAdd(urlsLevel, stepLevel);
    if (guessType(baseurl) in [rtFile, rtRemoteURL]) and (guessType(urls[high(urls)]) = rtFile) then
      urls[high(urls)] := strResolveURI(urls[high(urls)], baseurl);
  end;

var
  n: TTreeNode;
  i: Integer;
  x: IXQValue;
begin
  case dest.kind of
    pvkUndefined: exit;
    pvkNode: begin
      n := dest.toNode;
      if n = nil then exit;
      if n.typ <> tetOpen then arrayAdd(urls, dest.toString)
      else if SameText(n.value, 'a') then arrayAdd(urls, n.getAttribute('href', ''))
      else if SameText(n.value, 'frame') or SameText(n.value, 'iframe') or SameText(n.value, 'img') then arrayAdd(urls, n.getAttribute('src', ''))
      else arrayAdd(urls, n.deepNodeText());
      activateNewUrl;
    end;
    pvkSequence:
      for x in dest do
        addBasicValueUrl(x, baseurl);
    else begin
      arrayAdd(urls, dest.toString);
      activateNewUrl;
    end;
  end;
end;

procedure TExtraction.printStatus(s: string);
begin
  if not quiet then writeln(stderr, s);
end;

procedure TExtraction.printExtractedValue(value: IXQValue);
var
  i: Integer;
  temp: TXQValueObject;
  x: IXQValue;
begin
  case outputFormat of
    ofAdhoc: begin
      if printTypeAnnotations then write(value.typeName+': ');
      if value is TXQValueSequence then begin
        i := 0;
        for x in value do begin
          if i <> 0 then writeln;
          printExtractedValue(x);
          i += 1;
        end;
      end else if printNodeXML and (value is TXQValueNode) then
        write(value.toNode.outerXML())
      else if value is TXQValueObject then begin
        x := value.clone;
        temp := x as TXQValueObject;
        write('{');
        if temp.values.count > 0 then begin
          write(temp.values.getName(0),': '); printExtractedValue(temp.values.get(0));
          for i:=1 to temp.values.count-1 do begin
            write(', ', temp.values.getName(i), ': ');
            printExtractedValue(temp.values.get(i));
          end;
        end;
        write('}');
      end
      else write(value.toString);
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

procedure followTo(dest: IXQValue); forward;

procedure TExtraction.pageProcessed(unused: TMultipageTemplateReader; parser: THtmlTemplateParser);
var
  i: Integer;
begin
  if firstExtraction then begin
    firstExtraction := false;
    if outputFormat = ofXML then writeln('<e>');
  end else writeln(outputArraySeparator[outputFormat]);

  printExtractedVariables(parser);

  for i := 0 to parser.variableChangeLog.count-1 do
    if parser.variableChangeLog.getName(i) = '_follow' then
      followTo(parser.variableChangeLog.get(i));
end;


procedure TExtraction.printExtractedVariables(vars: TXQVariableChangeLog; state: string);
  function acceptName(n: string): boolean;
  begin
    result := ((length(extractInclude) = 0) and (arrayIndexOf(extractExclude, n) = -1)) or
              ((length(extractInclude) > 0) and (arrayIndexOf(extractInclude, n) > -1));
  end;

var
  i: Integer;
  tempUsed: array of boolean;
  first: boolean;
  values: IXQValue;
  j: Integer;
begin
  printStatus(state);
  case outputFormat of
    ofAdhoc: begin
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if not hideVariableNames then write(vars.Names[i] + ': ');
           printExtractedValue(vars.get(i));
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
            printExtractedValue(vars.get(i));
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
            values := vars.getAll(vars.Names[i]);
            if values.getSequenceCount = 1 then printExtractedValue(values)
            else begin
              write('[');
              printExtractedValue(values.getChild(1));
              for j:=2 to values.getSequenceCount do begin
                write(', ');
                printExtractedValue(values.getChild(j));
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
            printExtractedValue(vars.get(i));
          end;
        end;
        if not first then write('</e>');
        writeln('</seq>');
      end else begin
        writeln('<object>');
        for i:=0 to vars.count-1 do
           if acceptName(vars.Names[i])  then begin
             write('<'+vars.Names[i] + '>');
             printExtractedValue(vars.Values[i]);
             writeln('</'+vars.Names[i] + '>');
           end;
        writeln('</object>');
      end;
    end;
  end;
end;

var
  baserequests, requests: array of TProcessingRequest;

procedure followTo(dest: IXQValue);
var
  n: TTreeNode;
  refRequest: integer;
  i: Integer;
  x: IXQValue;
begin
  if (length(requests) > 1) and (requests[0].stepLevel = requests[1].stepLevel - 1) then
    refRequest := 1
   else
    refRequest := 0;

  case dest.kind of
    pvkObject: begin //this can't imho be in addBasicValueUrl, because you can't reliable call a method on a  record in an array that will be modified
      SetLength(requests, length(requests) + 1);
      requests[high(requests)] := requests[refRequest];
      requests[high(requests)].mergeWithObject(dest as TXQValueObject);
    end;
    pvkSequence:
      for x in dest do
        followTo(x);
    else requests[refRequest].addBasicValueUrl(dest, requests[0].urls[0])
  end;
end;


function makeAbsoluteFilePath(s: string): string;
begin
  result := s;
  if strContains('://', s) then exit;
  if s = '' then exit;
  if s[1] in AllowDirectorySeparators then exit;
  if (length(s) >= 3) and (s[2] = ':') and (s[3] in AllowDirectorySeparators) then exit;
  result := ExpandFileName(s)
end;

type

{ THtmlTemplateParserBreaker }

 THtmlTemplateParserBreaker = class(THtmlTemplateParser)
  procedure parseHTMLSimple(html,uri,contenttype: string);
end;

 TTemplateReaderBreaker = class(TMultipageTemplateReader)
   constructor create();
   procedure setTemplate(atemplate: TMultiPageTemplate);
   procedure perform(actions: TStringArray);
   procedure selfLog(sender: TMultipageTemplateReader; logged: string; debugLevel: integer);
 end;

var htmlparser:THtmlTemplateParserBreaker;
    i: Integer;
    temp: TStringArray;
    j: Integer;
    data, downloadTo: string;

    alreadyProcessed: TStringList;
    xpathparser: TXQueryEngine;
    multipage: TTemplateReaderBreaker;
    multipagetemp: TMultiPageTemplate;

{ TMultiPageTemplateBreaker }


{ THtmlTemplateParserBreaker }

procedure THtmlTemplateParserBreaker.parseHTMLSimple(html, uri, contenttype: string);
begin
  inherited parseHTMLSimple(html, uri, contenttype);
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

procedure TTemplateReaderBreaker.perform(actions: TStringArray);
begin
  if length(template.baseActions.children) = 0 then raise Exception.Create('Template contains no actions!'+LineEnding+'A Multipage template should look like <action>  <page url="..."> <post> post data </post> <template> single page template </template> </page> </action> ');
  if length(actions) = 0 then callAction(template.baseActions.children[0])
  else for i:= 0 to high(actions) do
    callAction(actions[i]);
end;

procedure TTemplateReaderBreaker.selfLog(sender: TMultipageTemplateReader; logged: string; debugLevel: integer);
begin
  if debugLevel <> 0 then exit;
  writeln(stderr, logged);
end;

procedure displayError(e: Exception);
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
  procedure overrideVar(const name, value: string);
  procedure clearNameless;
end;

{ TCommandLineReaderBreaker }

procedure TCommandLineReaderBreaker.overrideVar(const name, value: string);
begin
  findProperty(name)^.strvalue:=value;
end;

procedure TCommandLineReaderBreaker.clearNameless;
begin
  SetLength(nameless, 0);
end;

procedure variableRead(pseudoself: TObject; sender: TObject; const name, value: string);
begin
  if (name = 'follow') or (name = 'follow-file') or ((name = '') and (length(requests[high(requests)].extractions) > 0))  then begin
    if name = 'follow-file' then
      if value = '-' then TCommandLineReaderBreaker(sender).overrideVar('follow', '-')
      else TCommandLineReaderBreaker(sender).overrideVar('follow', strLoadFromFileChecked(value));

    //writeln(stderr,name,'=',value);
    requests[high(requests)].initFromCommandLine(TCommandLineReader(sender), length(requests) - 1);
    TCommandLineReaderBreaker(sender).clearNameless;
    SetLength(requests, length(requests) + 1);

    TCommandLineReaderBreaker(sender).overrideVar('follow', '');
    TCommandLineReaderBreaker(sender).overrideVar('extract', '');
    TCommandLineReaderBreaker(sender).overrideVar('download', '');
  end else if (name = 'extract') or (name = 'extract-file') or (name = 'template-file') then begin
    if name = 'extract-file' then begin
      if value = '-' then TCommandLineReaderBreaker(sender).overrideVar('extract', '-')
      else TCommandLineReaderBreaker(sender).overrideVar('extract', strLoadFromFileChecked(value));
    end else if name = 'template-file' then begin
      TCommandLineReaderBreaker(sender).overrideVar('extract-kind', 'multipage');
      TCommandLineReaderBreaker(sender).overrideVar('extract', strLoadFromFileChecked(value));
    end;

    SetLength(requests[high(requests)].extractions, length(requests[high(requests)].extractions) + 1);
    requests[high(requests)].extractions[high(requests[high(requests)].extractions)].initFromCommandLine(TCommandLineReader(sender));

    //if name = 'template-file' then TCommandLineReaderBreaker(sender).overrideVar('extract-kind', 'auto');
  end else if name = 'download' then
    arrayAdd(requests[high(requests)].downloads, value);
end;

procedure printVersion;
begin
  writeln('Xidel '+IntToStr(majorVersion)+'.'+IntToStr(minorVersion));
  writeln('');
  writeln('http://videlibri.sourceforge.net/xidel.html');
  writeln('by Benito van der Zander <benito@benibela.de>');
  writeln();
end;

procedure printUsage;
{var
  S: TResourceStream;
  F: TFileStream;
  temp: string;
  split: TStringArray;
  i: integer;
begin
  // create a resource stream which points to our resource
  S := TResourceStream.Create(HInstance, 'README', 'RCDATA');
  try
    setlength(temp, s.Size);
    split := strSplit(temp, #10);
    for i := 0 to high(split) do
      writeln(split[i]);
  finally
    S.Free; // destroy the resource stream
  end;         } //doesn't work?? with 'README         RCDATA "readme"' in xidelbase.rc
begin
  {$I printUsage.inc}
end;

procedure perform;
var
  tempvalue: IXQValue;
  realUrl: string;
  tempHost: string;
  realPath: String;
  realFile: String;
  tempProto: string;
  toCreate: String;
  contenttype: String;
begin
  //normalized formats (for use in unittests)
  DecimalSeparator:='.';
  ThousandSeparator:=#0;
  ShortDateFormat:='YYYY-MM-DD';
  LongDateFormat:='YYYY-MM-DD';

  mycmdline.onOptionRead:=TOptionReadEvent(procedureToMethod(TProcedure(@variableRead)));
  mycmdline.allowOverrides:=true;

  mycmdLine.declareString('data', 'Data/URL to process (--data= prefix can be omitted)');
  mycmdLine.declareString('download', 'Downloads/saves the data to a given filename (- prints to stdout, . uses the filename of the url)');

  mycmdLine.beginDeclarationCategory('Extraction options:');

  mycmdLine.declareString('extract', joined(['Expression to extract from the data.','If it starts with < it is interpreted as template, otherwise as XPath 2 expression']));
  mycmdline.addAbbreviation('e');
  mycmdLine.declareString('extract-exclude', 'Comma separated list of variables ignored in an extract template. (black list) (default _follow,_url)', '_follow');
  mycmdLine.declareString('extract-include', 'If not empty, comma separated list of variables to use in an extract template (white list)');
  mycmdLine.declareFile('extract-file', 'File containing an extract expression (for longer expressions)');
  mycmdLine.declareString('extract-kind', 'How the extract expression is evaluated. Can be auto (automatically choose between xpath/template), xpath, xquery, css, template or multipage', 'auto');
  mycmdLine.declareFile('template-file', 'Abbreviation for --extract-kind=multipage --extract-file=...');
  mycmdLine.declareString('template-action', 'Select which action from the multipage template should be run (multiple actions are allowed with comma separated values)');

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
    mycmdLine.declareFlag('print-received-headers', 'Print the received headers');
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

  mycmdLine.beginDeclarationCategory('XPath/XQuery compatibility options:');

  mycmdline.declareFlag('no-extended-strings', 'Disables the replacements of variables in double quoted strings, like "$varname;"');
  mycmdline.declareFlag('no-objects', 'Disables the object.property syntax like in (object(("a", x)).a)');
  mycmdline.declareFlag('strict-type-checking', 'Disables weakly typing ("1" + 2 will raise an error, otherwise it evaluates to 3)');
  mycmdline.declareFlag('strict-namespaces', 'Disables the usage of undeclared namespace. Otherwise foo:bar always matches an element with prefix foo.');

  mycmdLine.declareFlag('version','Print version number ('+IntToStr(majorVersion)+'.'+IntToStr(minorVersion)+')');
  mycmdLine.declareFlag('usage','Print help, examples and usage information');

  SetLength(requests,1);

  mycmdLine.parse();

  if mycmdline.readFlag('version') then
    printVersion;
  if mycmdline.readFlag('usage') then begin
    printUsage;
    exit;
  end;

  requests[high(requests)].initFromCommandLine(mycmdLine, length(requests) - 1);

  if (length(requests) >= 2) and (length(requests[high(requests)].extractions) = 0) and (length(requests[high(requests)].downloads) = 0) then begin
    requests[high(requests)].extractions := requests[high(requests)-1].extractions;
    requests[high(requests)].downloads := requests[high(requests)-1].downloads;
    requests[high(requests)].follow := requests[high(requests)-1].follow;
  end;

  baserequests := requests;
  setlength(baserequests, length(baserequests));

  if mycmdLine.readString('output-format') = 'adhoc' then outputFormat:=ofAdhoc
  else if mycmdLine.readString('output-format') = 'json' then outputFormat:=ofJson
  else if mycmdLine.readString('output-format') = 'xml' then outputFormat:=ofXML
  else raise EInvalidArgument.Create('Unknown output format: ' + mycmdLine.readString('output-format'));

  if assigned(onPreOutput) then onPreOutput();


  if outputFormat = ofJson then writeln('[')
  else if outputFormat = ofXML then writeln('<?xml version="1.0" encoding="UTF-8"?>'+LineEnding+'<seq>');


  htmlparser:=THtmlTemplateParserBreaker.create;
  htmlparser.TemplateParser.parsingModel:=pmHTML;
  htmlparser.KeepPreviousVariables:=kpvKeepValues;
  if allowInternetAccess then begin
    multipage := TTemplateReaderBreaker.create();
    multipage.parser:=htmlparser;
  end;
  xpathparser := htmlparser.QueryEngine;

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
        xpathparser.AllowVariableUseInStringLiterals:= not compatibilityNoExtendedStrings;
        xpathparser.VariableChangelog.allowObjects:=not compatibilityNoObjects;
        htmlparser.variableChangeLog.allowObjects:=xpathparser.VariableChangelog.allowObjects;
        xpathparser.StaticContext.strictTypeChecking:=compatibilityStrictTypeChecking;
        xpathparser.StaticContext.useLocalNamespaces:=not compatibilityStrictNamespaces;


        urls[0] := htmlparser.replaceVars(trim(urls[0]));
        if urls[0] = '' then begin deleteUrl0; continue; end;

        data := urls[0];
        if not cgimode then begin
          if data = '-' then data := strReadFromStdin()
          else if allowInternetAccess then begin
            if assigned(onPrepareInternet) then  internet := onPrepareInternet(userAgent, proxy);
            printStatus('**** Retrieving:'+urls[0]+' ****');
            if post <> '' then printStatus('Post: '+post);
            if assigned(onRetrieve) then data := onRetrieve(urls[0], post);
            if printReceivedHeaders and assigned(internet) then begin
              printStatus('** Headers: (status: '+inttostr(internet.lastHTTPResultCode)+')**');
              for j:=0 to internet.lastHTTPHeaders.Count-1 do
                writeln(internet.lastHTTPHeaders[j]);
            end;
            if Assigned(internet) then contenttype := internet.getLastHTTPHeader('Content-Type');
          end
        end;

        alreadyProcessed.Add(urls[0]+#1+post);
        htmlparser.variableChangeLog.add('url', urls[0]);
        decodeURL(urls[0], tempProto, tempHost, realUrl);
        htmlparser.variableChangeLog.add('host', tempHost);
        htmlparser.variableChangeLog.add('path', realUrl);


        printStatus('**** Processing:'+urls[0]+' ****');
        for j := 0 to high(extractions) do begin
          htmlparser.OutputEncoding := outputEncoding;

          case extractions[j].extractKind of
            ekTemplate: begin
              htmlparser.UnnamedVariableName:=extractions[j].defaultName;
              htmlparser.parseTemplate(extractions[j].extract); //todo reuse existing parser
              htmlparser.parseHTML(data, makeAbsoluteFilePath(urls[0]), contenttype);
              extractions[j].pageProcessed(nil,htmlparser);
            end;
            ekXPath, ekCSS, ekXQuery: begin
              if firstExtraction then begin
                firstExtraction := false;
                if outputFormat = ofXML then writeln('<e>');
              end else writeln(outputArraySeparator[outputFormat]);

              htmlparser.parseHTMLSimple(data, urls[0], contenttype);
              xpathparser.RootElement := htmlparser.HTMLTree;
              xpathparser.ParentElement := xpathparser.RootElement;
              xpathparser.StaticContext.BaseUri := makeAbsoluteFilePath(urls[0]);
              case extractions[j].extractKind of
                ekCSS: xpathparser.parseCSS3(extractions[j].extract);
                ekXPath: xpathparser.parseXPath2(extractions[j].extract);
                ekXQuery: xpathparser.parseXQuery1(extractions[j].extract);
              end;
              tempvalue :=xpathparser.evaluate();
              extractions[j].printExtractedValue(tempvalue);
              writeln;
            end;
            ekMultipage: if assigned (onPrepareInternet) then begin
              multipage.onPageProcessed:=@extractions[j].pageProcessed;
              multipage.internet := onPrepareInternet(userAgent, proxy);;
              multipagetemp := TMultiPageTemplate.create();
              multipagetemp.loadTemplateFromString(extractions[j].extract);
              multipage.setTemplate(multipagetemp);
              multipage.perform(extractions[j].templateActions);
              multipage.setTemplate(nil);
              multipagetemp.free;
            end
            else raise Exception.Create('Impossible');
          end;
        end;

        if not cgimode and allowFileAccess and (length(downloads) > 0) then begin
          realUrl := strSplitGet('?', realUrl);
          realUrl := strSplitGet('#', realUrl);
          j := strRpos('/', realUrl);
          if j = 0 then begin
            realPath := '';
            realFile := realUrl;
          end else begin
            realPath := copy(realUrl, 1, j);
            realFile := copy(realUrl, j + 1, length(realUrl) - j)
          end;
          if strBeginsWith(realPath, '/') then delete(realPath,1,1);

          for j := 0 to high(downloads) do begin
            downloadTo := htmlparser.replaceVars(downloads[j]);
            {$ifdef win32}
            downloadTo := StringReplace(downloadTo, '\' , '/', [rfReplaceAll]);
            {$endif}
            //Download abc/def/index.html
            //    foo/bar/xyz   save in directory foo/bar with name xyz
            //    foo/bar/      save in directory foo/bar/abc/def with name index.html
            //    foo/bar/.     save in directory foo/bar with name index.html
            //    foo           save in current directory/abc/def with name foo
            //    ./            save in current directory/abc/def with name index.html
            //    ./.           save in current directory with name index.html
            //    .             save in current directory with name index.html
            //    -             print to stdout
            if downloadTo = '-' then begin
              write(data);
              continue;
            end;
            if downloadTo = './.' then downloadTo:=realPath+realFile
            else if strEndsWith(downloadTo, '/.') then begin
              SetLength(downloadto,Length(downloadTo)-1);
              downloadTo:=downloadTo+realFile;
            end else if strEndsWith(downloadTo, '/') then begin
              downloadTo:=downloadTo+realPath+realFile;
            end;
            if strEndsWith(downloadTo, '/') or (downloadTo = '') then downloadTo += 'index.html';
            printStatus('**** Save as: '+downloadTo+' ****');
            if pos('/', downloadTo) > 0 then
              ForceDirectories(StringReplace(StringReplace(copy(downloadTo, 1, strRpos('/', downloadTo)-1), '//', '/', [rfReplaceAll]), '/', DirectorySeparator, [rfReplaceAll]));
            strSaveToFileUTF8(StringReplace(downloadTo, '/', DirectorySeparator, [rfReplaceAll]), data);
          end;
        end;

        if follow <> '' then begin
          htmlparser.OutputEncoding := outputEncoding; //todo correct encoding?

          if follow[1] = '<' then begin //assume my template
            htmlparser.parseTemplate(follow); //todo reuse existing parser
            htmlparser.parseHTML(data, urls[0], contenttype);
            for i:=0 to htmlparser.variableChangeLog.count-1 do
              if ((length(followInclude) = 0) and (arrayIndexOf(followExclude, htmlparser.variableChangeLog.getName(i)) = -1)) or
                 ((length(followInclude) > 0) and (arrayIndexOf(followInclude, htmlparser.variableChangeLog.getName(i)) > -1)) then
                followTo(htmlparser.variableChangeLog.get(i));
          end else begin
            //assume xpath
            htmlparser.parseHTMLSimple(data, urls[0], contenttype);
            xpathparser.RootElement := htmlparser.HTMLTree;
            xpathparser.ParentElement := xpathparser.RootElement;
            xpathparser.StaticContext. BaseUri := urls[0];
            if strBeginsWith(follow, 'xquery') then xpathparser.parseXQuery1(follow)
            else xpathparser.parseXPath2(follow);
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
      if not cgimode then raise;
    end;
    on e: EHTMLParseMatchingException do begin
      displayError(e);
      if not cgimode then raise;
    end;
    on e: EXQEvaluationException do begin
      displayError(e);
      if not cgimode then raise;
    end;
    on e: EXQParsingException do begin
      displayError(e);
      if not cgimode then raise;
    end;
  end;
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

