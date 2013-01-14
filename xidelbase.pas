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
  Classes,         {$ifdef win32} windows, {$endif}
  extendedhtmlparser,  xquery, sysutils, bbutils, simplehtmltreeparser, multipagetemplate,
  internetaccess, contnrs,
  rcmdline
  ;

var cgimode: boolean = false;
    allowInternetAccess: boolean = true;
    allowFileAccess: boolean = true;
    mycmdline: TCommandLineReader;
    defaultUserAgent: string = 'Mozilla/3.0 (compatible; Xidel)';

    majorVersion: integer = 0;
    minorVersion: integer = 6;

    onPrepareInternet: function (const useragent, proxy: string): tinternetaccess;
    onRetrieve: function (const method, url, postdata: string): string;
    onPreOutput: procedure ();

procedure perform;

implementation

//{$R xidelbase.res}

type TOutputFormat = (ofAdhoc, ofJsonWrapped, ofXMLWrapped);
var //output options
    outputFormat: TOutputFormat;
    outputEncoding: TEncoding = eUTF8;  //default to utf-8
    outputHeader: string;
    firstExtraction: boolean = true;
    outputArraySeparator: array[toutputformat] of string = ('',  ', ', '</e><e>');
    {$ifdef win32}systemEncodingIsUTF8: boolean = true;{$endif}

    internet: TInternetAccess;

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

    //data processing classes
var htmlparser:THtmlTemplateParserBreaker;
    xpathparser: TXQueryEngine;
    multipage: TTemplateReaderBreaker;
    multipagetemp: TMultiPageTemplate;

procedure w(const s: string);
{$ifdef win32}
var
  temp, temp2: String;
{$endif}
begin
  if s = '' then exit;
  if (outputEncoding = eUTF8) or (outputEncoding = eUnknown) then write(s)
  {$ifdef win32}
  else if outputEncoding = eUnknownUser1 then begin
    if systemEncodingIsUTF8 then temp := s
    else temp := Utf8ToAnsi(s);
    SetLength(temp2, length(temp)+1);
    if charToOEM(pchar(temp), pchar(temp2)) then
      write(pchar(temp2));
  end
  {$endif}
  else write(strConvertFromUtf8(s, outputEncoding));
end;

procedure wln(const s: string = '');
begin
  w(s);
  w(LineEnding);
end;

function joined(s: array of string): string;
var
  i: Integer;
begin
  if length(s) = 0 then exit('');
  result := s[0];
  for i:=1 to high(s) do result := result + LineEnding + s[i];
end;


function makeAbsoluteFilePath(s: string): string; //todo: check if it really needed where it is used
begin
  result := s;
  if strContains('://', s) then exit;
  if s = '' then exit;
  if s[1] in AllowDirectorySeparators then exit;
  if (length(s) >= 3) and (s[2] = ':') and (s[3] in AllowDirectorySeparators) then exit;
  result := ExpandFileName(s)
end;

function strLoadFromFileChecked(const fn: string): string;
begin
  result := strLoadFromFile(fn);
  if Result = '' then raise Exception.Create('File '+fn+' is empty.');
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


procedure setOutputEncoding(e: string);
var
  str: String;
begin
  //todo call this
  str:=UpperCase(e);
  outputEncoding:=eUnknown;
  case str of
    'UTF-8', 'UTF8': outputEncoding:=eUTF8;
    'CP1252', 'ISO-8859-1', 'LATIN1', 'ISO-8859-15': outputEncoding:=eWindows1252;
    'UTF-16BE', 'UTF16BE': outputEncoding:=eUTF16BE;
    'UTF-16LE', 'UTF16LE': outputEncoding:=eUTF16LE;
    'UTF-32BE', 'UTF32BE': outputEncoding:=eUTF32BE; //utf-32 seems to be broken, but no one is using anyways. TODO: Fix sometime
    'UTF-32LE', 'UTF32LE': outputEncoding:=eUTF32LE;
    'OEM': outputEncoding:=eUnknownUser1
  end;
end;


type
  TOptionReaderWrapper = class
    function read(const name: string; out value: string): boolean; virtual; abstract;
    function read(const name: string; out value: integer): boolean; virtual; abstract;
    function read(const name: string; out value: boolean): boolean; virtual; abstract;
    function read(const name: string; out value: Extended): boolean; virtual; abstract;
  end;

  { TOptionReaderFromCommandLine }

  TOptionReaderFromCommandLine = class(TOptionReaderWrapper)
    constructor create(cmdLine: TCommandLineReader);
    function read(const name: string; out value: string): boolean; override;
    function read(const name: string; out value: integer): boolean; override;
    function read(const name: string; out value: boolean): boolean; override;
    function read(const name: string; out value: Extended): boolean; override;
  private
    acmdLine: TCommandLineReader;
  end;

  { TOptionReaderFromObject }

  TOptionReaderFromObject = class(TOptionReaderWrapper)
    constructor create(aobj: TXQValueObject);
    function read(const name: string; out value: string): boolean; override;
    function read(const name: string; out value: integer): boolean; override;
    function read(const name: string; out value: boolean): boolean; override;
    function read(const name: string; out value: Extended): boolean; override;
  private
    obj: TXQValueObject;
  end;

type

{ TData }

TData = class
{private todo: optimize
  fparsed: TTreeDocument;
  function GetParsed: TTreeDocument;
public}
  rawdata: string;
  fullurl: string;
  contenttype: string;
  constructor create(somedata: string; aurl: string; acontenttype: string = '');
  //property parsed:TTreeDocument read GetParsed;
end;

TDataProcessing = class;
TProcessingContext = class;

{ TFollowTo }

TFollowTo = class
  class function createFromRetrievalAddress(data: string): TFollowTo;
  function clone: TFollowTo; virtual; abstract;
  function retrieve(parent: TProcessingContext): TData; virtual; abstract;
  procedure replaceVariables; virtual;
  function equalTo(ft: TFollowTo): boolean; virtual; abstract;
end;

{ THTTPRequest }

THTTPRequest = class(TFollowTo)
  url: string;
  method: string;
  data: string;
  constructor create(aurl: string);
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): TData; override;
  procedure replaceVariables; override;
  function equalTo(ft: TFollowTo): boolean; override;
end;

{ TFileRequest }

TFileRequest = class(TFollowTo)
  url: string;
  constructor create(aurl: string);
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): TData; override;
  procedure replaceVariables; override;
  function equalTo(ft: TFollowTo): boolean; override;
end;

{ TDirectDataRequest }

TDirectDataRequest = class(TFollowTo)
  data: string;
  constructor create(adata: string);
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): TData; override;
  function equalTo(ft: TFollowTo): boolean; override;
  //procedure replaceVariables;  do not replace vars in direct data
end;

{ TStdinDataRequest }

TStdinDataRequest = class(TFollowTo)
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): TData; override;
  function equalTo(ft: TFollowTo): boolean; override;
end;

{TFollowXQV = class(TFollowTo)
  xqv: TXQValue;
  //can be url/http-request, file(?), data
  //object with arbitrary options
  //sequence of previous
end;}


{ TFollowToList }

TFollowToList = class(TFpObjectList)
  constructor Create;
  procedure merge(l: TFollowToList);
  function first: TFollowTo;

  procedure add(ft: TFollowTo);
  procedure merge(dest: IXQValue; basedata: TData; parent: TProcessingContext);

  function containsEqual(ft: TFollowTo): boolean;
private
  procedure addBasicUrl(absurl: string; baseurl: string);
end;



{ TDataProcessing }

TDataProcessing = class
  parent: TProcessingContext;
  function process(data: TData): TFollowToList; virtual; abstract;

  procedure readOptions(reader: TOptionReaderWrapper); virtual;
  procedure initFromCommandLine(cmdLine: TCommandLineReader); virtual;
  procedure mergeWithObject(obj: TXQValueObject); virtual;

  function clone: TDataProcessing; virtual; abstract;
end;


{ TDownload }

TDownload = class(TDataProcessing)
  downloadTarget: string;
  function process(data: TData): TFollowToList; override;
  procedure readOptions(reader: TOptionReaderWrapper); override;
  function clone: TDataProcessing; override;
end;

{ TExtraction }

TExtraction = class(TDataProcessing)
 extract: string;
 extractExclude, extractInclude: TStringArray;
 extractKind: (ekAuto, ekXPath, ekTemplate, ekCSS, ekXQuery, ekMultipage);

 templateActions: TStringArray;

 defaultName: string;
 printVariables: set of (pvLog, pvCondensedLog, pvFinal);
 printTypeAnnotations,  hideVariableNames: boolean;
 printedNodeFormat: TTreeNodeSerialization;

 constructor create;

 procedure setExtractKind(v: string);

 procedure readOptions(reader: TOptionReaderWrapper); override;

 procedure setVariables(v: string);

 procedure printExtractedValue(value: IXQValue);
 procedure printExtractedVariables(vars: TXQVariableChangeLog; state: string);
 procedure printExtractedVariables(parser: THtmlTemplateParser);

 function process(data: TData): TFollowToList; override;

 procedure assignOptions(other: TExtraction);
 function clone: TDataProcessing; override;
private
 currentFollowList: TFollowToList;
 currentData: TData;
 procedure pageProcessed(unused: TMultipageTemplateReader; parser: THtmlTemplateParser);
end;


{ TFollowToWrapper }

TFollowToWrapper = class(TDataProcessing)
  followTo: TFollowTo;
  function process(data: TData): TFollowToList; override;
  function clone: TDataProcessing; override;
end;

{ TProcessingContext }

TProcessingContext = class(TDataProcessing)
  dataSources: array of TDataProcessing;
  actions: array of TDataProcessing;

  follow: string;
  followExclude, followInclude: TStringArray;
  followTo: TProcessingContext;

  nextSibling: TProcessingContext;

  wait: Extended;
  userAgent: string;
  proxy: string;
  printReceivedHeaders: boolean;

  quiet: boolean;

  compatibilityNoExtendedStrings,compatibilityNoObjects, compatibilityStrictTypeChecking, compatibilityStrictNamespaces: boolean;

  procedure printStatus(s: string);

  procedure readOptions(reader: TOptionReaderWrapper); override;
  procedure mergeWithObject(obj: TXQValueObject); override;

  procedure readNewDataSource(data: TFollowTo; options: TOptionReaderWrapper);
  procedure readNewAction(action: TDataProcessing; options: TOptionReaderWrapper);

  procedure assignOptions(other: TProcessingContext);
  procedure assignActions(other: TProcessingContext);

  function clone: TDataProcessing; override;


  function process(data: TData): TFollowToList; override;
end;

type EInvalidArgument = Exception;

{ TOptionReaderFromObject }

constructor TOptionReaderFromObject.create(aobj: TXQValueObject);
begin
  obj := aobj;
end;

function TOptionReaderFromObject.read(const name: string; out value: string): boolean;
var
  temp: TXQValue;
begin
  result := obj.hasProperty(name, @temp);
  if result then value := temp.toString;
end;

function TOptionReaderFromObject.read(const name: string; out value: integer): boolean;
var
  temp: TXQValue;
begin
  result := obj.hasProperty(name, @temp);
  if result then value := temp.toInt64;
end;

function TOptionReaderFromObject.read(const name: string; out value: boolean): boolean;
var
  temp: TXQValue;
begin
  result := obj.hasProperty(name, @temp);
  if result then value := temp.toBoolean;
end;

function TOptionReaderFromObject.read(const name: string; out value: Extended): boolean;
var
  temp: TXQValue;
begin
  result := obj.hasProperty(name, @temp);
  if result then value := temp.toDecimal;
end;

{ TOptionReaderFromCommandLine }

constructor TOptionReaderFromCommandLine.create(cmdLine: TCommandLineReader);
begin
  acmdLine := cmdLine;
end;

function TOptionReaderFromCommandLine.read(const name: string; out value: string): boolean;
begin
  value := acmdLine.readString(name);
  result := acmdLine.existsProperty(name);
end;

function TOptionReaderFromCommandLine.read(const name: string; out value: integer): boolean;
begin
  value := acmdLine.readInt(name);
  result := acmdLine.existsProperty(name);
end;

function TOptionReaderFromCommandLine.read(const name: string; out value: boolean): boolean;
begin
  value := acmdLine.readFlag(name);
  result := acmdLine.existsProperty(name);
end;

function TOptionReaderFromCommandLine.read(const name: string; out value: Extended): boolean;
begin
  value := acmdLine.readFloat(name);
  result := acmdLine.existsProperty(name);
end;

{ TDownload }

function TDownload.process(data: TData): TFollowToList;
var
  realUrl: String;
  j: LongInt;
  realPath: String;
  realFile: String;
  downloadTo: String;
begin
  result := nil;
  if cgimode or not allowFileAccess then
    raise Exception.Create('Download not permitted');

  realUrl := strSplitGet('?', data.fullurl);
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

  downloadTo := htmlparser.replaceVars(Self.downloadTarget);
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
  //    .             save in current directory with name index.html        TODO: fix
  //    -             print to stdout
  if downloadTo = '-' then begin
    w(data.rawdata);
    exit;
  end;
  if downloadTo = './.' then downloadTo:=realPath+realFile
  else if strEndsWith(downloadTo, '/.') then begin
    SetLength(downloadto,Length(downloadTo)-1);
    downloadTo:=downloadTo+realFile;
  end else if strEndsWith(downloadTo, '/') then begin
    downloadTo:=downloadTo+realPath+realFile;
  end;
  if strEndsWith(downloadTo, '/') or (downloadTo = '') then downloadTo += 'index.html';
  parent.printStatus('**** Save as: '+downloadTo+' ****');
  if pos('/', downloadTo) > 0 then
    ForceDirectories(StringReplace(StringReplace(copy(downloadTo, 1, strRpos('/', downloadTo)-1), '//', '/', [rfReplaceAll]), '/', DirectorySeparator, [rfReplaceAll]));
  strSaveToFileUTF8(StringReplace(downloadTo, '/', DirectorySeparator, [rfReplaceAll]), data.rawdata);
end;

procedure TDownload.readOptions(reader: TOptionReaderWrapper);
begin
  reader.read('download', downloadTarget);
end;

function TDownload.clone: TDataProcessing;
begin
  result := TDownload.Create;
  TDownload(result).downloadTarget:=downloadTarget;
end;


{ THTTPRequest }

constructor THTTPRequest.create(aurl: string);
begin
  url := aurl;
end;

function THTTPRequest.clone: TFollowTo;
begin
  result := THTTPRequest.create(url);
  THTTPRequest(result).method:=method;
  THTTPRequest(result).data:=data;
end;

function THTTPRequest.retrieve(parent: TProcessingContext): TData;
var
  i: Integer;
begin
  if not allowInternetAccess then raise Exception.Create('Internet access not permitted');
  if assigned(onPrepareInternet) then  internet := onPrepareInternet(parent.userAgent, parent.proxy);
  parent.printStatus('**** Retrieving:'+url+' ****');
  if data <> '' then parent.printStatus('Data: '+data);
  result := tdata.create('', url);
  if assigned(onRetrieve) then result.rawdata := onRetrieve(method, url, data);
  if parent.printReceivedHeaders and assigned(internet) then begin
    parent.printStatus('** Headers: (status: '+inttostr(internet.lastHTTPResultCode)+')**');
    for i:=0 to internet.lastHTTPHeaders.Count-1 do
      wln(internet.lastHTTPHeaders[i]);
  end;
  if Assigned(internet) then result.contenttype := internet.getLastHTTPHeader('Content-Type');
end;

procedure THTTPRequest.replaceVariables;
begin
  inherited replaceVariables;
end;

function THTTPRequest.equalTo(ft: TFollowTo): boolean;
begin
  result := (ft is THTTPRequest) and (THTTPRequest(ft).url = url) and (THTTPRequest(ft).method = method) and (THTTPRequest(ft).data = data);
end;

{ TFileRequest }

constructor TFileRequest.create(aurl: string);
begin
  url := aurl;
end;

function TFileRequest.clone: TFollowTo;
begin
  result := TFileRequest.create(url);
end;

function TFileRequest.retrieve(parent: TProcessingContext): TData;
begin
  if not allowFileAccess then raise Exception.Create('File access not permitted');
  parent.printStatus('**** Retrieving:'+url+' ****');
  result := tdata.create(strLoadFromFileUTF8(url), url);
end;

procedure TFileRequest.replaceVariables;
begin
  url := htmlparser.replaceVars(url);
end;

function TFileRequest.equalTo(ft: TFollowTo): boolean;
begin
  result := (ft is TFileRequest) and (TFileRequest(ft).url = url);
end;

{ TDirectDataRequest }

constructor TDirectDataRequest.create(adata: string);
begin
  data := adata;
end;

function TDirectDataRequest.clone: TFollowTo;
begin
  result := TDirectDataRequest.create(data);
end;

function TDirectDataRequest.retrieve(parent: TProcessingContext): TData;
begin
  result := tdata.Create(data, copy(data, 1, 128));
  if length(data) > length(result.fullurl) then result.fullurl := result.fullurl + '...';
end;

function TDirectDataRequest.equalTo(ft: TFollowTo): boolean;
begin
  result := (ft is TDirectDataRequest) and (TDirectDataRequest(ft).data = data);
end;

constructor TData.create(somedata: string; aurl: string; acontenttype: string);
begin
  rawdata := somedata;
  fullurl := aurl;
  contenttype := acontenttype;
end;

{ TStdinDataRequest }

function TStdinDataRequest.clone: TFollowTo;
begin
  result := TStdinDataRequest.create;
end;

function TStdinDataRequest.retrieve(parent: TProcessingContext): TData;
begin
  result := TData.Create(strReadFromStdin(), '-');
end;

function TStdinDataRequest.equalTo(ft: TFollowTo): boolean;
begin
  result := false; //always different??
end;


class function TFollowTo.createFromRetrievalAddress(data: string): TFollowTo;
begin
  data := trim(data);
  if cgimode then
    exit(TDirectDataRequest.create(data));
  if data = '-' then
    exit(TStdinDataRequest.create());
  case guessType(data) of
    rtRemoteURL: result := THTTPRequest.Create(data);
    rtFile: result := TFileRequest.create(data);
    rtEmpty, rtXML: result := TDirectDataRequest.create(data);
    else raise Exception.Create('Impossible 232');
  end;
  //todo: handle completely empty data ''
end;

procedure TFollowTo.replaceVariables;
begin
  //empty
end;

{ TFollowToWrapper }

function TFollowToWrapper.process(data: TData): TFollowToList;
var
  res: TFollowTo;
begin
  res := followTo.clone;
  res.replaceVariables();

  result := TFollowToList.Create;
  Result.Add(res);
end;

function TFollowToWrapper.clone: TDataProcessing;
begin
  result := TFollowToWrapper.Create;
  TFollowToWrapper(result).followTo := followTo.clone;
end;

{ TFollowToList }

constructor TFollowToList.Create;
begin
  inherited;
  OwnsObjects:=true;
end;

procedure TFollowToList.merge(l: TFollowToList);
var
  i: Integer;
begin
  if l = nil then exit;
  for i := 0 to l.Count-1 do
    inherited add(TFollowTo(l[i]));
  l.OwnsObjects:=false;
  l.free;
end;

function TFollowToList.first: TFollowTo;
begin
  result := TFollowTo(inherited first);
end;

var globalDuplicationList: TFollowToList;

procedure TFollowToList.add(ft: TFollowTo);
begin
  if (globalDuplicationList <> nil) and (self <> globalDuplicationList) then begin
    if globalDuplicationList.containsEqual(ft) then exit;
    globalDuplicationList.add(ft.clone);
  end;
  inherited add(ft);
end;


procedure TFollowToList.merge(dest: IXQValue; basedata: TData; parent: TProcessingContext);
var x: IXQValue;
    temp: TProcessingContext;
    n: TTreeNode;
begin
  case dest.kind of
    pvkUndefined: exit;
    pvkObject: if parent <> nil then begin
      temp := TProcessingContext.Create();
      temp.assignOptions(parent);
      temp.parent := parent;
      temp.mergeWithObject(dest as TXQValueObject);
      if length(temp.actions) = 0 then
        temp.assignActions(parent);
      merge(temp.process(basedata));
      temp.Free;
    end;
    pvkSequence:
      for x in dest do
        merge(x, basedata, parent);
    pvkNode: begin
      n := dest.toNode;
      if n = nil then exit;
      if n.typ <> tetOpen then addBasicUrl(dest.toString, basedata.fullurl)
      else if SameText(n.value, 'a') then addBasicUrl(n.getAttribute('href', ''), basedata.fullurl)
      else if SameText(n.value, 'frame') or SameText(n.value, 'iframe') or SameText(n.value, 'img') then addBasicUrl(n.getAttribute('src', ''), basedata.fullurl)
      else addBasicUrl(n.deepNodeText(), basedata.fullurl);
    end;
    else addBasicUrl(dest.toString, basedata.fullurl);
  end;
end;

function TFollowToList.containsEqual(ft: TFollowTo): boolean;
var
  i: Integer;
begin
  for i := 0 to count-1 do
    if (self[i] as TFollowTo).equalto(ft) then exit(true);
  exit(false);
end;

procedure TFollowToList.addBasicUrl(absurl: string; baseurl: string);
begin
  if (guessType(baseurl) in [rtFile, rtRemoteURL]) and (guessType(absurl) = rtFile) then
    absurl := strResolveURI(absurl, baseurl);
  Add(TFollowTo.createFromRetrievalAddress(absurl));
end;

procedure TDataProcessing.readOptions(reader: TOptionReaderWrapper);
begin
  //empty
end;

procedure TDataProcessing.initFromCommandLine(cmdLine: TCommandLineReader);
var
  temp: TOptionReaderFromCommandLine;
begin
  temp := TOptionReaderFromCommandLine.Create(cmdLine);
  readOptions(temp);
  temp.free;
end;

procedure TDataProcessing.mergeWithObject(obj: TXQValueObject);
var
  temp: TOptionReaderFromObject;
begin
  temp := TOptionReaderFromObject.create(obj);
  readOptions(temp);
  temp.free;
end;

{ TExtraction }

constructor TExtraction.create;
begin
  printVariables:=[pvCondensedLog];
end;

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

procedure TExtraction.readOptions(reader: TOptionReaderWrapper);
var
  tempstr: string;
begin
  reader.read('extract', extract);  //todo. option: extract-file
  extract:=trim(extract);
  if reader.read('extract-exclude', tempstr) then extractExclude := strSplit(tempstr, ',', false);
  if reader.read('extract-include', tempstr) then extractInclude := strSplit(tempstr, ',', false);
  if reader.read('extract-kind', tempstr) then setExtractKind(tempstr);

  if reader.read('template-file', tempstr)  then begin
    extract := strLoadFromFileChecked(tempstr);
    extractKind := ekMultipage;
  end;
  if reader.read('template-action', tempstr) then templateActions := strSplit(tempstr, ',', false);

  reader.read('default-variable-name', defaultName);
  reader.read('print-type-annotations', printTypeAnnotations);
  reader.read('hide-variable-names', hideVariableNames);

  if reader.read('print-variables', tempstr) then setVariables(tempstr);

  if reader.read('printed-node-format', tempstr) then begin
      case tempstr of
        'text': printedNodeFormat:=tnsText;
        'xml': printedNodeFormat:=tnsXML;
        'html': printedNodeFormat:=tnsHTML;
        else raise EInvalidArgument.create('Unknown node format option: '+tempstr);
      end;
    end else if reader.read('output-format', tempstr) then
      case tempstr of
        'xml': printedNodeFormat:=tnsXML;
        'html': printedNodeFormat:=tnsHTML;
      end;
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

procedure TProcessingContext.printStatus(s: string);
begin
  if not quiet then writeln(stderr, s);
end;

procedure TProcessingContext.readOptions(reader: TOptionReaderWrapper);
var
  tempstr: string;
begin

  if allowInternetAccess then begin
    reader.read('wait', wait);
    reader.read('user-agent', userAgent);
    reader.read('proxy', proxy);
    //reader.read('post', Post);
    //reader.read('method', method); moved to
    reader.read('print-received-headers', printReceivedHeaders);
  end;

  if reader.read('output-encoding', tempstr) then setOutputEncoding(tempstr); //allows object returned by extract to change the output-encoding




  {if cmdLine.readString('follow-file') <> '' then follow := strLoadFromFileChecked(cmdLine.readString('follow-file'))
  else begin
    follow := cmdLine.readString('follow');
    if follow = '-' then follow :=strReadFromStdin;
  end;} //handled in variableRead
  reader.read('follow', follow);
  reader.read('follow-exclude', tempstr); followExclude := strSplit(tempstr, ',', false);
  reader.read('follow-include', tempstr); followInclude := strSplit(tempstr, ',', false);
//todo  reader.read('follow-level', followMaxLevel);


//deprecated:   if (length(extractions) > 0) and (extractions[high(extractions)].extractKind = ekMultipage) and (length(urls) = 0) then
//    arrayAdd(urls, '<empty/>');
//  if cmdLine.readString('data') <> '' then arrayAdd(urls, cmdLine.readString('data'));


end;


procedure TProcessingContext.mergeWithObject(obj: TXQValueObject);
var
  tempreader: TOptionReaderFromObject;
  temp: TXQValue;
  i: integer;
begin
  inherited;

  tempreader := TOptionReaderFromObject.create(obj);
  if length(actions) > 0 then
    for i := 0 to high(actions) do
      actions[i].readOptions(tempreader);
  {todo:
  if length(extractions) > 0 then
    extractions[high(extractions)].mergeWithObject(obj);

  if obj.hasProperty('download', @temp) then arrayAdd(downloads, temp.toString);

  if obj.hasProperty('follow-file', @temp) then follow := strLoadFromFileChecked(temp.toString)

  setlength(urls, 0);

  if (length(extractions) > 0) and (extractions[high(extractions)].extractKind = ekMultipage) and (length(urls) = 0) then begin
    arrayAdd(urls, '<empty/>');
    arrayAdd(urlsLevel, stepLevel);
  end;            }
  if obj.hasProperty('url', @temp) then
    readNewDataSource(TFollowTo.createFromRetrievalAddress(temp.toString), tempreader);
  tempreader.free;
end;

procedure TProcessingContext.readNewDataSource(data: TFollowTo; options: TOptionReaderWrapper);
begin
  SetLength(dataSources, length(dataSources) + 1);
  dataSources[high(dataSources)] := TFollowToWrapper.Create;
  dataSources[high(dataSources)].parent := self;
  TFollowToWrapper(dataSources[high(dataSources)]).followTo := data;
  TFollowToWrapper(dataSources[high(dataSources)]).readOptions(options);
end;

procedure TProcessingContext.readNewAction(action: TDataProcessing; options: TOptionReaderWrapper);
begin
  SetLength(actions, length(actions) + 1);
  actions[high(actions)] := action;
  actions[high(actions)].parent := self;
  actions[high(actions)].readOptions(options);
end;

procedure TProcessingContext.assignOptions(other: TProcessingContext);
begin
  //neither dataSources nor actions: array of TDataProcessing; ?? todo

  {follow := other.follow;
  followExclude := other.followExclude;
  followInclude := other.followInclude;
  followTo := other.followTo ; //todo ??? disabled, leads to endless recursion
  }

  wait := other.wait;
  userAgent := other.userAgent;
  proxy := other.proxy;
  printReceivedHeaders:=other.printReceivedHeaders;

  quiet := other.quiet;

  compatibilityNoExtendedStrings := other.compatibilityNoExtendedStrings;
  compatibilityNoObjects := other.compatibilityNoObjects;
  compatibilityStrictTypeChecking := other.compatibilityStrictTypeChecking;
  compatibilityStrictNamespaces := other.compatibilityStrictNamespaces;
end;

procedure TProcessingContext.assignActions(other: TProcessingContext);
var i: integer;
begin
  setlength(actions, length(other.actions));
  for i := 0 to high(actions) do
    actions[i] := other.actions[i].clone;
end;

function TProcessingContext.clone: TDataProcessing;
var
  i: Integer;
begin
  result := TProcessingContext.Create;
  TProcessingContext(result).assignOptions(self);
  TProcessingContext(result).assignActions(self);
  setlength(TProcessingContext(result).dataSources, length(dataSources));
  for i := 0 to high(TProcessingContext(result).dataSources) do
    TProcessingContext(result).dataSources[i] := dataSources[i].clone;
  if nextSibling <> nil then
    TProcessingContext(result).nextSibling := nextSibling.clone as TProcessingContext;
end;

function encodingName(e: TEncoding): string;
begin
  case e of
    eWindows1252: result := 'ISO-8859-1';
    eUTF16BE, eUTF16LE: result := 'UTF-16';
    eUTF32BE, eUTF32LE: result := 'UTF-32';
    else result := 'UTF-8';
  end;
end;



function TProcessingContext.process(data: TData): TFollowToList;
var next, res: TFollowToList;
  procedure subProcess(data: TData);
  var
    i: Integer;
    tempProto, tempHost, tempPath: string;
  begin
    printStatus('**** Processing:'+data.fullurl+' ****');

    //alreadyProcessed.Add(urls[0]+#1+post);
    htmlparser.variableChangeLog.add('url', data.fullurl);
    decodeURL(data.fullurl, tempProto, tempHost, tempPath);
    htmlparser.variableChangeLog.add('host', tempHost);
    htmlparser.variableChangeLog.add('path', tempPath);


    for i := 0 to high(actions) do
      next.merge(actions[i].process(data));
    if follow <> '' then begin
      if res = nil then res := TFollowToList.Create;

      htmlparser.OutputEncoding := eUTF8; //todo correct encoding?

      if follow[1] = '<' then begin //assume my template
        htmlparser.parseTemplate(follow); //todo reuse existing parser
        htmlparser.parseHTML(data.rawdata, data.fullurl, data.contenttype); //todo: optimize
        for i:=0 to htmlparser.variableChangeLog.count-1 do
          if ((length(followInclude) = 0) and (arrayIndexOf(followExclude, htmlparser.variableChangeLog.getName(i)) = -1)) or
             ((length(followInclude) > 0) and (arrayIndexOf(followInclude, htmlparser.variableChangeLog.getName(i)) > -1)) then
            res.merge(htmlparser.variableChangeLog.get(i), data, self);
      end else begin
        //assume xpath
        htmlparser.parseHTMLSimple(data.rawdata, data.fullurl, data.contenttype);
        xpathparser.RootElement := htmlparser.HTMLTree;
        xpathparser.ParentElement := xpathparser.RootElement;
        xpathparser.StaticContext. BaseUri := data.fullurl;
        if strBeginsWith(follow, 'xquery') then xpathparser.parseXQuery1(follow)
        else xpathparser.parseXPath2(follow);
        res.merge(xpathparser.evaluate(), data, self);
      end;

      if followTo <> nil then
        for i := 0 to res.Count - 1 do
          followto.process(TFollowTo(res[i]).retrieve(self));
    end;
  end;

var
  i: Integer;
begin

  //init
  xpathparser.AllowVariableUseInStringLiterals:= not compatibilityNoExtendedStrings;
  xpathparser.VariableChangelog.allowObjects:=not compatibilityNoObjects;
  htmlparser.variableChangeLog.allowObjects:=xpathparser.VariableChangelog.allowObjects;
  xpathparser.StaticContext.strictTypeChecking:=compatibilityStrictTypeChecking;
  xpathparser.StaticContext.useLocalNamespaces:=not compatibilityStrictNamespaces;

  //apply all actions to all data source
  next := TFollowToList.Create;
  res := nil;
  if data <> nil then subProcess(data);
  for i := 0 to high(dataSources) do
    next.merge(dataSources[i].process(nil));
  if (length(actions) = 0) and (follow = '') then
    exit(next);
  while next.Count > 0 do begin
    subProcess(next.First.retrieve(self));
    if wait > 0.001 then Sleep(trunc(wait * 1000));
    next.Delete(0);
  end;

  result := res;
  if nextSibling <> nil then begin
    res := nextSibling.process(nil);
    if result = nil then result := res
    else result.merge(res);
  end;
  { some examples:
      [ -e //title -f //a  ]
      http://www.google.de http://www.example.org -e //title

      http://www.google.de -e //a1 http://www.example.org -e //a2

      [ http://www.google.de  http://www.example.org ] -e //a2

      http://example.org -f //a [ -e //title ]

      http://example.org  [ -f //a -e //title -f //x ]

      http://example.org  [ http://google.de -e //title ]}
end;

procedure TExtraction.printExtractedValue(value: IXQValue);
var
  i: Integer;
  temp: TXQValueObject;
  x: IXQValue;
begin
  case outputFormat of
    ofAdhoc: begin
      if printTypeAnnotations then w(value.typeName+': ');
      if value is TXQValueSequence then begin
        i := 0;
        for x in value do begin
          if i <> 0 then wln();
          printExtractedValue(x);
          i += 1;
        end;
      end else if value is TXQValueNode then begin
        case printedNodeFormat of
          tnsText: w(value.toString);
          tnsXML: w(value.toNode.outerXML());
          tnsHTML: w(value.toNode.outerHTML());
          else raise EInvalidArgument.Create('Unknown node print format');
        end;
      end
      else if value is TXQValueObject then begin
        x := value.clone;
        temp := x as TXQValueObject;
        w('{');
        if temp.values.count > 0 then begin
          w(temp.values.getName(0)+': '); printExtractedValue(temp.values.get(0));
          for i:=1 to temp.values.count-1 do begin
            w(', '+ temp.values.getName(i)+ ': ');
            printExtractedValue(temp.values.get(i));
          end;
        end;
        w('}');
      end
      else w(value.toString);
    end;
    ofJsonWrapped: begin
      w(value.jsonSerialize(printedNodeFormat));
    end;
    ofXMLWrapped: begin
      w(value.xmlSerialize(printedNodeFormat, 'seq', 'e', 'object'));
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

procedure TExtraction.pageProcessed(unused: TMultipageTemplateReader; parser: THtmlTemplateParser);
var
  i: Integer;
begin
  if firstExtraction then begin
    firstExtraction := false;
    if outputFormat = ofXMLWrapped then wln('<e>');
  end else wln(outputArraySeparator[outputFormat]);

  printExtractedVariables(parser);

  for i := 0 to parser.variableChangeLog.count-1 do
    if parser.variableChangeLog.getName(i) = '_follow' then begin
      if currentFollowList = nil then currentFollowList := TFollowToList.Create;
      currentFollowList.merge(parser.variableChangeLog.get(i), currentData, parent);
    end;
end;

function TExtraction.process(data: TData): TFollowToList;
begin
  //set flags when first processed
  if extract = '-' then extract:=strReadFromStdin;
  if extractKind = ekAuto then
    if (extract <> '') and (extract[1] = '<') then extractKind:=ekTemplate
    else if strBeginsWith(extract, 'xquery') then extractKind:=ekXQuery
    else extractKind:=ekXPath;


  currentFollowList := nil;
  currentData:=data;

  if outputEncoding = eUnknown then htmlparser.OutputEncoding := outputEncoding
  else htmlparser.OutputEncoding := eUTF8;

  case extractKind of
    ekTemplate: begin
      htmlparser.UnnamedVariableName:=defaultName;
      htmlparser.parseTemplate(extract); //todo reuse existing parser
      htmlparser.parseHTML(data.rawdata, makeAbsoluteFilePath(data.fullurl), data.contenttype); //todo: full url is abs?
      pageProcessed(nil,htmlparser);
    end;
    ekXPath, ekCSS, ekXQuery: begin
      if firstExtraction then begin
        firstExtraction := false;
        if outputFormat = ofXMLWrapped then wln('<e>');
      end else wln(outputArraySeparator[outputFormat]);

      htmlparser.parseHTMLSimple(data.rawdata, data.fullurl, data.contenttype);
      xpathparser.RootElement := htmlparser.HTMLTree;
      xpathparser.ParentElement := xpathparser.RootElement;
      xpathparser.StaticContext.BaseUri := makeAbsoluteFilePath(data.fullurl);
      case extractKind of
        ekCSS: xpathparser.parseCSS3(extract); //todo: optimize
        ekXPath: xpathparser.parseXPath2(extract);
        ekXQuery: xpathparser.parseXQuery1(extract);
      end;
      printExtractedValue(xpathparser.evaluate());
      wln;
    end;
    ekMultipage: if assigned (onPrepareInternet) then begin
      multipage.onPageProcessed:=@pageProcessed;
      multipage.internet := onPrepareInternet(parent.userAgent, parent.proxy);
      multipagetemp := TMultiPageTemplate.create();
      multipagetemp.loadTemplateFromString(extract);
      multipage.setTemplate(multipagetemp);
      multipage.perform(templateActions);
      multipage.setTemplate(nil);
      multipagetemp.free;
    end
    else raise Exception.Create('Impossible');
  end;

  result := currentFollowList;
  currentFollowList := nil;
end;

procedure TExtraction.assignOptions(other: TExtraction);
begin
  extract := other.extract;
  extractExclude := other.extractExclude; SetLength(extractExclude, length(extractExclude));
  extractInclude := other.extractInclude; SetLength(extractInclude, length(extractInclude));
  extractKind := other.extractKind;
  templateActions := other.templateActions;
  SetLength(templateActions, length(templateActions));

  defaultName := other.defaultName;
  printVariables := other.printVariables;
  printTypeAnnotations := other.printTypeAnnotations;
  hideVariableNames := other.hideVariableNames;
  printedNodeFormat := other.printedNodeFormat;
end;

function TExtraction.clone: TDataProcessing;
begin
  result := TExtraction.create;
  TExtraction(result).assignOptions(self);
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
  parent.printStatus(state);
  case outputFormat of
    ofAdhoc: begin
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if not hideVariableNames then w(vars.Names[i] + ': ');
           printExtractedValue(vars.get(i));
           wln;
         end;
    end;
    ofJsonWrapped:
      if hideVariableNames then begin
        w('[');
        first := true;
        for i:=0 to vars.count-1 do begin
          if acceptName(vars.Names[i]) then begin
            if first then first := false
            else wln(', ');
            printExtractedValue(vars.get(i));
          end;
        end;
        wln(']');
      end else begin
        first := true;
        wln('{');
        setlength(tempUsed, vars.count);
        FillChar(tempUsed[0], sizeof(tempUsed[0])*length(tempUsed), 0);
        for i:=0 to vars.count-1 do begin
          if tempUsed[i] then continue;
          if acceptName(vars.Names[i]) then begin
            if first then first := false
            else wln(',');
            w(jsonStrEscape(vars.Names[i]) + ': ');
            values := vars.getAll(vars.Names[i]);
            if values.getSequenceCount = 1 then printExtractedValue(values)
            else begin
              w('[');
              printExtractedValue(values.getChild(1));
              for j:=2 to values.getSequenceCount do begin
                w(', ');
                printExtractedValue(values.getChild(j));
              end;
              w(']');
            end;
          end;
          for j := i + 1 to vars.count-1 do
            if vars.Names[i] = vars.Names[j] then tempUsed[j] := true;
        end;
        wln();
        wln('}');
    end;
    ofXMLWrapped: begin
      if hideVariableNames then begin
        w('<seq>');
        first := true;
        for i:=0 to vars.count-1 do begin
          if acceptName(vars.Names[i]) then begin
            if first then begin first := false; w('<e>');end
            else w('</e><e>');
            printExtractedValue(vars.get(i));
          end;
        end;
        if not first then w('</e>');
        wln('</seq>');
      end else begin
        wln('<object>');
        for i:=0 to vars.count-1 do
           if acceptName(vars.Names[i])  then begin
             w('<'+vars.Names[i] + '>');
             printExtractedValue(vars.Values[i]);
             wln('</'+vars.Names[i] + '>');
           end;
        wln('</object>');
      end;
    end;
  end;
end;






var i: Integer;
    temp: TStringArray;
    j: Integer;


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

procedure displayError(e: Exception; printPartialMatches: boolean = false);
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
    ofJsonWrapped: begin
      sayln('{"_error": {');
      say('"_message": '+jsonStrEscape(e.Message));
      if printPartialMatches then begin
        sayln(', ');
        sayln('"_partial-matches": [');
        temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
        if length(temp) > 0 then
          say(jsonStrEscape(temp[j]));
        for j := 1 to high(temp) do  say (', '+LineEnding+jsonStrEscape(temp[j]));
        sayln(']');
      end else sayln('');
      sayln('}}');
      if cgimode then
        sayln(']');
    end;
    ofXMLWrapped: begin
      sayln('<error>');
      sayln('<message>'+xmlStrEscape(e.Message)+'</message>');
      if printPartialMatches then begin
        sayln('<partial-matches><![CDATA[');
        temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
        for j := 0 to high(temp) do  sayln( temp[j]);
        sayln(']]></partial-matches>');
      end;
      sayln('</error>');
      if cgimode then
        sayln('</seq>');
    end;
    ofAdhoc: begin
      sayln( 'Error:');
      sayln( e.Message);
      if printPartialMatches then begin
        sayln('');
        sayln( 'Partial matches:');
        temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
        for j := 0 to high(temp) do  sayln( temp[j]);
      end;
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

var currentContext: TProcessingContext;
    cmdlineWrapper: TOptionReaderFromCommandLine;

procedure variableRead(pseudoself: TObject; sender: TObject; const name, value: string);
begin
  if (name = 'follow') or (name = 'follow-file') or ((name = '') and (length(currentContext.actions) > 0))  then begin
    if name = 'follow-file' then
      if value = '-' then TCommandLineReaderBreaker(sender).overrideVar('follow', '-')
      else TCommandLineReaderBreaker(sender).overrideVar('follow', strLoadFromFileChecked(value));

    //writeln(stderr,name,'=',value);
    currentContext.readOptions(cmdlineWrapper);
    TCommandLineReaderBreaker(sender).clearNameless;

    if name <> '' then begin
      //following (applying the later commands to the data read by the current context)
      currentContext.followTo := TProcessingContext.Create;
      currentContext.followTo.assignOptions(currentContext);
      currentContext.followTo.parent := currentcontext;
      currentContext := currentContext.followTo;
    end else begin
      //sibling (later commands form a new context, unrelated to the previous one)
      currentContext.nextSibling := TProcessingContext.Create;
      currentContext.nextSibling.assignOptions(currentContext);
      currentContext.nextSibling.parent := currentContext.parent;
      currentContext := currentContext.nextSibling;
      currentContext.readNewDataSource(TFollowTo.createFromRetrievalAddress(value), cmdlineWrapper);
    end;

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

    TCommandLineReaderBreaker(sender).overrideVar(name, value);
    currentContext.readNewAction(TExtraction.Create, cmdlineWrapper);
  end else if name = 'download' then begin
    TCommandLineReaderBreaker(sender).overrideVar(name, value);
    currentContext.readNewAction(TDownload.Create, cmdlineWrapper)
  end else if (name = '') then begin
    {if value = '[' then pushCommandLineOptions
    else if value = ']' then popCommandLineOptions
    else todo } currentContext.readNewDataSource(TFollowTo.createFromRetrievalAddress(value), cmdlineWrapper);
  end;
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
  baseContext: TProcessingContext;
begin
  //normalized formats (for use in unittests)
  DecimalSeparator:='.';
  ThousandSeparator:=#0;
  ShortDateFormat:='YYYY-MM-DD';
  LongDateFormat:='YYYY-MM-DD';
  {$ifdef win32}systemEncodingIsUTF8:=getACP = CP_UTF8;{$endif}


  mycmdline.onOptionRead:=TOptionReadEvent(procedureToMethod(TProcedure(@variableRead)));
  mycmdline.allowOverrides:=true;

  mycmdLine.declareString('data', 'Data/URL to process (--data= prefix can be omitted)');
  mycmdLine.declareString('download', 'Downloads/saves the data to a given filename (- prints to stdout, . uses the filename of the url)');

  mycmdLine.beginDeclarationCategory('Extraction options:');

  mycmdLine.declareString('extract', joined(['Expression to extract from the data.','If it starts with < it is interpreted as template, otherwise as XPath 2 expression']));
  mycmdline.addAbbreviation('e');
  mycmdLine.declareString('extract-exclude', 'Comma separated list of variables ignored in an extract template. (black list) (default _follow)', '_follow');
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
    mycmdLine.declareString('method', 'Http method to use (e.g. GET, POST, PUT)', 'GET');
    mycmdLine.declareFlag('print-received-headers', 'Print the received headers');
  end;

  mycmdLine.beginDeclarationCategory('Output options:');

  mycmdLine.declareFlag('quiet','Do not print status information to stderr', 'q');
  mycmdLine.declareString('default-variable-name', 'Variable name for values read in the template without explicitely given variable name', 'result');
  mycmdLine.declareString('print-variables', joined(['Which of the separate variable lists are printed', 'Comma separated list of:', '  log: Prints every variable value', '  final: Prints only the final value of a variable, if there are multiple assignments to it', '  condensed-log: Like log, but removes assignments to object properties(default)']), 'condensed-log');
  mycmdLine.declareFlag('print-type-annotations','Prints all variable values with type annotations (e.g. string: abc, instead of abc)');
  mycmdLine.declareFlag('hide-variable-names','Do not print the name of variables defined in an extract template');
  mycmdLine.declareString('printed-node-format', 'Format of an extracted node: text, html or xml');
  mycmdLine.declareString('output-format', 'Output format: adhoc (simple human readable), json or xml', 'adhoc');
  mycmdLine.declareString('output-encoding', 'Character encoding of the output. utf-8 (default), latin1, utf-16be, utf-16le, oem (windows console) or input (no encoding conversion)', 'utf-8');
  mycmdLine.declareString('output-header', 'Header for the output. (e.g. <!DOCTYPE html>, default depends on output-format)', '');

  mycmdLine.beginDeclarationCategory('XPath/XQuery compatibility options:');

  mycmdline.declareFlag('no-extended-strings', 'Disables the replacements of variables in double quoted strings, like "$varname;"');
  mycmdline.declareFlag('no-objects', 'Disables the object.property syntax like in (object(("a", x)).a)');
  mycmdline.declareFlag('strict-type-checking', 'Disables weakly typing ("1" + 2 will raise an error, otherwise it evaluates to 3)');
  mycmdline.declareFlag('strict-namespaces', 'Disables the usage of undeclared namespace. Otherwise foo:bar always matches an element with prefix foo.');

  mycmdLine.declareFlag('version','Print version number ('+IntToStr(majorVersion)+'.'+IntToStr(minorVersion)+')');
  mycmdLine.declareFlag('usage','Print help, examples and usage information');

  currentContext := TProcessingContext.Create;
  baseContext := currentContext;

  cmdlineWrapper := TOptionReaderFromCommandLine.create(mycmdline);
  mycmdLine.parse();

  if mycmdline.readFlag('version') then
    printVersion;
  if mycmdline.readFlag('usage') then begin
    printUsage;
    exit;
  end;

  currentContext.readOptions(cmdlineWrapper);
  if length(currentContext.actions) <> 0 then currentContext.actions[high(currentContext.actions)].readOptions(cmdlineWrapper);

  if (currentContext.parent <> nil) and (length(currentContext.actions) = 0) and (length(currentContext.dataSources) = 0) then begin
    //this allows a trailing follow to recurse
    currentContext.follow := currentContext.parent.follow;
    currentContext.followExclude := currentContext.parent.followExclude;
    currentContext.followInclude := currentContext.parent.followInclude;
    currentContext.followTo := currentContext;
    currentContext.actions := currentContext.parent.actions;
    SetLength(currentContext.actions, length(currentContext.actions));
    for i := 0 to high(currentContext.actions) do
      currentContext.actions[i] := currentContext.actions[i].clone;
  end;

  if (currentContext.parent = nil) and (length(currentContext.dataSources) = 0) and (length(currentContext.actions) > 0) then begin
    //this allows data less evaluations, like xidel -e 1+2+3
    currentContext.readNewDataSource(TFollowTo.createFromRetrievalAddress('<empty/>'), cmdlineWrapper);
  end;
  cmdlineWrapper.Free;

  outputHeader := mycmdline.readString('output-header');
  if mycmdLine.readString('output-format') = 'adhoc' then
    outputFormat:=ofAdhoc
  else if mycmdLine.readString('output-format') = 'html' then begin
    outputFormat:=ofAdhoc;
    if not mycmdline.existsProperty('output-header') then outputHeader:='<!DOCTYPE html>'+LineEnding;
  end else if mycmdLine.readString('output-format') = 'xml' then begin
    outputFormat:=ofAdhoc;
    if not mycmdline.existsProperty('output-header') then outputHeader:='<?xml version="1.0" encoding="'+ encodingName(outputEncoding)+'"?>'+LineEnding;
  end else if mycmdLine.readString('output-format') = 'xml-wrapped' then begin
    outputFormat:=ofXMLWrapped;
    if not mycmdline.existsProperty('output-header') then outputHeader:='<?xml version="1.0" encoding="'+ encodingName(outputEncoding)+'"?>'+LineEnding;
  end else if (mycmdLine.readString('output-format') = 'json') or (mycmdLine.readString('output-format') = 'json-wrapped') then begin
    outputFormat:=ofJsonWrapped;
  end else raise EInvalidArgument.Create('Unknown output format: ' + mycmdLine.readString('output-format'));

  if assigned(onPreOutput) then onPreOutput();

  if outputHeader <> '' then w(outputHeader);
  case outputFormat of
    ofJsonWrapped: wln('[');
    ofXMLWrapped: wln('<seq>');
  end;

  htmlparser:=THtmlTemplateParserBreaker.create;
  htmlparser.TemplateParser.parsingModel:=pmHTML;
  htmlparser.KeepPreviousVariables:=kpvKeepValues;
  if allowInternetAccess then begin
    multipage := TTemplateReaderBreaker.create();
    multipage.parser:=htmlparser;
  end;
  xpathparser := htmlparser.QueryEngine;

  globalDuplicationList := TFollowToList.Create;
  try
    baseContext.process(nil);
  except
    on e: EHTMLParseException do begin
      displayError(e, true);
     // if not cgimode then raise;
    end;
    on e: EHTMLParseMatchingException do begin
      displayError(e, true);
     // if not cgimode then raise;
    end;
    on e: EXQEvaluationException do begin
      displayError(e);
     // if not cgimode then raise;
    end;
    on e: EXQParsingException do begin
      displayError(e);
     // if not cgimode then raise;
    end;
  end;
  if allowInternetAccess then multipage.Free
  else htmlparser.free;
  mycmdLine.free;
  globalDuplicationList.Free;

  if outputFormat = ofJsonWrapped then wln(']')
  else if outputFormat = ofXMLWrapped then begin
    if not firstExtraction then wln('</e>');
    wln('</seq>');
  end;

end;


end.

