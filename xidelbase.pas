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
  internetaccess, contnrs, dregexpr, simplexmltreeparserfpdom,
  rcmdline
  ;

var cgimode: boolean = false;
    allowInternetAccess: boolean = true;
    allowFileAccess: boolean = true;
    xqueryDefaultCollation: string = '';
    mycmdline: TCommandLineReader;
    defaultUserAgent: string = 'Mozilla/3.0 (compatible; Xidel)';

    majorVersion: integer = 0;
    minorVersion: integer = 8;

type TExtractionKind = (ekAuto, ekXPath2, ekXPath3, ekTemplate, ekCSS, ekXQuery1, ekXQuery3, ekMultipage);

var
    onPostParseCmdLine: procedure ();
    onPrepareInternet: function (const useragent, proxy: string): tinternetaccess;
    onRetrieve: function (const method, url, postdata, headers: string): string;
    onPreOutput: procedure (extractionKind: TExtractionKind);


procedure perform;

implementation

uses process, strutils, xquery_json, xquery_utf8;
//{$R xidelbase.res}

type TOutputFormat = (ofAdhoc, ofJsonWrapped, ofXMLWrapped, ofRawXML, ofRawHTML, ofBash, ofWindowsCmd);
var //output options
    outputFormat: TOutputFormat;
    outputEncoding: TEncoding = eUTF8;  //default to utf-8
    outputHeader, outputFooter: string;
    firstExtraction: boolean = true;
    outputArraySeparator: array[toutputformat] of string = ('',  ', ', '</e><e>', '', '', '', '');
    {$ifdef win32}systemEncodingIsUTF8: boolean = true;{$endif}

    internet: TInternetAccess;

type

TInputFormat = (ifAuto, ifXML, ifHTML, ifXMLStrict, ifJSON);

IData = interface //data interface, so we do not have to care about memory managment
function rawData: string;
function baseUri: string;
function displayBaseUri: string;
function contenttype: string;
function recursionLevel: integer;
function inputFormat: TInputFormat;
end;

{ THtmlTemplateParserBreaker }

THtmlTemplateParserBreaker = class(THtmlTemplateParser)
  ignorenamespaces: boolean;

  procedure initParsingModel(const data: IData);
  procedure parseHTML(const data: IData);
  procedure parseHTMLSimple(const data: IData);
  procedure closeVariableLog;

  procedure parseDoc(sender: TXQueryEngine; html,uri,contenttype: string; var node: TTreeNode);
end;

 { TTemplateReaderBreaker }

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
    currentRoot: TTreeNode;

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
  if strContains(s, '://') then exit;
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

{ TDataObject }

TDataObject = class(TInterfacedObject, IData)
{private todo: optimize
  fparsed: TTreeDocument;
  function GetParsed: TTreeDocument;
public}
private
  frawdata: string;
  fbaseurl, fdisplaybaseurl: string;
  fcontenttype: string;
  frecursionLevel: integer;
  finputformat: TInputFormat;
public
  function rawData: string;
  function baseUri: string;
  function displayBaseUri: string;
  function contentType: string;
  function recursionLevel: integer;
  function inputFormat: TInputFormat;
  constructor create(somedata: string; aurl: string; acontenttype: string = '');
  //property parsed:TTreeDocument read GetParsed;
end;

TDataProcessing = class;
TProcessingContext = class;

{ TFollowTo }

TFollowTo = class
  nextAction: integer; //the next action after the action yielding the data, so an action does not process its own follows
  inputFormat: TInputFormat;
  class function createFromRetrievalAddress(data: string): TFollowTo;

  function clone: TFollowTo; virtual; abstract;
  function retrieve(parent: TProcessingContext): IData; virtual; abstract;
  function retrieve(parent: TProcessingContext; recursionLevel: integer): IData; virtual;
  procedure replaceVariables; virtual;
  function equalTo(ft: TFollowTo): boolean; virtual; abstract;
  procedure readOptions(reader: TOptionReaderWrapper); virtual;
  procedure assign(other: TFollowTo); virtual;
end;

{ THTTPRequest }

THTTPRequest = class(TFollowTo)
  url: string;
  method: string;
  data: string;
  header: string;
  constructor create(aurl: string);
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): IData; override;
  procedure replaceVariables; override;
  function equalTo(ft: TFollowTo): boolean; override;
  procedure readOptions(reader: TOptionReaderWrapper); override;
end;

{ TFileRequest }

TFileRequest = class(TFollowTo)
  url: string;
  constructor create(aurl: string);
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): IData; override;
  procedure replaceVariables; override;
  function equalTo(ft: TFollowTo): boolean; override;
end;

{ TDirectDataRequest }

TDirectDataRequest = class(TFollowTo)
  data: string;
  constructor create(adata: string);
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): IData; override;
  function equalTo(ft: TFollowTo): boolean; override;
  //procedure replaceVariables;  do not replace vars in direct data
end;

{ TStdinDataRequest }

TStdinDataRequest = class(TFollowTo)
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): IData; override;
  function equalTo(ft: TFollowTo): boolean; override;
end;

{ TFollowToProcessedData }

TFollowToProcessedData = class(TFollowTo)
  data: IData;
  constructor create(d: IData);
  function clone: TFollowTo; override;
  function retrieve(parent: TProcessingContext): IData; override;
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
  procedure merge(l: TFollowToList; nextAction: integer = 0);
  function first: TFollowTo;

  procedure add(ft: TFollowTo);
  procedure merge(dest: IXQValue; basedata: IData; parent: TProcessingContext);

  function containsEqual(ft: TFollowTo): boolean;
private
  procedure addBasicUrl(absurl: string; baseurl: string);
end;



{ TDataProcessing }

TDataProcessing = class
  parent: TProcessingContext;
  function process(data: IData): TFollowToList; virtual; abstract;

  procedure readOptions(reader: TOptionReaderWrapper); virtual;
  procedure initFromCommandLine(cmdLine: TCommandLineReader); virtual;
  procedure mergeWithObject(obj: TXQValueObject); virtual;

  function clone(newparent: TProcessingContext): TDataProcessing; virtual; abstract;
end;


{ TDownload }

TDownload = class(TDataProcessing)
  downloadTarget: string;
  function process(data: IData): TFollowToList; override;
  procedure readOptions(reader: TOptionReaderWrapper); override;
  function clone(newparent: TProcessingContext): TDataProcessing; override;
end;

{ TExtraction }
TExtraction = class(TDataProcessing)
 extract: string;
 extractQueryCache: IXQuery;
 extractExclude, extractInclude: TStringArray;
 extractKind: TExtractionKind;

 templateActions: TStringArray;

 defaultName: string;
 printVariables: set of (pvLog, pvCondensedLog, pvFinal);
 printTypeAnnotations,  hideVariableNames: boolean;
 printedNodeFormat: TTreeNodeSerialization;

 constructor create;

 procedure setExtractKind(v: string);

 procedure readOptions(reader: TOptionReaderWrapper); override;

 procedure setVariables(v: string);

 procedure printExtractedValue(value: IXQValue; invariable: boolean);
 procedure printCmdlineVariable(const name: string; const value: IXQValue);
 procedure printExtractedVariables(vars: TXQVariableChangeLog; state: string; showDefaultVariable: boolean);
 procedure printExtractedVariables(parser: THtmlTemplateParser; showDefaultVariableOverride: boolean);

 function process(data: IData): TFollowToList; override;

 procedure assignOptions(other: TExtraction);
 function clone(newparent: TProcessingContext): TDataProcessing; override;
private
 currentFollowList: TFollowToList;
 currentData: IData;
 procedure pageProcessed(unused: TMultipageTemplateReader; parser: THtmlTemplateParser);
end;


{ TFollowToWrapper }

TFollowToWrapper = class(TDataProcessing)
  followTo: TFollowTo;
  procedure readOptions(reader: TOptionReaderWrapper); override;
  function process(data: IData): TFollowToList; override;
  function clone(newparent: TProcessingContext): TDataProcessing; override;
  destructor Destroy; override;
end;

{ TProcessingContext }

TProcessingContext = class(TDataProcessing)
  dataSources: array of TDataProcessing;
  actions: array of TDataProcessing;

  follow: string;
  followQueryCache: IXQuery;
  followExclude, followInclude: TStringArray;
  followTo: TProcessingContext;
  followMaxLevel: integer;

  nextSibling: TProcessingContext;

  wait: Extended;
  userAgent: string;
  proxy: string;
  printReceivedHeaders: boolean;
  errorHandling: string;

  quiet: boolean;

  ignoreNamespace: boolean;
  compatibilityNoExtendedStrings,compatibilityNoJSON, compatibilityNoJSONliterals, compatibilityOnlyJSONObjects, compatibilityNoExtendedJson, compatibilityStrictTypeChecking, compatibilityStrictNamespaces: boolean;
  compatibilityDotNotation: TXQPropertyDotNotation;
  noOptimizations: boolean;

  yieldDataToParent: boolean;

  procedure printStatus(s: string);

  procedure readOptions(reader: TOptionReaderWrapper); override;
  procedure mergeWithObject(obj: TXQValueObject); override;

  procedure addNewDataSource(source: TDataProcessing);
  procedure readNewDataSource(data: TFollowTo; options: TOptionReaderWrapper);
  procedure addNewAction(action: TDataProcessing);
  procedure readNewAction(action: TDataProcessing; options: TOptionReaderWrapper);

  procedure assignOptions(other: TProcessingContext);
  procedure assignActions(other: TProcessingContext);

  function clone(newparent: TProcessingContext): TDataProcessing; override;

  function last: TProcessingContext; //returns the last context in this sibling/follow chain

  procedure insertFictiveDatasourceIfNeeded; //if no data source is given in an expression (or an subexpression), but an aciton is there, <empty/> is added as data source

  function process(data: IData): TFollowToList; override;

  class function replaceEnclosedExpressions(expr: string): string;
  function replaceEnclosedExpressions(data: IData; expr: string): string;

  destructor destroy; override;
private
  procedure loadDataForQuery(const data: IData; const query: IXQuery);
  function evaluateQuery(const query: IXQuery; const data: IData; const allowWithoutReturnValue: boolean = false): IXQValue;
end;

type EInvalidArgument = Exception;

{ TDataObject }

function TDataObject.rawData: string;
begin
  result := frawdata;
end;

function TDataObject.baseUri: string;
begin
  result := fbaseurl;
end;

function TDataObject.displayBaseUri: string;
begin
  result := fdisplaybaseurl;
end;

function TDataObject.contentType: string;
begin
  result := fcontenttype;
end;

function TDataObject.recursionLevel: integer;
begin
  result := frecursionLevel;
end;

function TDataObject.inputFormat: TInputFormat;
  function checkRawDataForHtml: boolean; //following http://mimesniff.spec.whatwg.org/ (except allowing #9 as TT ) todo: what is with utf-16?
  var tocheck: array[1..16] of string = ('<!DOCTYPE HTML', '<HTML', '<HEAD', '<SCRIPT', '<IFRAME', '<H1', '<DIV', '<FONT', '<TABLE', '<A', '<STYLE', '<TITLE', '<B', '<BODY', '<BR', '<P');
    i: Integer;
  begin
    for i := low(tocheck) to high(tocheck) do
      if (length(rawData) > length(tocheck[i])) and
         (rawData[length(tocheck[i])+1] in [' ', '>', #9]) and
         (striBeginsWith(rawData, tocheck[i])) then
        exit(true);
    exit(false);
  end;

begin
  result := finputFormat;
  if result = ifAuto then
    if striEndsWith(baseUri, 'html') or striEndsWith(baseUri, 'htm')
       or striContains(contenttype, 'html')
       or checkRawDataForHtml() then
      Result := ifHTML
    else if strBeginsWith(rawData, '<?xml') then //mimesniff.spec says to check for this
      result := ifXML
    else if striEndsWith(baseUri, '.json')
         or striContains(contentType, 'json')
         or strBeginsWith(rawData, '{')
         or strBeginsWith(rawData, '[') then
      result := ifJSON
    else
      result := ifXML;
end;


{ TFollowToProcessedData }

constructor TFollowToProcessedData.create(d: IData);
begin
  data := d;
end;

function TFollowToProcessedData.clone: TFollowTo;
begin
  result :=  TFollowToProcessedData.Create(data);
  result.inputFormat := inputFormat;
end;

function TFollowToProcessedData.retrieve(parent: TProcessingContext): IData;
begin
  result := data;
end;

function TFollowToProcessedData.equalTo(ft: TFollowTo): boolean;
begin
  result := (ft is TFollowToProcessedData) and (TFollowToProcessedData(ft).data = data);
end;

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
  if result then value := temp.toFloat;
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


function TDownload.process(data: IData): TFollowToList;
var
  temp, realUrl: String;
  j: LongInt;
  realPath: String;
  realFile: String;
  downloadTo: String;
begin
  result := nil;
  if cgimode or not allowFileAccess then
    raise Exception.Create('Download not permitted');

  realUrl := data.baseUri;
  if guessType(realUrl) = rtRemoteURL then realurl := decodeURL(realUrl).path;

  j := strRpos('/', realUrl);
  if j = 0 then begin
    realPath := '';
    realFile := realUrl;
  end else begin
    realPath := copy(realUrl, 1, j);
    realFile := copy(realUrl, j + 1, length(realUrl) - j)
  end;
  while strBeginsWith(realPath, '/') do delete(realPath,1,1);

  downloadTo := parent.replaceEnclosedExpressions(data, Self.downloadTarget);
  if striBeginsWith(downloadTo, 'http://') then delete(downloadTo, 1, length('http://'));
  if striBeginsWith(downloadTo, 'https://') then delete(downloadTo, 1, length('https://'));
  {$ifdef win32}
  downloadTo := StringReplace(downloadTo, '\' , '/', [rfReplaceAll]);
  {$endif}

  //If downloadTo is a file                               : save with that name
  //If downloadTo is a directory and does not end with /  : save with basename
  //If downloadTo is a directory and does     end with /  : save with path and basename
  //If downloadTo is -                                    : print to stdout

  //example: Download abc/def/index.html
  //    foo/bar/xyz   save in directory foo/bar with name xyz
  //    foo/bar/      save in directory foo/bar/abc/def with name index.html
  //    foo/bar/.     save in directory foo/bar with name index.html
  //    foo           save in current directory with name foo
  //    ./            save in current directory/abc/def with name index.html
  //    ./.           save in current directory with name index.html
  //    .             save in current directory with name index.html
  //    -             print to stdout
  if downloadTo = '-' then begin
    w(data.rawdata);
    exit;
  end;
  if strEndsWith(downloadTo, '/.') then downloadTo := downloadTo + '/' + realFile
  else if strEndsWith(downloadTo, '/') then downloadTo := downloadTo + '/' + realPath + realFile
  else if DirectoryExists(downloadTo) or (downloadTo = '.' { <- redunant check, but safety first }) then downloadTo := downloadTo + '/' + realFile;
  if strEndsWith(downloadTo, '/') or (downloadTo = '') then downloadTo += 'index.html'; //sometimes realFile is empty
  parent.printStatus('**** Save as: '+downloadTo+' ****');
  if pos('/', downloadTo) > 0 then
    ForceDirectories(StringReplace(StringReplace(copy(downloadTo, 1, strRpos('/', downloadTo)-1), '//', '/', [rfReplaceAll]), '/', DirectorySeparator, [rfReplaceAll]));
  strSaveToFileUTF8(StringReplace(downloadTo, '/', DirectorySeparator, [rfReplaceAll]), data.rawdata);
end;

procedure TDownload.readOptions(reader: TOptionReaderWrapper);
begin
  reader.read('download', downloadTarget);
end;

function TDownload.clone(newparent: TProcessingContext): TDataProcessing;
begin
  result := TDownload.Create;
  result.parent := newparent;
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
  THTTPRequest(result).header:=header;
  result.assign(self);
end;

function THTTPRequest.retrieve(parent: TProcessingContext): IData;
  function doRetrieve(retries: integer): string;
    function matches(filter: string; value: string): boolean;
    var
      i: Integer;
    begin
      if length(filter) <> length(value) then exit(false);
      for i := 1 to length(filter) do
        if (filter[i] <> 'x') and (filter[i] <> value[i]) then
          exit(false);
      result := true;
    end;

  var
    errors: TStringArray;
    cur: TStringArray;
    i: Integer;
  begin
    try
      result := onRetrieve(method, url, data ,header);
    except
      on e: EInternetException do begin
        errors := strSplit(parent.errorHandling, ',');
        for i:=0 to high(errors) do begin
          cur := strSplit(errors[i], '=');
          if matches(trim(cur[0]), inttostr(e.errorCode)) then
            case trim(cur[1]) of
              'abort': raise;
              'ignore': exit('');
              'retry': begin
                if retries <= 0 then raise;
                Sleep(trunc(parent.wait*1000));
                exit(doRetrieve(retries-1));
              end;
            end;
        end;
        raise;
      end;
    end;
  end;

var
  i: Integer;
begin
  if not allowInternetAccess then raise Exception.Create('Internet access not permitted');
  if assigned(onPrepareInternet) then  internet := onPrepareInternet(parent.userAgent, parent.proxy);
  parent.printStatus('**** Retrieving:'+url+' ****');
  if data <> '' then parent.printStatus('Data: '+data);
  result := TDataObject.create('', url);
  if assigned(onRetrieve) then (result as TDataObject).frawdata := doRetrieve(10);
  if parent.printReceivedHeaders and assigned(internet) then begin
    parent.printStatus('** Headers: (status: '+inttostr(internet.lastHTTPResultCode)+')**');
    for i:=0 to internet.lastHTTPHeaders.Count-1 do
      wln(internet.lastHTTPHeaders[i]);
  end;
  if Assigned(internet) then (result as TDataObject).fcontenttype := internet.getLastHTTPHeader('Content-Type');
end;

procedure THTTPRequest.replaceVariables;
begin
  url := TProcessingContext.replaceEnclosedExpressions(url);
  method := TProcessingContext.replaceEnclosedExpressions(method);
  data := TProcessingContext.replaceEnclosedExpressions(data);
end;

function THTTPRequest.equalTo(ft: TFollowTo): boolean;
begin
  result := (ft is THTTPRequest) and (THTTPRequest(ft).url = url) and (THTTPRequest(ft).method = method) and (THTTPRequest(ft).data = data);
end;

function isStdin(s: string): boolean;
begin
  result := (s = '-') or (s = 'stdin:///') or (s = 'stdin:') or (s = 'stdin://');
end;

procedure THTTPRequest.readOptions(reader: TOptionReaderWrapper);
var temp: string;
begin
  inherited;
  if method <> '' then exit; //already initialized, must abort to keep stdin working (todo: allow postfix data/method options?)
  method:='GET';
  if reader.read('post', data) then
    method:='POST';
  if reader.read('method', temp) then begin
    method:=temp;
    if isStdin(method) then
      method := trim(strReadFromStdin);
  end;
  if isStdin(data) then
    data := strReadFromStdin;
  reader.read('header', header);
end;

{ TFileRequest }

constructor TFileRequest.create(aurl: string);
begin
  url := aurl;
end;

function TFileRequest.clone: TFollowTo;
begin
  result := TFileRequest.create(url);
  result.assign(self);
end;

function TFileRequest.retrieve(parent: TProcessingContext): IData;
begin
  if not allowFileAccess then raise Exception.Create('File access not permitted');
  parent.printStatus('**** Retrieving:'+url+' ****');
  result := TDataObject.create(strLoadFromFileUTF8(url), url);
  (result as TDataObject).fbaseurl:=makeAbsoluteFilePath((result as TDataObject).fbaseurl);
end;

procedure TFileRequest.replaceVariables;
begin
  url := TProcessingContext.replaceEnclosedExpressions(url);
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
  result.assign(self);
end;

function TDirectDataRequest.retrieve(parent: TProcessingContext): IData;
var
  partialData: String;
begin
  partialData := data;
  if length(partialData) > 80 then begin SetLength(partialData, 80); partialData += '...'; end;
  result := TDataObject.Create(data, 'data:,'+partialData);
  (result as TDataObject).fbaseurl := GetCurrentDir+DirectorySeparator;
  //if length(data) > length(result.fullurl) then (result as TDataObject).ffullurl := (result as TDataObject).ffullurl + '...';
end;

function TDirectDataRequest.equalTo(ft: TFollowTo): boolean;
begin
  if data = '<empty/>' then exit(false); //it is just a placeholder anyways
  result := (ft is TDirectDataRequest) and (TDirectDataRequest(ft).data = data);
end;

constructor TDataObject.create(somedata: string; aurl: string; acontenttype: string);
begin
  frawdata := somedata;
  fbaseurl:=aurl;
  fdisplaybaseurl:=aurl;
  fcontenttype := acontenttype;
end;

{ TStdinDataRequest }

function TStdinDataRequest.clone: TFollowTo;
begin
  result := TStdinDataRequest.create;
  result.assign(self);
end;

function TStdinDataRequest.retrieve(parent: TProcessingContext): IData;
begin
  result := TDataObject.Create(strReadFromStdin(), 'stdin:///');
  (Result as TDataObject).fbaseurl := GetCurrentDir + DirectorySeparator;
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
  if isStdin(data) then
    exit(TStdinDataRequest.create());
  case guessType(data) of
    rtRemoteURL: result := THTTPRequest.Create(data);
    rtFile: result := TFileRequest.create(data);
    rtEmpty, rtXML: result := TDirectDataRequest.create(data);
    else raise Exception.Create('Impossible 232');
  end;
  //todo: handle completely empty data ''
end;

function TFollowTo.retrieve(parent: TProcessingContext; recursionLevel: integer): IData;
begin
  result := retrieve(parent);
  if result is TDataObject then begin
    (result as TDataObject).frecursionLevel:=recursionLevel;
    (result as TDataObject).finputformat:=inputFormat;
  end;
end;

procedure TFollowTo.replaceVariables;
begin
  //empty
end;

procedure TFollowTo.readOptions(reader: TOptionReaderWrapper);
var ifs: string;
begin
  if reader.read('input-format', ifs) then
    case ifs of
      'auto': inputFormat:=ifAuto;
      'xml': inputFormat:=ifXML;
      'html': inputFormat:=ifHTML;
      'xml-strict': inputFormat:=ifXMLStrict;
      'json': inputFormat := ifJSON
      else raise Exception.Create('Invalid input-format: '+ifs);
    end;
end;

procedure TFollowTo.assign(other: TFollowTo);
begin
  inputFormat:=other.inputFormat;
end;

{ TFollowToWrapper }

procedure TFollowToWrapper.readOptions(reader: TOptionReaderWrapper);
begin
  followTo.readOptions(reader);
end;

function TFollowToWrapper.process(data: IData): TFollowToList;
var
  res: TFollowTo;
begin
  res := followTo.clone;
  res.replaceVariables();

  result := TFollowToList.Create;
  Result.Add(res);
end;

function TFollowToWrapper.clone(newparent: TProcessingContext): TDataProcessing;
begin
  result := TFollowToWrapper.Create;
  result.parent := newparent;
  TFollowToWrapper(result).followTo := followTo.clone;
end;

destructor TFollowToWrapper.Destroy;
begin
  followTo.free;
  inherited Destroy;
end;

{ TFollowToList }

constructor TFollowToList.Create;
begin
  inherited;
  OwnsObjects:=true;
end;

procedure TFollowToList.merge(l: TFollowToList; nextAction: integer = 0);
var
  i: Integer;
begin
  if l = nil then exit;
  for i := 0 to l.Count-1 do begin
    TFollowTo(l[i]).nextAction := nextAction;
    inherited add(TFollowTo(l[i]));
  end;
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
    if globalDuplicationList.containsEqual(ft) then begin ft.free; exit; end;
    globalDuplicationList.add(ft.clone);
  end;
  inherited add(ft);
end;


procedure TFollowToList.merge(dest: IXQValue; basedata: IData; parent: TProcessingContext);
var x: IXQValue;
    temp: TProcessingContext;
    n: TTreeNode;
begin
  if dest.kind <> pvkSequence then
    dest := xpathparser.evaluateXPath2('pxp:resolve-html(., $url)', dest);
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
    pvkNode: raise Exception.Create('Assert failure: Expected resolved url for following, but got raw '+dest.debugAsStringWithTypeAnnotation());
    else addBasicUrl(dest.toString, basedata.baseUri);
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
  else if striEqual(v, 'xpath') then extractKind:=ekXPath3
  else if striEqual(v, 'xquery') then extractKind:=ekXQuery3
  else if striEqual(v, 'xpath2') then extractKind:=ekXPath2
  else if striEqual(v, 'xquery1') then extractKind:=ekXQuery1
  else if striEqual(v, 'xpath3') then extractKind:=ekXPath3
  else if striEqual(v, 'xquery3') then extractKind:=ekXQuery3
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
  tempbool: boolean;
begin

  if allowInternetAccess then begin
    reader.read('wait', wait);
    reader.read('user-agent', userAgent);
    reader.read('proxy', proxy);
    //reader.read('post', Post);
    //reader.read('method', method); moved to
    reader.read('print-received-headers', printReceivedHeaders);
    reader.read('error-handling', errorHandling);
  end;

  if reader.read('output-encoding', tempstr) then setOutputEncoding(tempstr); //allows object returned by extract to change the output-encoding

  reader.read('quiet', quiet);

  {if cmdLine.readString('follow-file') <> '' then follow := strLoadFromFileChecked(cmdLine.readString('follow-file'))
  else begin
    follow := cmdLine.readString('follow');
    if follow = '-' then follow :=strReadFromStdin;
  end;} //handled in variableRead
  reader.read('follow', follow);
  reader.read('follow-exclude', tempstr); followExclude := strSplit(tempstr, ',', false);
  reader.read('follow-include', tempstr); followInclude := strSplit(tempstr, ',', false);
  reader.read('follow-level', followMaxLevel);

  reader.read('no-json', compatibilityNoJSON);
  reader.read('no-json-literals', compatibilityNoJSONliterals);
  reader.read('dot-notation', tempstr);
  case tempstr of
    'on': compatibilityDotNotation := xqpdnAllowFullDotNotation;
    'off': compatibilityDotNotation := xqpdnDisallowDotNotation;
    'unambiguous': compatibilityDotNotation := xqpdnAllowUnambiguousDotNotation;
  end;
  if reader.read('no-dot-notation', tempbool) then
    if tempbool = true then
      compatibilityDotNotation := xqpdnDisallowDotNotation;
  reader.read('only-json-objects', compatibilityOnlyJSONObjects);
  reader.read('no-extended-json', compatibilityNoExtendedJson);
  reader.read('strict-type-checking', compatibilityStrictTypeChecking);
  reader.read('strict-namespaces', compatibilityStrictNamespaces);
  reader.read('no-extended-strings', compatibilityNoExtendedStrings);
  reader.read('ignore-namespaces', ignoreNamespace);
  reader.read('no-optimizations', noOptimizations);


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
    readNewDataSource(TFollowTo.createFromRetrievalAddress(temp.toString), tempreader)
  else if obj.hasProperty('data', @temp) then
    readNewDataSource(TFollowTo.createFromRetrievalAddress(temp.toString), tempreader);
  tempreader.free;
end;

procedure TProcessingContext.addNewDataSource(source: TDataProcessing);
begin
  SetLength(dataSources, length(dataSources) + 1);
  dataSources[high(dataSources)] := source;
  dataSources[high(dataSources)].parent := self;
end;

procedure TProcessingContext.readNewDataSource(data: TFollowTo; options: TOptionReaderWrapper);
begin
  addNewDataSource(TFollowToWrapper.Create);
  TFollowToWrapper(dataSources[high(dataSources)]).followTo := data;
  if options <> nil then
    TFollowToWrapper(dataSources[high(dataSources)]).readOptions(options);
end;

procedure TProcessingContext.addNewAction(action: TDataProcessing);
begin
  SetLength(actions, length(actions) + 1);
  actions[high(actions)] := action;
  actions[high(actions)].parent := self;
end;

procedure TProcessingContext.readNewAction(action: TDataProcessing; options: TOptionReaderWrapper);
begin
  addNewAction(action);
  actions[high(actions)].readOptions(options);
end;

procedure TProcessingContext.assignOptions(other: TProcessingContext);
begin
  //neither dataSources nor actions: array of TDataProcessing; ?? todo
  wait := other.wait;
  userAgent := other.userAgent;
  proxy := other.proxy;
  printReceivedHeaders:=other.printReceivedHeaders;
  errorHandling:=errorHandling;

  quiet := other.quiet;

  compatibilityNoExtendedStrings := other.compatibilityNoExtendedStrings;
  compatibilityNoJSON := other.compatibilityNoJSON;
  compatibilityNoJSONliterals := other.compatibilityNoJSONliterals;
  compatibilityDotNotation := other.compatibilityDotNotation;
  compatibilityOnlyJSONObjects := other.compatibilityOnlyJSONObjects;
  compatibilityNoExtendedJson := other.compatibilityNoExtendedJson;
  compatibilityStrictTypeChecking := other.compatibilityStrictTypeChecking;
  compatibilityStrictNamespaces := other.compatibilityStrictNamespaces;
  ignoreNamespace:=other.ignoreNamespace;
  noOptimizations:=other.noOptimizations;
end;

procedure TProcessingContext.assignActions(other: TProcessingContext);
var i: integer;
begin
  setlength(actions, length(other.actions));
  for i := 0 to high(actions) do
    actions[i] := other.actions[i].clone(self);
end;

function TProcessingContext.clone(newparent: TProcessingContext): TDataProcessing;
var
  i: Integer;
begin
  result := TProcessingContext.Create;
  result.parent := newparent;
  TProcessingContext(result).assignOptions(self);
  TProcessingContext(result).assignActions(self);
  setlength(TProcessingContext(result).dataSources, length(dataSources));
  for i := 0 to high(TProcessingContext(result).dataSources) do
    TProcessingContext(result).dataSources[i] := dataSources[i].clone(TProcessingContext(result));
  if nextSibling <> nil then begin
    TProcessingContext(result).nextSibling := nextSibling.clone(TProcessingContext(result)) as TProcessingContext;
  end;

  TProcessingContext(result).follow := follow;
  TProcessingContext(result).followExclude := followExclude;
  TProcessingContext(result).followInclude := followInclude;
  TProcessingContext(result).followMaxLevel := followMaxLevel;
  if followTo <> nil then
    if followTo = self then TProcessingContext(result).followTo := TProcessingContext(result)
    else TProcessingContext(result).followTo := TProcessingContext(followTo.clone(TProcessingContext(result)));
end;

function TProcessingContext.last: TProcessingContext;
begin
  if nextSibling <> nil then exit(nextSibling.last);
  if followTo <> nil then exit(followTo.last);
  exit(self);
end;

procedure TProcessingContext.insertFictiveDatasourceIfNeeded;
var
  i: Integer;
  needDatasource: Boolean;
begin
  if Length(dataSources) > 0 then exit();
  if Length(actions) = 0 then exit();
  needDatasource := false;
  for i := 0 to high(actions) do
    if not (actions[i] is TProcessingContext) then begin
      needDatasource := true;
      break;
    end;
  if needDatasource then
    readNewDataSource(TFollowTo.createFromRetrievalAddress('<empty/>'), nil)
   else for i := 0 to high(actions) do
    if actions[i] is TProcessingContext then
      TProcessingContext(actions[i]).insertFictiveDatasourceIfNeeded;
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



function guessExtractionKind(e: string): TExtractionKind;
  function checkWords(first: string; second: array of string): boolean;
  var
    temp: PChar;
    i: Integer;
  begin
    if length(e) < length(first) + 1 then exit(false);
    if not strBeginsWith(e, first) then exit(false);
    if not (e[length(first)+1] in [#1..#32]) then exit(false);
    if length(second) = 0 then exit(true);
    temp := @e[length(first)+1];
    while temp^ in [#1..#32] do inc(temp); //skip additional whitespace
    for i:= 0 to high(second) do
      if strBeginsWith(temp, second[i]) then exit(true);
    exit(false);
  end;
var
  dots: Integer;
  i: Integer;
begin
  { try to detect the type of an extract expression:
    Template:  if it is an xml file, i.e. starts with a <
    CSS:       If it contains many # or .    i.e. if there is a [#.] before any other non letter/space character
    XQuery:    If it starts with a XQuery only command (i.e. xquery version, declare function, ...)
    XPath:     otherwise
  }


  if (e = '') or (e = '.' {just context item}) then exit(ekXPath3);
  if e[1] in [#0..#32] then e := trim(e);
  if (e[1] = '<') then exit(ekTemplate);

  if e[1] = '#' then exit(ekCSS);

  if checkWords('xquery', ['version']) or checkWords('typeswitch', []) or checkWords('import', ['module', 'schema']) or
     checkWords('module', ['namespace']) or
     checkWords('declare', ['function', 'variable', 'namespace', 'default', 'boundary-space', 'base-uri', 'option', 'construction', 'copy-namespace'])
     or checkWords('let', []) {<- that will be changed to mean xpath 3 some time} then
    exit(ekXQuery3);

  result := ekXPath3;

  dots := 0;
  for i := 1 to length(e) do
    case e[i] of
      'a'..'z','A'..'Z',#1..#32: ;
      '#': exit(ekCSS);
      '.': if ((i = 1) or (e[i-1] in ['a'..'z','A'..'Z'])) and ((i = length(e)) or (e[i+1] in ['a'..'z','A'..'Z'])) then
         dots+=1;
      else exit(ekXPath3);
    end;
  if dots > 0 then exit(ekCSS)
  else exit(ekXPath3);
end;

function TProcessingContext.process(data: IData): TFollowToList;
var next, res: TFollowToList;
  procedure subProcess(data: IData; skipActions: integer = 0);
  var
    i: Integer;
    decoded: TDecodedUrl;
    followKind: TExtractionKind;
  begin
    if follow <> '' then printStatus('**** Processing: '+data.displayBaseUri+' ****')
    else for i := skipActions to high(actions) do
      if actions[i] is TExtraction then begin
        printStatus('**** Processing: '+data.displayBaseUri+' ****');
        break; //useless printing message if no extraction is there
      end;

    //printStatus(strFromPtr(self) + data.rawdata);
    //alreadyProcessed.Add(urls[0]+#1+post);
    htmlparser.variableChangeLog.add('url', data.baseUri);
    decoded := decodeURL(data.baseUri);
    htmlparser.variableChangeLog.add('host', decoded.host + IfThen(decoded.port <> '' , ':' + decoded.port, ''));
    htmlparser.variableChangeLog.add('path', decoded.path);
    htmlparser.variableChangeLog.add('raw', data.rawData);

    if yieldDataToParent then begin
      if res = nil then res := TFollowToList.Create;
      res.add(TFollowToProcessedData.create(data));
    end;


    for i := skipActions to high(actions) do
      next.merge(actions[i].process(data), i + 1);

    if follow <> '' then begin
      if res = nil then res := TFollowToList.Create;

      htmlparser.OutputEncoding := eUTF8; //todo correct encoding?

      followKind := guessExtractionKind(follow);

      if followKind = ekTemplate then begin //assume my template
        htmlparser.parseTemplate(follow); //todo reuse existing parser
        htmlparser.parseHTML(data); //todo: optimize
        for i:=0 to htmlparser.variableChangeLog.count-1 do
          if ((length(followInclude) = 0) and (arrayIndexOf(followExclude, htmlparser.variableChangeLog.getName(i)) = -1)) or
             ((length(followInclude) > 0) and (arrayIndexOf(followInclude, htmlparser.variableChangeLog.getName(i)) > -1)) then
            res.merge(htmlparser.variableChangeLog.get(i), data, self);
      end else begin
        //assume xpath like
        xpathparser.StaticContext. BaseUri := data.baseUri;
        if followQueryCache = nil then
          case followKind of
            ekXQuery1: followQueryCache := xpathparser.parseXQuery1(follow, xpathparser.StaticContext);
            ekXQuery3: followQueryCache := xpathparser.parseXQuery3(follow, xpathparser.StaticContext);
            ekCSS: followQueryCache := xpathparser.parseCSS3(follow);
            ekXPath2: followQueryCache := xpathparser.parseXPath2(follow, xpathparser.StaticContext);
            else{ekXPath3: }followQueryCache := xpathparser.parseXPath3(follow, xpathparser.StaticContext);
          end;
        loadDataForQuery(data, followQueryCache);
        res.merge(evaluateQuery(followQueryCache, data), data, self);
      end;
      if followTo <> nil then begin
        if data.recursionLevel + 1 <= followMaxLevel then
          for i := 0 to res.Count - 1 do
            followto.process(TFollowTo(res[i]).retrieve(self, data.recursionLevel+1)).free;
        res.Clear;
      end;
    end;
  end;

var
  i: Integer;
  curRecursionLevel: Integer;
begin
  //init
  xpathparser.ParsingOptions.AllowExtendedStrings:= not compatibilityNoExtendedStrings;
  xpathparser.ParsingOptions.AllowJSON:=not compatibilityNoJSON;
  xpathparser.ParsingOptions.AllowJSONLiterals:=not compatibilityNoJSONliterals;
  xpathparser.ParsingOptions.AllowPropertyDotNotation:=compatibilityDotNotation;
  xpathparser.StaticContext.objectsRestrictedToJSONTypes:=compatibilityOnlyJSONObjects;
  xpathparser.StaticContext.jsonPXPExtensions:=not compatibilityNoExtendedJson;
  xpathparser.StaticContext.strictTypeChecking:=compatibilityStrictTypeChecking;
  xpathparser.StaticContext.useLocalNamespaces:=not compatibilityStrictNamespaces;
  htmlparser.ignoreNamespaces := ignoreNamespace;

  //apply all actions to all data source
  next := TFollowToList.Create;
  res := nil;
  if data <> nil then subProcess(data);
  for i := 0 to high(dataSources) do
    next.merge(dataSources[i].process(nil));
  if (length(actions) = 0) and (follow = '') then begin
    if res <> nil then res.free; //does this ever happen?
    exit(next);
  end;

  if (data = nil) and (length(dataSources) = 0) and (length(actions) > 0) then
    for i := 0 to high(actions) do
      if actions[i] is TProcessingContext then //evaluate subexpressions, even if there is no data source (they might have their own sources)
        next.merge(actions[i].process(nil), i + 1);

  curRecursionLevel := 0;
  if data <> nil then curRecursionLevel:=data.recursionLevel+1;
  while next.Count > 0 do begin
    if curRecursionLevel <= followMaxLevel then begin
      subProcess(next.First.retrieve(self, curRecursionLevel), next.first.nextAction);
      if wait > 0.001 then Sleep(trunc(wait * 1000));
    end;
    next.Delete(0);
  end;


  result := res;
  if nextSibling <> nil then begin
    res := nextSibling.process(nil);
    if result = nil then result := res
    else result.merge(res);
  end;

  next.free;
end;

function translateDeprecatedStrings(expr: string): string;
var
  regex: TRegExpr;
begin
  if mycmdline.readFlag('deprecated-string-options') then begin
    regex := TRegExpr.Create('([$][a-zA-Z0-9-]);');
    while regex.Exec(expr) do
      expr := regex.Replace(expr, '{$1}', true);
  end;
  result := expr;
end;

class function TProcessingContext.replaceEnclosedExpressions(expr: string): string;
begin
  result := htmlparser.replaceEnclosedExpressions(translateDeprecatedStrings(expr));
end;

type
  TXQueryEngineBreaker = class(TXQueryEngine)
    function parserEnclosedExpressionsString(s: string): IXQuery;
  end;

  function TXQueryEngineBreaker.parserEnclosedExpressionsString(s: string): IXQuery;
  begin
    result := parseXStringNullTerminated(s);
  end;

function TProcessingContext.replaceEnclosedExpressions(data: IData; expr: string): string;
var
  standard: Boolean;
  i: Integer;
  temp: IXQuery;
begin
  expr := translateDeprecatedStrings(expr);

  //see htmlparser.replaceEnclosedExpressions(expr)
  standard := true;
  for i:=1 to length(expr) do
    if expr[i] in ['{', '}' ] then begin
      standard := false;
      break;
    end;
  if standard then exit(expr);


  temp := TXQueryEngineBreaker(xpathparser).parserEnclosedExpressionsString(expr);
  loadDataForQuery(data, temp);
  result := evaluateQuery(temp, data).toString;
end;

destructor TProcessingContext.destroy;
var
  i: Integer;
begin
  for i := 0 to high(dataSources) do dataSources[i].free;
  for i := 0 to high(actions) do actions[i].free;
  nextSibling.free;
  if followTo <> self then followTo.free;
  inherited destroy;
end;

procedure TProcessingContext.loadDataForQuery(const data: IData; const query: IXQuery);
var
  f: TInputFormat;
begin
  if query.Term = nil then exit;
  f := data.inputFormat;
  if ((self = nil) or (noOptimizations)) or
     (f = ifJSON {can not detect access to json variable via get("j"||"son") } ) or
     (xqcdFocusDocument in query.Term.getContextDependencies) then begin
    if f = ifJSON then begin
      htmlparser.VariableChangelog.add('json', xpathparser.evaluateXPath2('jn:parse-json($raw)')); //todo: cache
      currentRoot := nil;
    end else begin
      htmlparser.parseHTMLSimple(data);
      currentRoot := htmlparser.HTMLTree;
    end;
  end;
end;

function TProcessingContext.evaluateQuery(const query: IXQuery; const data: IData; const allowWithoutReturnValue: boolean): IXQValue;
begin
  if query.Term = nil then exit(xqvalue());
  if allowWithoutReturnValue and ((query.Term is TXQTermModule) and (query.Term.children[high(query.Term.children)] = nil)) then
    query.Term.children[high(query.Term.children)] := TXQTermSequence.Create; //allows to process queries without return value, e.g. "declare variable $a := 1"

  if data.inputFormat <> ifJSON then result := query.evaluate(currentRoot)
  else result := query.evaluate(htmlparser.variableChangeLog.get('json'));
end;

procedure needRawWrapper;
begin
  if outputFooter <> '' then exit;
  case outputFormat of
    ofRawHTML: begin
      wln('<html><body>');
      outputFooter := '</body></html>';
    end;
    ofRawXML: begin
      wln('<xml>');
      outputFooter := '</xml>';
    end;
  end;
end;

function bashStrEscape(s: string): string;
begin
  if not strContains(s, #13) and not strContains(s, #10) then
    exit('''' + StringReplace(s, '''', '''' + '"' + '''' + '"' + ''''  {<- 5 individual characters} , [rfReplaceAll]) + '''');
  exit ('$''' + StringReplace(StringReplace(StringReplace(StringReplace(s,
                   '\', '\\', [rfReplaceAll]),
                   '''', '\''', [rfReplaceAll]),
                   #10, '\n', [rfReplaceAll]),
                   #13, '\r', [rfReplaceAll])
         +  '''');
end;

function windowsCmdEscape(s: string): string;
begin
  result := StringsReplace(s, [#10, #13, '%%', '^',  '&',  '<',  '>',  '|',  '"',  ',',  ';',  '(',  ')', '"' ],
                              ['',  '',  '%',  '^^', '^&', '^<', '^>', '^|', '^"', '^,', '^;', '^(', '^)', '^"'],
                              [rfReplaceAll]);
end;

procedure TExtraction.printExtractedValue(value: IXQValue; invariable: boolean);
  function cmdescape(s: string): string;
  begin
    case outputFormat of
      ofAdhoc, ofRawHTML, ofRawXML: exit(s);
      ofBash: exit(bashStrEscape(s));
      ofWindowsCmd: exit(windowsCmdEscape(s));
      else raise Exception.Create('Invalid output format');
    end;
  end;

  function escape(s: string): string;
  begin
    case outputFormat of
      ofAdhoc: exit(s);
      ofRawHTML: exit(htmlStrEscape(s));
      ofRawXML, ofXMLWrapped: exit(xmlStrEscape(s));
      ofBash: exit(bashStrEscape(s));
      ofWindowsCmd: exit(windowsCmdEscape(s));
      else raise Exception.Create('Invalid output format');
    end;
  end;

var
  i: Integer;
  temp: TXQValueObject;
  x: IXQValue;
begin
  case outputFormat of
    ofAdhoc, ofRawHTML, ofRawXML, ofBash, ofWindowsCmd: begin
      if (outputFormat in [ofBash, ofWindowsCmd]) and not invariable then begin
        printCmdlineVariable(defaultName, value);
        exit;
      end;
      if value is TXQValueSequence then begin
        if (outputFormat <> ofAdhoc) and ((value.getSequenceCount > 0) or printTypeAnnotations) and not invariable then needRawWrapper;
        if printTypeAnnotations then w(escape(value.typeName+': '));
        i := 0;
        for x in value do begin
          if i <> 0 then wln();
          printExtractedValue(x, true);
          i += 1;
        end;
      end else if value is TXQValueNode then begin
        if (outputFormat <> ofAdhoc) and (printTypeAnnotations or (not (value.toNode.typ in [tetOpen,tetDocument]) or (printedNodeFormat = tnsText))) and not invariable then needRawWrapper;
        if printTypeAnnotations then w(escape(value.typeName+': '));
        case printedNodeFormat of
          tnsText: w(escape(value.toString));
          tnsXML: w(cmdescape(value.toNode.outerXML()));
          tnsHTML: w(cmdescape(value.toNode.outerHTML()));
          else raise EInvalidArgument.Create('Unknown node print format');
        end;
      end
      else if (value is TXQValueObject) or (value is TXQValueJSONArray) then begin
        if (outputFormat <> ofAdhoc) and not invariable then needRawWrapper;
        if printTypeAnnotations then begin needRawWrapper; w(escape(value.typeName+': ')); end;
        w(escape(value.jsonSerialize(printedNodeFormat)));
      end
      else begin
        if (outputFormat <> ofAdhoc) and not invariable then needRawWrapper;
        if printTypeAnnotations then w(escape(value.typeName+': '));
        w(escape(value.toString));
      end;
    end;
    ofJsonWrapped: begin
      w(value.jsonSerialize(printedNodeFormat));
    end;
    ofXMLWrapped: begin
      w(value.xmlSerialize(printedNodeFormat, 'seq', 'e', 'object'));
    end;
  end;
end;

var usedCmdlineVariables: array of record
  name: string;
  count: longint;
  value: IXQValue;
end;

procedure TExtraction.printCmdlineVariable(const name: string; const value: IXQValue);
var
  i: Integer;
  v: IXQValue;
begin
  if value is TXQValueSequence then begin
    for v in value do
      printCmdlineVariable(name, v);
    exit;
  end;

  for i := 0 to high(usedCmdlineVariables) do
    if usedCmdlineVariables[i].name = name then begin
      if usedCmdlineVariables[i].count = 1 then
        case outputFormat of
          ofBash: wln(name+'[0]="$'+name+'"');
          ofWindowsCmd: printCmdlineVariable(name+'[0]', usedCmdlineVariables[i].value);
        end;
      printCmdlineVariable(name+'['+IntToStr(usedCmdlineVariables[i].count)+']', value);
      usedCmdlineVariables[i].count+=1;
      exit;
    end;

  case outputFormat of
    ofBash: w(name+'=');
    ofWindowsCmd: w('SET '+name+'=');
  end;
  printExtractedValue(value, true);
  wln;
  SetLength(usedCmdlineVariables, length(usedCmdlineVariables)+1);
  usedCmdlineVariables[high(usedCmdlineVariables)].name:=name;
  usedCmdlineVariables[high(usedCmdlineVariables)].count:=1;
  if outputFormat = ofWindowsCmd then;
    usedCmdlineVariables[high(usedCmdlineVariables)].value:=value;
end;

procedure TExtraction.printExtractedVariables(parser: THtmlTemplateParser; showDefaultVariableOverride: boolean);
var
  i: Integer;
begin
  if firstExtraction then begin
    firstExtraction := false;
    if outputFormat = ofXMLWrapped then wln('<e>');
  end else wln(outputArraySeparator[outputFormat]);

  if pvFinal in printVariables then
    printExtractedVariables(parser.variables, '** Current variable state: **', showDefaultVariableOverride or parser.hasRealVariableDefinitions);

  if pvLog in printVariables then
    printExtractedVariables(parser.variableChangeLog, '** Current variable state: **', showDefaultVariableOverride or parser.hasRealVariableDefinitions);

  if pvCondensedLog in printVariables then
    printExtractedVariables(parser.VariableChangeLogCondensed, '** Current variable state: **', showDefaultVariableOverride or parser.hasRealVariableDefinitions);

  for i := 0 to parser.variableChangeLog.count-1 do
    if parser.variableChangeLog.getName(i) = '_follow' then begin
      if currentFollowList = nil then currentFollowList := TFollowToList.Create;
      currentFollowList.merge(parser.variableChangeLog.get(i), currentData, parent);
    end;
end;

procedure TExtraction.pageProcessed(unused: TMultipageTemplateReader; parser: THtmlTemplateParser);
begin
  printExtractedVariables(parser, false);
end;


function TExtraction.process(data: IData): TFollowToList;
  function termContainsVariableDefinition(term: TXQTerm): boolean;
  var
    i: Integer;
  begin
    if term = nil then exit(false);
    if term is TXQTermDefineVariable then exit(true);
    if (term is TXQTermModule) or (term is TXQTermDefineFunction) then exit(termContainsVariableDefinition(term.children[high(term.children)])); //todo: move to xquery engine
    for i := 0 to high(term.children) do
      if termContainsVariableDefinition(term.children[i]) then exit(true);
    exit(false);
  end;



var
  value: IXQValue;
begin
  //set flags when first processed
  if isStdin(extract) then extract:=strReadFromStdin;
  if extractKind = ekAuto then extractKind := guessExtractionKind(extract);


  //parent.printStatus(strFromPtr(self) + data.rawdata + ' :: ' + extract);
  currentFollowList := nil;
  currentData:=data;

  if outputEncoding = eUnknown then htmlparser.OutputEncoding := outputEncoding
  else htmlparser.OutputEncoding := eUTF8;

  case extractKind of
    ekTemplate: begin
      htmlparser.UnnamedVariableName:=defaultName;
      htmlparser.QueryEngine.ParsingOptions.StringEntities:=xqseIgnoreLikeXPath;
      htmlparser.parseTemplate(extract); //todo reuse existing parser
      htmlparser.parseHTML(data); //todo: full url is abs?
      pageProcessed(nil,htmlparser);
    end;
    ekXPath2, ekXPath3, ekCSS, ekXQuery1, ekXQuery3: begin
      xpathparser.StaticContext.BaseUri := makeAbsoluteFilePath(data.baseUri);
      xpathparser.ParsingOptions.StringEntities:=xqseDefault;
      if extractQueryCache = nil then
        case extractKind of
          ekCSS: extractQueryCache := xpathparser.parseCSS3(extract); //todo: optimize
          ekXPath2: extractQueryCache := xpathparser.parseXPath2(extract, xpathparser.StaticContext);
          ekXQuery1: extractQueryCache := xpathparser.parseXQuery1(extract, xpathparser.StaticContext);
          ekXPath3: extractQueryCache := xpathparser.parseXPath3(extract, xpathparser.StaticContext);
          ekXQuery3: extractQueryCache := xpathparser.parseXQuery3(extract, xpathparser.StaticContext);
        end;

      parent.loadDataForQuery(data, extractQueryCache);
      if termContainsVariableDefinition(extractQueryCache.Term) then begin
        THtmlTemplateParserBreaker(htmlparser).closeVariableLog;
        parent.evaluateQuery(extractQueryCache, data, true);
        printExtractedVariables(htmlparser, true);
      end else begin
        if firstExtraction then begin
          firstExtraction := false;
          if outputFormat = ofXMLWrapped then wln('<e>');
        end else wln(outputArraySeparator[outputFormat]);

        value := parent.evaluateQuery(extractQueryCache, data, true);
        printExtractedValue(value, false);
        htmlparser.oldVariableChangeLog.add(defaultName, value);
      end;
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

function TExtraction.clone(newparent: TProcessingContext): TDataProcessing;
begin
  result := TExtraction.create;
  result.parent := newparent;
  TExtraction(result).assignOptions(self);
end;


procedure TExtraction.printExtractedVariables(vars: TXQVariableChangeLog; state: string; showDefaultVariable: boolean);
  function acceptName(n: string): boolean;
  begin
    result := ((length(extractInclude) = 0) and (arrayIndexOf(extractExclude, n) = -1)) or
              ((length(extractInclude) > 0) and (arrayIndexOf(extractInclude, n) > -1));
  end;
  function showVar(n: string): boolean;
  begin
    result := not hideVariableNames and (showDefaultVariable or (n <> defaultName) ) and (n <> 'json'); //todo: make json configurable
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
           if showVar(vars.Names[i]) then w(vars.Names[i] + ': ');
           printExtractedValue(vars.get(i), showVar(vars.Names[i]));
           wln;
         end;
    end;
    ofRawXML: begin
      if vars.count > 1 then needRawWrapper;
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if showVar(vars.Names[i]) then w('<'+vars.Names[i] + '>');
           printExtractedValue(vars.get(i), showVar(vars.Names[i]) );
           if showVar(vars.Names[i]) then wln('</'+vars.Names[i] + '>');
         end;
    end;
    ofRawHTML: begin
      if vars.count > 1 then needRawWrapper;
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if showVar(vars.Names[i]) then w('<span class="'+vars.Names[i] + '">');
           printExtractedValue(vars.get(i), showVar(vars.Names[i]) );
           if showVar(vars.Names[i]) then wln('</span>');
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
            printExtractedValue(vars.get(i), true);
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
            if values.getSequenceCount = 1 then printExtractedValue(values, true)
            else begin
              w('[');
              printExtractedValue(values.getChild(1), true);
              for j:=2 to values.getSequenceCount do begin
                w(', ');
                printExtractedValue(values.getChild(j), true);
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
            printExtractedValue(vars.get(i), true);
          end;
        end;
        if not first then w('</e>');
        wln('</seq>');
      end else begin
        wln('<object>');
        for i:=0 to vars.count-1 do
           if acceptName(vars.Names[i])  then begin
             w('<'+vars.Names[i] + '>');
             printExtractedValue(vars.Values[i], true);
             wln('</'+vars.Names[i] + '>');
           end;
        wln('</object>');
      end;
    end;
    ofBash, ofWindowsCmd:
      for i:=0 to vars.count-1 do
        printCmdlineVariable(vars.Names[i], vars.Values[i]);
  end;
end;






var i: Integer;
    temp: TStringArray;
    j: Integer;
    alternativeXMLParser: TTreeParser = nil;

{ TMultiPageTemplateBreaker }


{ THtmlTemplateParserBreaker }

procedure THtmlTemplateParserBreaker.initParsingModel(const data: IData);
var
  f: TInputFormat;
  tempparser: TTreeParser;
begin
  f := data.inputFormat;
  if f = ifJSON then exit;
  HTMLParser.repairMissingStartTags := f = ifHTML;
  if (f = ifXMLStrict) <> (HTMLParser is TTreeParserDOM) then begin
    if alternativeXMLParser = nil then begin
      alternativeXMLParser := TTreeParserDOM.Create;
      alternativeXMLParser.readComments:=true;
      alternativeXMLParser.readProcessingInstructions:=true;
      alternativeXMLParser.parsingModel:=pmStrict;
    end;
    tempparser := HTMLParser;
    FHTML := alternativeXMLParser;
    alternativeXMLParser := tempparser;
  end;
end;

procedure THtmlTemplateParserBreaker.parseHTML(const data: IData);
begin
  initParsingModel(data);
  inherited parseHTML(data.rawData, data.baseUri, data.contenttype);
end;

function strFirstNonSpace(const s: string): char;
begin
  for i:=1 to length(s) do
    if not (s[i] in WHITE_SPACE) then exit(s[i]);
  exit(#0);
end;

procedure THtmlTemplateParserBreaker.parseHTMLSimple(const data: IData);
var temp: TTreeNode;
    i: integer;
begin
  (*if (strFirstNonSpace(html) in ['{', '[']) and (
    striEndsWith(uri, 'json') or striEndsWith(uri, 'jsonp')
    or striContains(contenttype, 'application/json') or striContains(contenttype, 'application/x-javascript')
    or striContains(contenttype, 'text/javascript') or striContains(contenttype, 'text/x-javascript')
    or striContains(contenttype, 'text/x-json')) then begin
    xpathparser.evaluate();  StaticContext. xpathparser.findNativeModule('http://jsoniq.org/functions').findBasicFunction('parse-json').func^(xqvalue(html));

    exit;
  end;               *)

  initParsingModel(data);
  inherited parseHTMLSimple(data.rawData, data.baseUri, data.contenttype);
  if ignoreNamespaces then begin
    temp := FHtmlTree;
    while temp <> nil do begin
      temp.namespace:=nil;
      if temp.attributes <> nil then
        for i:=temp.attributes.count-1 downto 0 do
          if temp.attributes.Items[i].isNamespaceNode then
            temp.attributes.Delete(i);
      temp := temp.next;
    end;
  end;
end;

procedure THtmlTemplateParserBreaker.closeVariableLog;
begin
  FreeAndNil(FVariables);
  oldVariableChangeLog.takeFrom(variableChangeLog);
  FreeAndNil(FVariableLogCondensed);
end;

procedure THtmlTemplateParserBreaker.parseDoc(sender: TXQueryEngine; html, uri, contenttype: string; var node: TTreeNode);
var
  tempData: TDataObject;
  temptemp: IData;
begin
  if cgimode or (not allowFileAccess) then raise EXQEvaluationException.create('pxp:cgi', 'Using doc is not allowed in cgi mode');
  tempData := TDataObject.create(html, uri, contenttype);
  temptemp := tempData;
  parseHTMLSimple(temptemp);
  node := HTMLTree;
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
      //if cgimode then
      //  sayln(']');
    end;
    ofXMLWrapped, ofRawXML, ofRawHTML: begin
      sayln('<error>');
      sayln('<message>'+xmlStrEscape(e.Message)+'</message>');
      if printPartialMatches then begin
        sayln('<partial-matches><![CDATA[');
        temp := strSplit(htmlparser.debugMatchings(50), LineEnding); //print line by line, or the output "disappears"
        for j := 0 to high(temp) do  sayln( temp[j]);
        sayln(']]></partial-matches>');
      end;
      sayln('</error>');
      //if cgimode then
      //  sayln('</seq>');
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
TPropertyArray = array of TProperty;
TCommandLineReaderBreaker = class(TCommandLineReader)
  procedure overrideVar(const name, value: string);
  procedure removeVar(const name: string);
  procedure clearNameless;
  procedure setProperties(newProperties: TPropertyArray);
  function getProperties(): TPropertyArray;

end;

{ TCommandLineReaderBreaker }

procedure TCommandLineReaderBreaker.overrideVar(const name, value: string);
var
  tempprop: PProperty;
begin
  tempprop := findProperty(name);
  tempprop^.strvalue:=value;
  tempprop^.found:=true;
end;

procedure TCommandLineReaderBreaker.removeVar(const name: string);
var
  tempprop: PProperty;
begin
  tempprop := findProperty(name);
  tempprop^.strvalue:=tempprop^.strvalueDefault;
  tempprop^.found:=false;
end;

procedure TCommandLineReaderBreaker.clearNameless;
begin
  SetLength(nameless, 0);
end;

procedure TCommandLineReaderBreaker.setProperties(newProperties: TPropertyArray);
begin
  propertyArray := newProperties;
end;

function TCommandLineReaderBreaker.getProperties: TPropertyArray;
begin
  result := propertyArray;
end;

var currentContext: TProcessingContext;
    cmdlineWrapper: TOptionReaderFromCommandLine;
    commandLineStack: array of array of TProperty;
    commandLineStackLastPostData, commandLineLastHeader: string;
    contextStack: array of TProcessingContext;

procedure pushCommandLineState;
begin
  SetLength(commandLineStack, length(commandLineStack) + 1);
  commandLineStack[high(commandLineStack)] := TCommandLineReaderBreaker(mycmdline).getProperties;
  SetLength(commandLineStack[high(commandLineStack)], length(commandLineStack[high(commandLineStack)]));
  SetLength(contextStack, length(contextStack) + 2);
  contextStack[high(contextStack) - 1] := currentContext;
  contextStack[high(contextStack)] := nil;
end;

function popCommandLineState: TProcessingContext;
begin
  result := contextStack[high(contextStack) - 1];
  SetLength(contextStack, length(contextStack) - 2);
  TCommandLineReaderBreaker(mycmdline).setProperties(commandLineStack[high(commandLineStack)]);
  SetLength(commandLineStack, high(commandLineStack));
  if TCommandLineReaderBreaker(mycmdline).existsProperty('post') then
    commandLineStackLastPostData := TCommandLineReaderBreaker(mycmdline).readString('post');
end;

procedure variableRead(pseudoself: TObject; sender: TObject; const name, value: string);
var
  specialized: string;
begin
  if (name = 'follow') or (name = 'follow-file') or ((name = '') and (value <> '[') and (value <> ']') and (length(currentContext.actions) > 0))  then begin
    if name = 'follow-file' then
      if isStdin(value) then TCommandLineReaderBreaker(sender).overrideVar('follow', '-')
      else TCommandLineReaderBreaker(sender).overrideVar('follow', strLoadFromFileChecked(value));

    //writeln(stderr,name,'=',value);
    currentContext.readOptions(cmdlineWrapper);
    TCommandLineReaderBreaker(sender).clearNameless;

    if name <> '' then begin
      if (length(currentContext.actions) > 0) and (currentContext.actions[high(currentContext.actions)] is TProcessingContext) then
        TProcessingContext(currentContext.actions[high(currentContext.actions)]).last.yieldDataToParent := true; //a follow directly after a closing bracket like ] -f // will apply it to the last data there
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

    TCommandLineReaderBreaker(sender).removeVar('follow');
    TCommandLineReaderBreaker(sender).removeVar('extract');
    TCommandLineReaderBreaker(sender).removeVar('download');
  end else if (name = 'extract') or (name = 'extract-file') or (name = 'template-file') or (name = 'css') or (name = 'xpath') or (name = 'xquery') or (name = 'xpath2') or (name = 'xquery1') or (name = 'xpath3') or (name = 'xquery3')  then begin
    specialized := '';
    case name of
      'extract-file':
        if isStdin(value) then TCommandLineReaderBreaker(sender).overrideVar('extract', '-')
        else TCommandLineReaderBreaker(sender).overrideVar('extract', strLoadFromFileChecked(value));
      'template-file': begin
        TCommandLineReaderBreaker(sender).overrideVar('extract-kind', 'multipage');
        TCommandLineReaderBreaker(sender).overrideVar('extract', strLoadFromFileChecked(value));
      end;
      'xpath', 'xquery', 'css', 'xpath2', 'xquery1', 'xpath3', 'xquery3': specialized:=name;
    end;
    if specialized <> '' then begin
      TCommandLineReaderBreaker(sender).overrideVar('extract', value);
      TCommandLineReaderBreaker(sender).overrideVar('extract-kind', specialized);
    end;

    TCommandLineReaderBreaker(sender).overrideVar(name, value);
    currentContext.readNewAction(TExtraction.Create, cmdlineWrapper);
    if specialized <> '' then TCommandLineReaderBreaker(sender).removeVar('extract-kind');
  end else if name = 'download' then begin
    TCommandLineReaderBreaker(sender).overrideVar(name, value);
    currentContext.readNewAction(TDownload.Create, cmdlineWrapper)
  end else if (name = 'html') or (name = 'xml') then begin
    TCommandLineReaderBreaker(sender).overrideVar('input-format', name);
    TCommandLineReaderBreaker(sender).overrideVar('output-format', name);
  end else if name = 'post' then begin
    if strBeginsWith(value, '&') then
      TCommandLineReaderBreaker(sender).overrideVar('post', commandLineStackLastPostData + value);
    commandLineStackLastPostData := TCommandLineReaderBreaker(sender).readString('post');
  end else if name = 'header' then begin
    TCommandLineReaderBreaker(sender).overrideVar('header', commandLineLastHeader + value + #13#10);
    commandLineLastHeader := TCommandLineReaderBreaker(sender).readString('header');
  end else if (name = '') or (name = 'data') then begin
    if (name = '') and (value = '[') then begin
      pushCommandLineState;
      currentContext := TProcessingContext.Create;
      currentContext.readOptions(cmdlineWrapper);
      contextStack[high(contextStack)] := currentContext;
    end
    else if (name = '') and (value = ']') then begin
      if length(contextStack) <= 1 then raise Exception.Create('Closing ] without opening [');
      currentContext.readOptions(cmdlineWrapper);

      if ( (currentContext = contextStack[high(contextStack)]) and (length(currentContext.actions) = 0) and (length(currentContext.dataSources) > 0) ) //like in [ foobar xyz ]
         or ((currentContext.parent <> nil) and (currentContext.parent.follow <> '') and (length(currentContext.actions) = 0) and (length(contextStack[high(contextStack)].dataSources) > 0)) //like in [ http://example.org -e /tmp -f foobar -f //a ]
         then begin
        contextStack[high(contextStack) - 1].addNewDataSource(contextStack[high(contextStack)]);
        if (currentContext.parent <> nil) and (currentContext.parent.followTo = currentContext)  then begin
          currentContext := currentContext.parent;
          currentContext.followTo.free;
          currentContext.followTo := nil; //remove follow, so addresses are yielded to parent
        end;
       end else
        contextStack[high(contextStack) - 1].addNewAction(contextStack[high(contextStack)]);

      currentContext := popCommandLineState;

    end else currentContext.readNewDataSource(TFollowTo.createFromRetrievalAddress(value), cmdlineWrapper);
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

procedure debugPrintContext(dp: TDataProcessing; indent: string = '');
  procedure wswi(s: string);
  begin
    writeln(stderr, indent, s);
  end;

var
  pc: TProcessingContext;
  i: integer;
begin
  writeln(stderr, indent + dp.ClassName + strFromPtr(dp));
  if dp is TProcessingContext then begin
    pc := dp as TProcessingContext;;
    writeln(stderr, indent + 'data sources: ');
    for i := 0 to high(pc.dataSources) do
      debugPrintContext(pc.dataSources[i], indent + '  ');
    writeln(stderr, indent + 'actions: ');
    for i := 0 to high(pc.actions) do
      debugPrintContext(pc.actions[i], indent + '  ');
    if pc.followTo <> nil then begin
      writeln(stderr, indent + 'follow to: ');
      if pc.followTo = dp then writeln(stderr, indent + ' recursion')
      else debugPrintContext(pc.followTo, indent + '  ');
    end;
  end else if dp is TFollowToWrapper then begin
    wswi('follow to wrapper');
  end else if dp is TExtraction then begin
    wswi('extract: '+TExtraction(dp).extract);
  end else if dp is TDownload then begin
    wswi('download: '+TDownload(dp).downloadTarget);
  end;


end;

var baseContext: TProcessingContext;


procedure importModule(pseudoSelf: tobject; sender: TXQueryEngine; const namespace: string; const at: array of string);
var
  ft: TFollowTo;
  d: IData;
begin
  if xpathparser.findModule(namespace) <> nil then exit;
  for i := 0 to high(at) do begin
    ft := TFollowTo.createFromRetrievalAddress(at[i]);
    d := ft.retrieve(baseContext);
    ft.free;
    if d <> nil then begin
      xpathparser.registerModule(xpathparser.parseXQuery1(d.rawData));
      exit
    end;
  end;
end;

procedure blockFileAccessFunctions; forward;

procedure perform;
begin
  if cgimode or (not allowFileAccess) then blockFileAccessFunctions;

  //normalized formats (for use in unittests)
  DecimalSeparator:='.';
  ThousandSeparator:=#0;
  ShortDateFormat:='YYYY-MM-DD';
  LongDateFormat:='YYYY-MM-DD';
  {$ifdef win32}systemEncodingIsUTF8:=getACP = CP_UTF8;{$endif}


  mycmdline.onOptionRead:=TOptionReadEvent(procedureToMethod(TProcedure(@variableRead)));
  mycmdline.allowOverrides:=true;

  mycmdLine.declareString('data', 'Data/URL/File/Stdin(-) to process (--data= prefix can be omitted)');
  mycmdLine.declareString('download', 'Downloads/saves the data to a given filename (- prints to stdout, . uses the filename of the url)');

  mycmdLine.beginDeclarationCategory('Extraction options:');

  mycmdLine.declareString('extract', joined(['Expression to extract from the data.','If it starts with < it is interpreted as template, otherwise as XPath expression']));
  mycmdline.addAbbreviation('e');
  mycmdLine.declareString('extract-exclude', 'Comma separated list of variables ignored in an extract template. (black list) (default _follow)', '_follow');
  mycmdLine.declareString('extract-include', 'If not empty, comma separated list of variables to use in an extract template (white list)');
  mycmdLine.declareFile('extract-file', 'File containing an extract expression (for longer expressions)');
  mycmdLine.declareString('extract-kind', 'How the extract expression is evaluated. Can be auto (automatically choose between xpath/template), xpath{|2|3}, xquery{|1|3}, css, template or multipage', 'auto');
  mycmdLine.declareString('css', 'Abbreviation for --extract-kind=css --extract=...');
  mycmdLine.declareString('xpath', 'Abbreviation for --extract-kind=xpath3 --extract=...');
  mycmdLine.declareString('xquery', 'Abbreviation for --extract-kind=xquery3 --extract=...');
  mycmdLine.declareString('xpath2', 'Abbreviation for --extract-kind=xpath2 --extract=...');
  mycmdLine.declareString('xquery1', 'Abbreviation for --extract-kind=xquery1 --extract=...');
  mycmdLine.declareString('xpath3', 'Abbreviation for --extract-kind=xpath3 --extract=...');
  mycmdLine.declareString('xquery3', 'Abbreviation for --extract-kind=xquery3 --extract=...');
  mycmdLine.declareFile('template-file', 'Abbreviation for --extract-kind=multipage --extract-file=...');
  mycmdLine.declareString('template-action', 'Select which action from the multipage template should be run (multiple actions are allowed with comma separated values)');

  mycmdLine.beginDeclarationCategory('Follow options:');

  mycmdLine.declareString('follow', joined(['Expression extracting links from the page which will be followed.', 'If the expression extracts a sequence, all elements are followed.', 'If the value is an "a" node, its @href attribute is followed, if it is a "i/frame" node its @src attribute is followed, otherwise its text().', 'If it is an object, its url properties and its other properties can override command line arguments','Otherwise, the string value is treated as url.']));
  mycmdline.addAbbreviation('f');
  mycmdLine.declareString('follow-exclude', 'Comma separated list of variables ignored in a follow template. (black list)');
  mycmdLine.declareString('follow-include', 'Comma separated list of variables used in a follow template. (white list)');
  mycmdLine.declareFile('follow-file', 'File containing a follow expression (for longer expressions)');
  mycmdLine.declareInt('follow-level', 'Maximal recursion deep', 99999);
  mycmdLine.declareFlag('allow-repetitions', 'Follow all links, even if that page was already visited.');

  mycmdLine.beginDeclarationCategory('Extraction options:');


  if allowInternetAccess then begin
    mycmdLine.beginDeclarationCategory('HTTP connection options:');

    mycmdLine.declareFloat('wait', 'Wait a certain count of seconds between requests');
    mycmdLine.declareString('user-agent', 'Useragent used in http request', defaultUserAgent);
    mycmdLine.declareString('proxy', 'Proxy used for http/s requests');
    mycmdLine.declareString('post', 'Post request to send (url encoded) (prepending & concats multiple data)');
    mycmdline.addAbbreviation('d');
    mycmdLine.declareString('method', 'HTTP method to use (e.g. GET, POST, PUT)', 'GET');
    mycmdLine.declareString('header', 'Additional header to include (e.g. "Set-Cookie: a=b") (preliminary, the behaviour of multiple headers is going to change)', 'GET'); mycmdline.addAbbreviation('H');
    mycmdLine.declareFlag('print-received-headers', 'Print the received headers');
    mycmdLine.declareString('error-handling', 'How to handle http errors, e.g. 403=ignore,4xx=abort,5xx=retry (default is xxx=abort)');
  end;

  mycmdLine.beginDeclarationCategory('Output options:');

  mycmdLine.declareFlag('quiet','Do not print status information to stderr', 'q');
  mycmdLine.declareString('default-variable-name', 'Variable name for values read in the template without explicitely given variable name', 'result');
  mycmdLine.declareString('print-variables', joined(['Which of the separate variable lists are printed', 'Comma separated list of:', '  log: Prints every variable value', '  final: Prints only the final value of a variable, if there are multiple assignments to it', '  condensed-log: Like log, but removes assignments to object properties(default)']), 'condensed-log');
  mycmdLine.declareFlag('print-type-annotations','Prints all variable values with type annotations (e.g. string: abc, instead of abc)');
  mycmdLine.declareFlag('hide-variable-names','Do not print the name of variables defined in an extract template');
  mycmdLine.declareString('printed-node-format', 'Format of an extracted node: text, html or xml');
  mycmdLine.declareString('output-format', 'Output format: adhoc (simple human readable), xml, html, xml-wrapped (machine readable version of adhoc), json-wrapped, bash (export vars to bash), or cmd (export vars to cmd.exe) ', 'adhoc');
  mycmdLine.declareString('output-encoding', 'Character encoding of the output. utf-8 (default), latin1, utf-16be, utf-16le, oem (windows console) or input (no encoding conversion)', 'utf-8');
  mycmdLine.declareString('output-declaration', 'Header for the output. (e.g. <!DOCTYPE html>, default depends on output-format)', '');
  mycmdLine.declareString('input-format', 'Input format: auto, html, xml, xml-strict, json', 'auto');
  mycmdLine.declareFlag('xml','Abbreviation for --input-format=xml --output-format=xml');
  mycmdLine.declareFlag('html','Abbreviation for --input-format=html --output-format=html');
  //mycmdLine.declareString('output-header', 'Header for the output. (e.g. <!DOCTYPE html>, default depends on output-format)', '');
  //mycmdLine.declareString('output-footer', 'Footer for the output. (e.g. </xml> if you want to wrap everything in an xml node)', '');

  mycmdLine.beginDeclarationCategory('XPath/XQuery compatibility options:');

  mycmdline.declareFlag('no-json', 'Disables the JSONiq syntax extensions (like [1,2,3] and {"a": 1, "b": 2})');
  mycmdline.declareFlag('no-json-literals', 'Disables the json true/false/null literals');
  mycmdline.declareString('dot-notation', 'Specifies if the dot operator $object.property can be used. Possible values: off, on, unambiguous (default, does not allow $obj.prop, but ($obj).prop ) ', 'unambiguous');
  mycmdline.declareFlag('no-dot-notation', 'Disables the dot notation for property access, like in $object.property (deprecated)');
  mycmdline.declareFlag('only-json-objects', 'Do not allow non-JSON types in object properties (like  () or (1,2) instead of null and [1,2]) ');
  mycmdline.declareFlag('no-extended-json', 'Disables non-jsoniq json extensions');
  mycmdline.declareFlag('strict-type-checking', 'Disables weakly typing ("1" + 2 will raise an error, otherwise it evaluates to 3)');
  mycmdline.declareFlag('strict-namespaces', 'Disables the usage of undeclared namespace. Otherwise foo:bar always matches an element with prefix foo.');
  mycmdline.declareFlag('no-extended-strings', 'Does not allow x-prefixed strings like x"foo{1+2+3}bar"');
  mycmdline.declareFlag('ignore-namespaces', 'Removes all namespaces from the input document');
  mycmdline.declareFlag('no-optimizations', 'Disables optimizations');
  mycmdline.declareFlag('deprecated-string-options', 'Replaces the old $foo; variables with the new {$foo} in arguments');

  mycmdLine.declareFlag('version','Print version number ('+IntToStr(majorVersion)+'.'+IntToStr(minorVersion)+')');
  mycmdLine.declareFlag('usage','Print help, examples and usage information');

  currentContext := TProcessingContext.Create;
  baseContext := currentContext;
  SetLength(contextStack, 1);
  contextStack[0] := baseContext;

  cmdlineWrapper := TOptionReaderFromCommandLine.create(mycmdline);

  mycmdLine.parse();

  if Assigned(onPostParseCmdLine) then onPostParseCmdLine();

  if mycmdline.readFlag('version') then
    printVersion;
  if mycmdline.readFlag('usage') then begin
    printUsage;
    baseContext.free;
    exit;
  end;

  currentContext.readOptions(cmdlineWrapper);
  if (length(currentContext.actions) <> 0) and not (currentContext.actions[high(currentContext.actions)] is TProcessingContext) then
    currentContext.actions[high(currentContext.actions)].readOptions(cmdlineWrapper); //last options wrap back, unless in [ ]

  if (currentContext.parent <> nil) and (length(currentContext.actions) = 0) and (length(currentContext.dataSources) = 0) then begin
    //this allows a trailing follow to recurse
    currentContext.follow := currentContext.parent.follow;
    currentContext.followExclude := currentContext.parent.followExclude;
    currentContext.followInclude := currentContext.parent.followInclude;
    currentContext.followTo := currentContext;
    currentContext.actions := currentContext.parent.actions;
    SetLength(currentContext.actions, length(currentContext.actions));
    for i := 0 to high(currentContext.actions) do
      currentContext.actions[i] := currentContext.actions[i].clone(currentContext);
  end;

  if (currentContext.parent = nil) and (baseContext.nextSibling = currentContext) and (length(baseContext.dataSources) = 0) and (length(currentContext.actions) = 0) and (currentContext.follow = '') then begin
    //wrap command lines exactly like -e query data1 data2... to data1 data2... -e query, (for e.g. xargs)
    currentContext.actions := baseContext.actions;
    SetLength(baseContext.actions,0);
    baseContext.nextSibling := nil;
    baseContext.free;
    baseContext := currentContext;
  end;


  if (baseContext = currentContext) then begin
    for i := 0 to high(baseContext.dataSources) do
      if baseContext.dataSources[i] is TFollowToWrapper then
        baseContext.dataSources[i].readOptions(cmdlineWrapper);

    if (length(baseContext.actions) = 0) and (baseContext.follow = '') and not mycmdline.readFlag('version') then begin
      writeln(stderr, 'No actions given.');
      writeln(stderr, 'Expected at least one --extract, -e, --extract-file, --xquery, --xpath, --css, or --template-file option.');
      ExitCode:=1;
    end;
  end;


  baseContext.insertFictiveDatasourceIfNeeded; //this allows data less evaluations, like xidel -e 1+2+3

  //debugPrintContext(baseContext);

  if allowInternetAccess and assigned(onPrepareInternet) then
    onPrepareInternet(baseContext.userAgent, baseContext.proxy);

  cmdlineWrapper.Free;

  outputHeader := mycmdline.readString('output-declaration');
  outputFooter := ''; //mycmdline.readString('output-footer');
  case mycmdLine.readString('output-format') of
    'adhoc': outputFormat:=ofAdhoc;
    'html': begin
      outputFormat:=ofRawHTML;
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<!DOCTYPE html>'+LineEnding;
    end;
    'xml': begin
      outputFormat:=ofRawXML;
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<?xml version="1.0" encoding="'+ encodingName(outputEncoding)+'"?>'+LineEnding;
    end;
    'xml-wrapped': begin
      outputFormat:=ofXMLWrapped;
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<?xml version="1.0" encoding="'+ encodingName(outputEncoding)+'"?>'+LineEnding;
    end;
    'json', 'json-wrapped': begin
      outputFormat:=ofJsonWrapped;
      if (mycmdLine.readString('output-format') = 'json') then writeln(stderr, 'Warning: Output-format json is deprecated, use json-wrapped instead');
    end;
    'bash': outputFormat:=ofBash;
    'cmd':  outputFormat:=ofWindowsCmd;
    else raise EInvalidArgument.Create('Unknown output format: ' + mycmdLine.readString('output-format'));
  end;

  if assigned(onPreOutput) then onPreOutput(guessExtractionKind(mycmdline.readString('extract')));

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
  xpathparser.OnParseDoc:= @htmlparser.parseDoc;
  xpathparser.OnImportModule:=TXQImportModuleEvent(procedureToMethod(TProcedure(@importModule)));
  if xqueryDefaultCollation <> '' then xpathparser.StaticContext.collation := TXQueryEngine.getCollation(xqueryDefaultCollation, '');

  if not mycmdline.readFlag('allow-repetitions') then
    globalDuplicationList := TFollowToList.Create;
  try
    baseContext.process(nil).free;
    baseContext.Free;
  except
    on e: EXMLReadError do begin
      displayError(e);
      ExitCode:=1;
     // if not cgimode then raise;
    end;
    on e: EHTMLParseException do begin
      displayError(e, true);
      ExitCode:=1;
     // if not cgimode then raise;
    end;
    on e: EHTMLParseMatchingException do begin
      displayError(e, true);
      ExitCode:=1;
     // if not cgimode then raise;
    end;
    on e: EXQEvaluationException do begin
      ExitCode:=1;
      displayError(e);
     // if not cgimode then raise;
    end;
    on e: EXQParsingException do begin
      ExitCode:=1;
      displayError(e);
     // if not cgimode then raise;
    end;
    on e: EInternetException do begin
      ExitCode:=1;//e.errorCode;
      displayError(e);
     // if not cgimode then raise;
    end;
  end;
  if allowInternetAccess then multipage.Free
  else htmlparser.free;
  globalDuplicationList.Free;
  mycmdLine.free;
  alternativeXMLParser.Free;

  case outputFormat of
    ofJsonWrapped: wln(']');
    ofXMLWrapped: begin
      if not firstExtraction then wln('</e>');
      wln('</seq>');
    end;
    ofWindowsCmd:
      for i := 0 to high(usedCmdlineVariables) do
        if usedCmdlineVariables[i].count > 1 then
          wln('SET #'+usedCmdlineVariables[i].name +'='+ inttostr(usedCmdlineVariables[i].count));
  end;

  if outputfooter <> '' then
    wln(outputFooter);

end;


function xqfSystem(const args: TXQVArray): IXQValue;
var
  proc: TProcess;
  temps: string;
begin
  if cgimode or not allowFileAccess then exit(xqvalue('Are you trying to hack an OSS project? Shame on you!'));
  requiredArgCount(args, 1);
  proc := TProcess.Create(nil);
  proc.CommandLine := args[0].toString;
  try
    proc.Options := proc.Options + [poUsePipes, poWaitOnExit];
    proc.Execute;
    if proc.Output.NumBytesAvailable > 0 then begin
      setlength(temps, proc.Output.NumBytesAvailable);
      proc.Output.Read(temps[1], length(temps));
    end else temps := '';
    result := xqvalue(temps);
  finally
    proc.free;
  end;
end;

function xqfRead(const args: TXQVArray): IXQValue;
var s: string;
begin
  requiredArgCount(args, 0);
  ReadLn(s);
  result := TXQValueString.create(baseSchema.untypedAtomic, s);
end;

function xqFunctionJSONSafe(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
var jn: TXQNativeModule;
begin
  jn := TXQueryEngine.findNativeModule('http://jsoniq.org/functions');
  result := jn.findBasicFunction('parse-json').func(args);
end;

function xqFunctionBlocked(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  raise EXQEvaluationException.create('pxp:cgi', 'function is not allowed in cgi mode');
end;

procedure blockFileAccessFunctions;
var fn, pxp, jn: TXQNativeModule;
begin
  fn := TXQueryEngine.findNativeModule(XMLNamespaceURL_XPathFunctions);
  fn.findComplexFunction('doc').func:=@xqFunctionBlocked;
  fn.findComplexFunction('doc-available').func:=@xqFunctionBlocked;

  pxp := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensions);
  pxp.findComplexFunction('json').func:=@xqFunctionJSONSafe;

  jn := TXQueryEngine.findNativeModule('http://jsoniq.org/functions');
  jn.findComplexFunction('json-doc').func:=@xqFunctionBlocked;
end;

var pxp: TXQNativeModule;
initialization
  pxp := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensions);
  pxp.registerFunction('system', @xqfSystem, ['($arg as xs:string) as xs:string']);
  pxp.registerFunction('read', @xqfRead, ['() as xs:untypedAtomic']);
end.

