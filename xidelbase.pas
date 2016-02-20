{Copyright (C) 2012-2015  Benito van der Zander

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
  internetaccess, contnrs, dregexpr, simplexmltreeparserfpdom, LazUTF8, xquery_module_file, xquery_module_math,
  rcmdline,math
  ;

var cgimode: boolean = false;
    allowInternetAccess: boolean = true;
    allowFileAccess: boolean = true;
    xqueryDefaultCollation: string = '';
    mycmdline: TCommandLineReader;
    defaultUserAgent: string = 'Mozilla/3.0 (compatible; Xidel)';

    majorVersion: integer = 0;
    minorVersion: integer = 9;
    buildVersion: integer = 0;


type EXidelException = class(Exception);

var
    onPostParseCmdLine: procedure ();
    onPrepareInternet: function (const useragent, proxy: string): tinternetaccess;
    onRetrieve: function (const method, url, postdata, headers: string): string;
    onPreOutput: procedure (extractionKind: TExtractionKind);


procedure perform;

implementation

uses process, strutils, bigdecimalmath, xquery_json, xquery_utf8;
//{$R xidelbase.res}

type TOutputFormat = (ofAdhoc, ofJsonWrapped, ofXMLWrapped, ofRawXML, ofRawHTML, ofBash, ofWindowsCmd);
var //output options
    outputFormat: TOutputFormat;
    outputEncoding: TEncoding = eUTF8;  //default to utf-8
    outputHeader, outputFooter, outputSeparator: string;
    //outputArraySeparator: array[toutputformat] of string = ('',  ', ', '</e><e>', '', '', '', '');
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

var firstItem: boolean = true;

procedure writeItem(const s: string);
begin
  if not firstItem then begin
    w(outputSeparator);
  end;
  w(s);
  firstItem := false;
end;

procedure writeVarName(const s: string);
begin
  writeItem(s);
  firstItem := true; //prevent another line break / separator
end;

var firstGroup: boolean = true;

procedure writeBeginGroup;
begin
  case outputFormat of
    ofXMLWrapped: begin
      w('<e>');
    end;
    ofJsonWrapped: if not firstGroup then wln(', ');
  end;
  firstGroup := false;
end;

procedure writeEndGroup;
begin
  case outputFormat of
    ofXMLWrapped: wln('</e>');
  end;
end;

{procedure printBeginValueGroup;
begin

end;

procedure printBeginValue(varname: string);
begin
  case outputFormat of
    ofXMLWrapped: w('<e>');
  end;
  firstValue := false;
end;

procedure printInnerValueSeparator;
begin
  if not firstValue then begin
    w(outputSeparator);
    //w(outputArraySeparator[outputFormat]);
  end;
  firstValue := false;
end;

procedure printEndValue;
begin
end;

procedure printEndValueGroup;
begin

end;                             }


function joined(s: array of string): string; //for command line help
var
  i: Integer;
begin
  if length(s) = 0 then exit('');
  result := s[0];
  for i:=1 to high(s) do result := result + LineEnding + s[i];
end;

function strLoadFromFileChecked(const fn: string): string;
begin
  result := strLoadFromFileUTF8(fn);
  if Result = '' then raise EXidelException.Create('File '+fn+' is empty.');
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

  { TOptionReaderWrapper }

  TOptionReaderWrapper = class
    function read(const name: string; out value: string): boolean; virtual; abstract;
    function read(const name: string; out value: integer): boolean; virtual; abstract;
    function read(const name: string; out value: boolean): boolean; virtual; abstract;
    function read(const name: string; out value: Extended): boolean; virtual; abstract;
    function read(const name: string; out value: IXQValue): boolean; virtual;
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
    function read(const name: string; out value: IXQValue): boolean; override;
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
private
  variablesReplaced: boolean;
public
  url: string;
  method: string;
  data: string;
  header: string;
  multipart: string;
  rawURL: boolean;
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
  procedure addObject(absurl: string; baseurl: string; options: TXQValueObject);
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

//Processing is done in processing contexts
//A processing context can have its own data sources (TFollowTo or data sources of a nested processing context) or receive the data from its parent
//To every data source actions are applied (e.g. tdownload or textraction). These actions can also yield new data sources (e.g. follow := assignments or nested processing contexts with yieldDataToParent)
//The expression in follow is evaluated and the resulting data processed in the context followTo
//Then processing continues in nextSibling
//Remaining unprocessed data is passed to the parent
TProcessingContext = class(TDataProcessing)
  dataSources: array of TDataProcessing; //data sources, e.g. a list of URLs
  actions: array of TDataProcessing;     //actions e.g. a download target

  follow: string;
  followKind: TExtractionKind;
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

  silent, printPostData: boolean;

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

{ TOptionReaderWrapper }

function TOptionReaderWrapper.read(const name: string; out value: IXQValue): boolean;
begin
  result := false;
end;

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
const FormatMap: array[TInternetToolsFormat] of TInputFormat = ( ifXML, ifHTML, ifJSON, ifXML );
begin
  result := finputFormat;
  if result = ifAuto then
    result := FormatMap[guessFormat(rawData, baseUri, contentType)];
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

function TOptionReaderFromObject.read(const name: string; out value: IXQValue): boolean;
var
  temp: TXQValue;
begin
  result := obj.hasProperty(name, @temp);
  if result then value := temp as TXQValue;
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
    raise EXidelException.Create('Download not permitted');

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
  THTTPRequest(result).multipart:=multipart;
  THTTPRequest(result).variablesReplaced:=variablesReplaced;
  THTTPRequest(result).rawURL:=rawURL;
  result.assign(self);
end;

function THTTPRequest.retrieve(parent: TProcessingContext): IData;
var escapedURL: string;
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
      result := onRetrieve(method, escapedURL, data ,header);
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
  if not allowInternetAccess then raise EXidelException.Create('Internet access not permitted');
  if assigned(onPrepareInternet) then  internet := onPrepareInternet(parent.userAgent, parent.proxy);
  escapedURL := url;
  if not rawURL then escapedURL := urlHexEncode(url, [#32..#126]); //    fn:escape-html-uri
  parent.printStatus('**** Retrieving ('+method+'): '+escapedURL+' ****');
  if parent.printPostData and (data <> '') then parent.printStatus(data);
  result := TDataObject.create('', escapedURL);
  if assigned(onRetrieve) then begin
    (result as TDataObject).frawdata := doRetrieve(10);
    if assigned(internet) then begin
      (result as TDataObject).fbaseurl := internet.lastUrl;
      (result as TDataObject).fdisplaybaseurl := internet.lastUrl;
    end;
  end;
  if parent.printReceivedHeaders and assigned(internet) then begin
    parent.printStatus('** Headers: (status: '+inttostr(internet.lastHTTPResultCode)+')**');
    for i:=0 to internet.lastHTTPHeaders.Count-1 do
      wln(internet.lastHTTPHeaders[i]);
  end;
  if Assigned(internet) then (result as TDataObject).fcontenttype := internet.getLastHTTPHeader('Content-Type');
end;

procedure THTTPRequest.replaceVariables;
  procedure parseFormMime();
  var mime: TMIMEMultipartData;
    forms: TStringArray;
    i: Integer;
    p: SizeInt;
    name: String;
    value: String;
    paren: Char;
    nvalue: String;
    temp: String;
    filename: String;
    contenttype: String;
    kind: Char;
    t: Integer;
  begin
    if data <> '' then raise EXidelException.Create('Cannot mix urlencoded and multipart data');
    forms := strSplit(multipart, #0, false);
    for i := 0 to high(forms) do begin
      p := pos('=', forms[i]);
      name := copy(forms[i], 1, p-1);
      value := strCopyFrom(forms[i], p+1);
      filename := '';
      contenttype := '';
      kind := 'x';
      if length(value) > 0 then begin
        if value[1] in ['<','@'] then begin
          kind := value[1];
          delete(value, 1, 1);
        end else kind := 'x';
        if value[1] in ['"', ''''] then begin
          paren := value[1];
          nvalue := '';
          t := 2;
          while (t <= length(value)) do begin
            if value[t] = '\' then begin
              inc(t);
              nvalue += value[t];
            end else if value[t] = paren then break
            else nvalue += value[t];
            inc(t);
          end;
          delete(value, 1, t+1);
        end else begin
          p := pos(';', value);
          if p = 0 then p := length(value) + 1;
          nvalue := copy(value, 1, p-1);
          delete(value, 1, p);
        end;
        if kind in ['<', '@'] then begin
          if kind = '@' then filename := nvalue;
          nvalue := strLoadFromFileUTF8(nvalue);
        end;
        if value <> '' then begin
          for temp in strSplit(value, ';', false) do begin
            case strSplitGet('=', temp) of
              'filename': filename := temp;
              'type':     contenttype := temp;
              else raise EXidelException.Create('Unknown option in '+forms[i]);
            end;
          end;
        end;
      end;
      mime.addFormData(name, nvalue, filename, contenttype, '');
    end;
    data := mime.compose(header);
    header := TMIMEMultipartData.HeaderForBoundary(header);
  end;
begin
  if variablesReplaced then exit; //this method is still supposed to be only called once
  url := TProcessingContext.replaceEnclosedExpressions(url);
  method := TProcessingContext.replaceEnclosedExpressions(method);
  data := TProcessingContext.replaceEnclosedExpressions(data);
  header := TProcessingContext.replaceEnclosedExpressions(header);
  multipart := TProcessingContext.replaceEnclosedExpressions(multipart);

  if multipart <> '' then parseFormMime();
  variablesReplaced := true;
end;

function THTTPRequest.equalTo(ft: TFollowTo): boolean;
begin
  result := (ft is THTTPRequest) and (THTTPRequest(ft).url = url) and (THTTPRequest(ft).method = method) and (THTTPRequest(ft).data = data) and (THTTPRequest(ft).header = header) and (THTTPRequest(ft).multipart = multipart);
end;

function isStdin(s: string): boolean;
begin
  result := (s = '-') or (s = 'stdin:///') or (s = 'stdin:') or (s = 'stdin://');
end;

procedure closeMultiArgs(var oldValue: string; separator: string); forward;

procedure THTTPRequest.readOptions(reader: TOptionReaderWrapper);
var temp: string;
  tempxq: IXQValue;
  h: IXQValue;
begin
  inherited;
  if method <> '' then exit; //already initialized, must abort to keep stdin working (todo: allow postfix data/method options?)
  reader.read('raw-url', rawURL);
  reader.read('header', header);
  if reader is TOptionReaderFromObject then begin
    variablesReplaced := true;
    if reader.read('headers', tempxq) then begin
      for h in tempxq  do begin
        if header <> '' then header := header + #13#10;
        header += h.toString;
      end;
    end;
  end;
  method:='GET';
  if reader.read('post', data) then
    method:='POST';
  if reader.read('form', multipart) then
    method:='POST';
  if reader.read('method', temp) then begin
    method:=temp;
    if isStdin(method) then
      method := trim(strReadFromStdin);
  end;
  if reader is TOptionReaderFromCommandLine then closeMultiArgs(data, '&');
  header := trim(header);
  if isStdin(data) then
    data := strReadFromStdin;
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
  if not allowFileAccess then raise EXidelException.Create('File access not permitted');
  parent.printStatus('**** Retrieving: '+url+' ****');
  result := TDataObject.create(strLoadFromFileUTF8(url), url);
  with result as TDataObject do
    fbaseurl:=fileNameExpandToURI(fbaseurl);
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
    rtEmpty, rtXML, rtJSON: result := TDirectDataRequest.create(data);
    else raise EXidelException.Create('Impossible 232');
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
      else raise EXidelException.Create('Invalid input-format: '+ifs);
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
    tempv: TXQValue;
    n: TTreeNode;
    keys: TStringList;
    isPureDataSource: Boolean;
    i: Integer;

begin
  if dest.kind <> pvkSequence then
    dest := xpathparser.evaluateXPath2('pxp:resolve-html(., $url)', dest);
  case dest.kind of
    pvkUndefined: exit;
    pvkObject: begin
      keys := TStringList.Create;
      (dest as TXQValueObject).enumerateKeys(keys);
      isPureDataSource := true;
      for i := 0 to keys.Count - 1 do
        case keys[i] of
          'header', 'headers', 'post', 'data', 'url', 'form', 'method': ;
          else begin
            isPureDataSource := false;
            break;
          end;
        end;
      keys.free;
      if isPureDataSource then begin
        if (dest as TXQValueObject).hasProperty('url', @tempv) then
          addObject( tempv.toString, basedata.baseUri, dest as TXQValueObject)
        else if (dest as TXQValueObject).hasProperty('data', @tempv) then
          addObject(tempv.toString, basedata.baseUri, dest as TXQValueObject );
      end else if parent <> nil then begin
        temp := TProcessingContext.Create();
        temp.assignOptions(parent); //do not copy actions/data sources. they would apply to basedata, not to dest
        temp.parent := parent;
        temp.follow := parent.follow; //need to copy follow and follow-to, so it follows to the new data
        temp.followKind := parent.followKind;
        temp.followTo := parent.followTo;
        temp.nextSibling := parent.nextSibling;
        temp.mergeWithObject(dest as TXQValueObject);
        merge(temp.process(basedata));
        temp.followTo := nil;
        temp.nextSibling := nil;
        temp.Free;
      end;
    end;
    pvkSequence:
      for x in dest do
        merge(x, basedata, parent);
    pvkNode: raise EXidelException.Create('Assert failure: Expected resolved url for following, but got raw '+dest.debugAsStringWithTypeAnnotation());
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

procedure TFollowToList.addObject(absurl: string; baseurl: string; options: TXQValueObject);
var
  followTo: TFollowTo;
  reader: TOptionReaderFromObject;
begin
  if (guessType(baseurl) in [rtFile, rtRemoteURL]) and (guessType(absurl) = rtFile) then
    absurl := strResolveURI(absurl, baseurl);
  followTo := TFollowTo.createFromRetrievalAddress(absurl);
  reader := TOptionReaderFromObject.create(options);
  followTo.readOptions(reader);
  reader.free;
  add(followTo);
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

function extractKindFromString(v: string): TExtractionKind;
begin
  case v of
    'auto': result := ekAuto;
    'xpath': result :=ekXPath3;
    'xquery': result :=ekXQuery3;
    'xpath2': result :=ekXPath2;
    'xquery1': result :=ekXQuery1;
    'xpath3': result :=ekXPath3;
    'xquery3': result :=ekXQuery3;
    'css': result :=ekCSS;
    'template', 'pattern', 'html-pattern': result :=ekPatternHTML;
    'xml-pattern': result := ekPatternXML;
    'multipage': result :=ekMultipage;
    else raise EXidelException.Create('Unknown kind for the extract expression: '+v);
  end;
end;

procedure TExtraction.readOptions(reader: TOptionReaderWrapper);
var
  tempstr: string;
begin
  reader.read('extract', extract);  //todo. option: extract-file
  extract:=trim(extract);
  if reader.read('extract-exclude', tempstr) then extractExclude := strSplit(tempstr, ',', false);
  if reader.read('extract-include', tempstr) then extractInclude := strSplit(tempstr, ',', false);
  if reader.read('extract-kind', tempstr) then if extract <> '' then extractKind := extractKindFromString(tempstr);

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
  if not silent then writeln(stderr, s);
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

  reader.read('silent', silent);
  reader.read('verbose', printPostData);

  {if cmdLine.readString('follow-file') <> '' then follow := strLoadFromFileChecked(cmdLine.readString('follow-file'))
  else begin
    follow := cmdLine.readString('follow');
    if follow = '-' then follow :=strReadFromStdin;
  end;} //handled in variableRead
  reader.read('follow', follow);
  if reader.read('follow-kind', tempstr) then followKind := extractKindFromString(tempstr);
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

  silent := other.silent;
  printPostData := other.printPostData;

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

  followExclude := other.followExclude;
  followInclude := other.followInclude;
  followMaxLevel := other.followMaxLevel;
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

  TProcessingContext(result).followKind := followKind;
  TProcessingContext(result).follow := follow;
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

      followKind := self.followKind;
      if followKind = ekAuto then followKind := guessExtractionKind(follow);

      if followKind in [ekPatternHTML, ekPatternXML] then begin
        if followKind = ekPatternHTML then htmlparser.TemplateParser.parsingModel := pmHTML
        else htmlparser.TemplateParser.parsingModel := pmStrict;
        htmlparser.QueryEngine.ParsingOptions.StringEntities:=xqseIgnoreLikeXPath;
        htmlparser.parseTemplate(follow); //todo reuse existing parser
        htmlparser.parseHTML(data); //todo: optimize
        for i:=0 to htmlparser.variableChangeLog.count-1 do
          if ((length(followInclude) = 0) and (arrayIndexOf(followExclude, htmlparser.variableChangeLog.getName(i)) = -1)) or
             ((length(followInclude) > 0) and (arrayIndexOf(followInclude, htmlparser.variableChangeLog.getName(i)) > -1)) then
            res.merge(htmlparser.variableChangeLog.get(i), data, self);
      end else begin
        //assume xpath like
        xpathparser.StaticContext. BaseUri := data.baseUri;
        xpathparser.ParsingOptions.StringEntities:=xqseDefault;
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
    //nothing is to be done
    if res <> nil then res.free; //does this ever happen?
    result := next; //yield data to caller (??)
    next := nil;
  end else begin
    //normal processing
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
  end;

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
  if allowWithoutReturnValue and ((query.Term is TXQTermModule) and (TXQTermModule(query.Term).children[high(TXQTermModule(query.Term).children)] = nil)) then
    TXQTermModule(query.Term).children[high(TXQTermModule(query.Term).children)] := TXQTermSequence.Create; //allows to process queries without return value, e.g. "declare variable $a := 1"

  if data.inputFormat <> ifJSON then result := query.evaluate(currentRoot)
  else result := query.evaluate(htmlparser.variableChangeLog.get('json'));
end;

var hasRawWrapper: boolean = false;
procedure needRawWrapper;
  procedure setHeaderFooter(const h, f: string);
  begin
    w(h);
    if outputSeparator = LineEnding then wln();
    if not mycmdline.existsProperty('output-footer') then outputFooter := f + LineEnding;
  end;

var
  le: string;
begin
  if hasRawWrapper then exit;
  hasRawWrapper := true;
  if not mycmdline.existsProperty('output-header') then begin
    if not mycmdline.existsProperty('output-separator') then le := LineEnding
    else le := '';
    case outputFormat of
      ofRawHTML: setHeaderFooter('<html><body>', le +  '</body></html>');
      ofRawXML: setHeaderFooter('<xml>', le + '</xml>');
      ofJsonWrapped: setHeaderFooter('[', le + ']');
      ofXMLWrapped: setHeaderFooter('<seq>', '</seq>');
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
  result := StringsReplace(s, [#10, #13, '%',   '^',  '&',  '<',  '>',  '|',  '"',  ',',  ';',  '(',  ')', '"' ],
                              ['',  '',  '%%',  '^^', '^&', '^<', '^>', '^|', '^"', '^,', '^;', '^(', '^)', '^"'],
                              [rfReplaceAll]);
end;

procedure TExtraction.printExtractedValue(value: IXQValue; invariable: boolean);
  function cmdescape(s: string): string;
  begin
    case outputFormat of
      ofAdhoc, ofRawHTML, ofRawXML: exit(s);
      ofBash: exit(bashStrEscape(s));
      ofWindowsCmd: exit(windowsCmdEscape(s));
      else raise EXidelException.Create('Invalid output format');
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
      else raise EXidelException.Create('Invalid output format');
    end;
  end;

  function singletonToString(const v: IXQValue): string;
    function escapeToXQueryString(const s: string): string;
    begin
      result := StringReplace(s, '&', '&amp;', [rfReplaceAll]);
      if pos('"', result) = 0 then exit('"' + result + '"');
      if pos('''', result) = 0 then exit('''' + result + '''');
      result := StringReplace(s, '"', '""', [rfReplaceAll]);
      result := '"' + result + '"';
    end;

    function qualifiedTypeName(const arg: string; const ignoreDefault: string = ''): string;
    begin
      if v.typeAnnotation.schema.url <> XMLNamespaceURL_XMLSchema then
         exit(escape('Q{'+v.typeAnnotation.schema.url + '}' + v.typeName+'('+arg+')'));
      if v.typeName = ignoreDefault then exit(escape(arg));
      exit(escape('xs:'+v.typeName+'('+escapeToXQueryString(v.toString)+')'));
    end;

  begin
    case v.kind of
      pvkNode: begin
        if (outputFormat <> ofAdhoc) and (printTypeAnnotations or (not (v.toNode.typ in [tetOpen,tetDocument]) or (printedNodeFormat = tnsText))) and not invariable then needRawWrapper;
        case printedNodeFormat of
          tnsText: result := escape(v.toString);
          tnsXML: result := cmdescape(v.toNode.outerXML());
          tnsHTML: result := cmdescape(v.toNode.outerHTML());
          else raise EInvalidArgument.Create('Unknown node print format');
        end;
        if printTypeAnnotations then
          if (printedNodeFormat = tnsText) or (v.toNode.typ = tetText) then
            result := 'text{' + escapeToXQueryString(result) + '}';
      end;
      pvkObject, pvkArray: begin
        if (outputFormat <> ofAdhoc) and not invariable then needRawWrapper;
        result := escape(v.jsonSerialize(printedNodeFormat));
      end;
      else if not printTypeAnnotations then begin
        if (outputFormat <> ofAdhoc) and not invariable then needRawWrapper;
        exit(escape(v.toString));
      end else case v.kind of
        pvkBoolean:    result := qualifiedTypeName(IfThen(v.toBoolean, 'true()', 'false()'), 'boolean');
        pvkNull:       result := 'null';
        pvkInt64:      result := qualifiedTypeName(v.toString, 'integer');
        pvkFloat:      result := qualifiedTypeName(v.toString, '');
        pvkBigDecimal: result := qualifiedTypeName(v.toString, IfThen(IsIntegral(v.toDecimal), 'integer', 'decimal') );
        pvkString:     result := qualifiedTypeName(escapeToXQueryString(v.toString), 'string' );
        pvkQName, pvkDateTime:
          result := qualifiedTypeName(escapeToXQueryString(v.toString), '' );
        pvkFunction: raise EXidelException.Create('Cannot serialize function');
      end;
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
      case value.getSequenceCount of
        0: begin
          if not printTypeAnnotations then begin
            if invariable and (outputFormat in [ofBash, ofWindowsCmd]) then writeItem('');
            exit;
          end;
          if (outputFormat <> ofAdhoc) and not invariable then needRawWrapper;
          writeItem(escape('()'));
        end;
        1: begin
          //if (outputFormat <> ofAdhoc) and not invariable then needRawWrapper;
          writeItem(singletonToString(value.get(1)))
        end;
        else begin
          if (outputFormat <> ofAdhoc) and not invariable then needRawWrapper;
          if not printTypeAnnotations then begin
            for x in value do writeItem(singletonToString(x));
          end else begin
            writeItem(escape('(') + singletonToString(value.get(1)) + escape(', '));
            for i := 2 to value.getSequenceCount - 1 do
              writeItem(singletonToString(value.get(i)) + escape(', '));
            writeItem(singletonToString(value.get(value.getSequenceCount)) + escape(')'));
          end;
        end;
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
          ofBash: writeItem(name+'[0]="$'+name+'"');
          ofWindowsCmd: printCmdlineVariable(name+'[0]', usedCmdlineVariables[i].value);
        end;
      printCmdlineVariable(name+'['+IntToStr(usedCmdlineVariables[i].count)+']', value);
      usedCmdlineVariables[i].count+=1;
      exit;
    end;

  case outputFormat of
    ofBash: writeVarName(name+'=');
    ofWindowsCmd: writeVarName('SET '+name+'=');
  end;
  printExtractedValue(value, true);
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
    if term is TXQTermWithChildren then
      with TXQTermWithChildren(term) do begin
        if term is TXQTermDefineVariable then exit(true);
        if (term is TXQTermModule) or (term is TXQTermDefineFunction) then
           exit(termContainsVariableDefinition(children[high(children)])); //todo: move to xquery engine
        for i := 0 to high(children) do
          if termContainsVariableDefinition(children[i]) then exit(true);
      end;
    exit(false);
  end;



var
  value: IXQValue;
begin
  //set flags when first processed
  if isStdin(extract) then extract:=strReadFromStdin;
  if extractKind = ekAuto then begin
    if extract = '' then extract := '()';
    extractKind := guessExtractionKind(extract);
  end;

  //parent.printStatus(strFromPtr(self) + data.rawdata + ' :: ' + extract);
  currentFollowList := nil;
  currentData:=data;

  if outputEncoding = eUnknown then htmlparser.OutputEncoding := outputEncoding
  else htmlparser.OutputEncoding := eUTF8;

  case extractKind of
    ekPatternHTML, ekPatternXML: begin
      htmlparser.UnnamedVariableName:=defaultName;
      htmlparser.QueryEngine.ParsingOptions.StringEntities:=xqseIgnoreLikeXPath;
      if extractKind = ekPatternHTML then htmlparser.TemplateParser.parsingModel := pmHTML
      else htmlparser.TemplateParser.parsingModel := pmStrict;
      htmlparser.parseTemplate(extract); //todo reuse existing parser
      htmlparser.parseHTML(data); //todo: full url is abs?
      pageProcessed(nil,htmlparser);
    end;
    ekXPath2, ekXPath3, ekCSS, ekXQuery1, ekXQuery3: begin
      xpathparser.StaticContext.BaseUri := fileNameExpandToURI(data.baseUri);
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
        value := parent.evaluateQuery(extractQueryCache, data, true);
        writeBeginGroup;
        printExtractedValue(value, false);
        writeEndGroup;
        htmlparser.oldVariableChangeLog.add(defaultName, value);
      end;
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
    else raise EXidelException.Create('Impossible');
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
  function showVar(const n: string): boolean;
  begin
    result := not hideVariableNames and (showDefaultVariable or (n <> defaultName) ) and (n <> 'json') //todo: make json configurable
  end;

var
  i: Integer;
  tempUsed: array of boolean;
  first: boolean;
  values: IXQValue;
  j: Integer;
begin
  writeBeginGroup;
  parent.printStatus(state);
  case outputFormat of
    ofAdhoc: begin
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if showVar(vars.Names[i]) then writeVarName(vars.Names[i] + ' := ');
           printExtractedValue(vars.get(i), showVar(vars.Names[i]));
         end;
    end;
    ofRawXML: begin
      if vars.count > 1 then needRawWrapper;
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if showVar(vars.Names[i]) then writeVarName('<'+vars.Names[i] + '>');
           printExtractedValue(vars.get(i), showVar(vars.Names[i]) );
           if showVar(vars.Names[i]) then w('</'+vars.Names[i] + '>');
         end;
    end;
    ofRawHTML: begin
      if vars.count > 1 then needRawWrapper;
      for i:=0 to vars.count-1 do
         if acceptName(vars.Names[i])  then begin
           if showVar(vars.Names[i]) then writeVarName('<span class="'+vars.Names[i] + '">');
           printExtractedValue(vars.get(i), showVar(vars.Names[i]) );
           if showVar(vars.Names[i]) then w('</span>');
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
        writeItem('{');
        setlength(tempUsed, vars.count);
        FillChar(tempUsed[0], sizeof(tempUsed[0])*length(tempUsed), 0);
        for i:=0 to vars.count-1 do begin
          if tempUsed[i] then continue;
          if acceptName(vars.Names[i]) then begin
            if not first then wln(', ');
            first := false;
            writeVarName(jsonStrEscape(vars.Names[i]) + ': ');
            values := vars.getAll(vars.Names[i]);
            if values.getSequenceCount = 1 then printExtractedValue(values, true)
            else begin
              w('[');
              printExtractedValue(values.get(1), true);
              for j:=2 to values.getSequenceCount do begin
                w(', ');
                printExtractedValue(values.get(j), true);
              end;
              w(']');
            end;
          end;
          for j := i + 1 to vars.count-1 do
            if vars.Names[i] = vars.Names[j] then tempUsed[j] := true;
        end;
        w(LineEnding + '}');
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
        w(LineEnding + '<object>' + LineEnding);
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
        if acceptName(vars.Names[i]) then
          printCmdlineVariable(vars.Names[i], vars.Values[i]);
  end;
  writeEndGroup;
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
  if length(template.baseActions.children) = 0 then raise EXidelException.Create('Template contains no actions!'+LineEnding+'A Multipage template should look like <action>  <page url="..."> <post> post data </post> <template> single page template </template> </page> </action> ');
  if length(actions) = 0 then callAction(template.baseActions.children[0])
  else for i:= 0 to high(actions) do
    callAction(actions[i]);
end;

procedure TTemplateReaderBreaker.selfLog(sender: TMultipageTemplateReader; logged: string; debugLevel: integer);
begin
  if debugLevel <> 0 then exit;
  writeln(stderr, logged);
end;



procedure traceCall(pseudoSelf: tobject; sender: TXQueryEngine; value, info: IXQValue);
begin
  if not info.isUndefined then write(stderr, info.toJoinedString() + ': ');
  writeln(stderr, value.debugAsStringWithTypeAnnotation());
end;

type TXQTracer = class
  log: array of record
    t: TXQTerm;
    args: TXQVArray;
  end;
  lastContext: TXQEvaluationContext;
  varLog: TXQVariableChangeLog;
  logLength: integer;
  all, backtrace, context, contextVariables: boolean;
  procedure globalTracing(term: TXQTerm; const acontext: TXQEvaluationContext; entering: boolean; const args: TXQVArray);
  procedure printStderr(term: TXQTerm; const args: TXQVArray);
  procedure printBacktrace;
  procedure printLastContext;
  destructor Destroy; override;
end;

var tracer: TXQTracer;

procedure TXQTracer.globalTracing(term: TXQTerm; const acontext: TXQEvaluationContext; entering: boolean; const args: TXQVArray);
begin
  if backtrace then begin;
    if entering then begin
      if logLength > high(log) then SetLength(log, max(logLength + 8, length(log) * 2));
      log[logLength].t := term;
      log[logLength].args := args;
      inc(logLength);
    end else begin
      while (logLength > 0) and (log[logLength - 1].t <> term) do dec(logLength);
      if (logLength > 0) and (log[logLength - 1].t = term) then dec(logLength);
    end;
  end;
  if entering then begin
    if all and entering then printStderr(term, args);
    if self.context and entering then begin
      lastContext := acontext;
      if contextVariables then
        if acontext.temporaryVariables = nil then varlog.clear
        else varLog.assign(acontext.temporaryVariables);
      if all then printLastContext;
    end;
  end;
end;

procedure TXQTracer.printStderr(term: TXQTerm; const args: TXQVArray);
var i: integer;
begin
  if term is TXQTermNamedFunction then begin
    if TXQTermNamedFunction(term).name <> nil then write(stderr, TXQTermNamedFunction(term).name.ToString)
    else write(stderr, 'unknown function');
  end else if term is TXQTermBinaryOp then begin
    if TXQTermBinaryOp(term).op <> nil then write(stderr, 'operator ', TXQTermBinaryOp(term).op.name)
    else write(stderr, 'unknown operator');
  end else if term is TXQTermDynamicFunctionCall then write('anonymous function')
  else if term is TXQTermTryCatch then write('try/catch')
  else write('unknown event');
  write(stderr, '(');
  for i := 0 to high(args) do
    if i = 0 then write(args[i].debugAsStringWithTypeAnnotation())
    else write(', ', args[i].debugAsStringWithTypeAnnotation());
  writeln(stderr, ')');
end;

procedure TXQTracer.printBacktrace;
var i: integer;
begin
  for i := 0 to logLength - 1 do
    printStderr(log[i].t, log[i].args);
end;

procedure TXQTracer.printLastContext;
var
  vars: TXQVariableChangeLog;
begin
  writeln(stderr, 'Dynamic context: ');
  if lastContext.RootElement <> nil then writeln(stderr, '  root node: ', lastContext.ParentElement.toString());
  if lastContext.ParentElement <> nil then writeln(stderr, '  parent node: ', lastContext.ParentElement.toString());
  if lastContext.SeqValue <> nil then writeln(stderr, '  context item (.): ', lastContext.SeqValue.debugAsStringWithTypeAnnotation());
  writeln(stderr, '  position()/last(): ', lastContext.SeqIndex, ' / ', lastContext.SeqLength);
  vars := lastContext.temporaryVariables;
  if contextVariables then vars := varLog;
  if (vars <> nil) and (vars.count > 0) then begin
    writeln(stderr, '  Local variables: ');
    for i := 0 to vars.count - 1 do
      writeln(stderr, '    ', vars.getName(i), ' = ', vars.get(i).debugAsStringWithTypeAnnotation());
  end;
  WriteLn(stderr, '');
end;

destructor TXQTracer.Destroy;
begin
  varLog.Free;
  inherited Destroy;
end;

type TMyConsoleColors = (ccNormal, ccRedBold);
var lastConsoleColor: TMyConsoleColors = ccNormal;

procedure displayError(e: Exception; printPartialMatches: boolean = false);
  procedure say(s: string; color: TMyConsoleColors = ccNormal);
  begin
    if cgimode then write(s)
    else begin
      if color <> lastConsoleColor then begin
        Flush(stderr);
        {$ifdef unix}
        case color of
          ccNormal: write(stderr, #27'[0m');
          ccRedBold: write(stderr, #27'[1;31m');
        end;
        {$endif}
        {$ifdef windows}
        case color of
          ccNormal: SetConsoleTextAttribute(StdErrorHandle, FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE);
          ccRedBold: SetConsoleTextAttribute(StdErrorHandle, FOREGROUND_RED or FOREGROUND_INTENSITY);
        end;
        {$endif}
        lastConsoleColor := color;
      end;
      write(stderr, s);
    end;
  end;
  procedure sayln(s: string; color: TMyConsoleColors = ccNormal);
  begin
    say(s+LineEnding, color);
  end;

const ParsingError  = '[<- error occurs before here]';
var
  message: String;
  p: LongInt;
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
    ofXMLWrapped{, ofRawXML, ofRawHTML}: begin
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
    else begin
      sayln( 'Error:');
      message := e.Message;
      if not cgimode then begin
        if  strBeginsWith(message, 'err:') or strBeginsWith(message, 'pxp:') then begin
          p := strIndexOf(message, [#13,#10]);
          say(copy(message,1,p-1), ccRedBold);
          delete(message, 1, p);
        end;
        while Length(message) > 0 do begin
          p := strIndexOf(message, ParsingError);
          if p <= 0 then break;
          say(copy(message, 1, p - 1));
          say(ParsingError, ccRedBold);
          delete(message, 1, p + length(ParsingError) - 1);
        end;
      end;
      sayln( message );
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
  if e is EXQEvaluationException then begin
    if (tracer = nil) or not tracer.backtrace then begin
      sayln('Possible backtrace:');
      for i := 0 to ExceptFrameCount-1 do begin
        sayln(BackTraceStrFunc(ExceptFrames[i]) + ': ' + EXQException.searchClosestFunction(ExceptFrames[i]));
      end;
      sayln(LineEnding + 'Call xidel with --trace-stack to get an actual backtrace')
    end;
    if (tracer <> nil) then begin
      if tracer.backtrace then tracer.printBacktrace;
      if tracer.context then tracer.printLastContext;
    end;
  end;
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

  procedure parseUTF8;

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

procedure TCommandLineReaderBreaker.parseUTF8;
{$ifndef windows}
var args: TStringArray;
  i: Integer;
{$endif}
begin
  if Paramcount = 0 then exit;

  {$ifdef windows}
  parse(SysToUTF8(string(GetCommandLine)), true);
  {$else}
  setlength(args, Paramcount);
  for i:=0 to high(args) do args[i] := SysToUTF8(paramstr(i+1));
  parse(args);
  {$endif}
end;

var currentContext: TProcessingContext;
    cmdlineWrapper: TOptionReaderFromCommandLine;
    commandLineStack: array of array of TProperty;
    commandLineStackLastPostData, commandLineStackLastFormData, commandLineLastHeader: string;
    contextStack: array of TProcessingContext;

procedure pushCommandLineState;
begin
  if TCommandLineReaderBreaker(mycmdline).existsProperty('post') then
    TCommandLineReaderBreaker(mycmdline).overrideVar('post', commandLineStackLastPostData);
  if TCommandLineReaderBreaker(mycmdline).existsProperty('form') then
    TCommandLineReaderBreaker(mycmdline).overrideVar('form', commandLineStackLastFormData);
  if TCommandLineReaderBreaker(mycmdline).existsProperty('header') then
    TCommandLineReaderBreaker(mycmdline).overrideVar('header', commandLineLastHeader);
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
  if TCommandLineReaderBreaker(mycmdline).existsProperty('form') then
    commandLineStackLastFormData := TCommandLineReaderBreaker(mycmdline).readString('form');
  if TCommandLineReaderBreaker(mycmdline).existsProperty('header') then
    commandLineLastHeader := TCommandLineReaderBreaker(mycmdline).readString('header');
end;

procedure variableInterpret(pseudoself, sender: TObject; var name, value: string; const args: TStringArray; var argpos: integer);
begin
  if strBeginsWith(name, 'xmlns:') then begin
    value := strCopyFrom(name, length('xmlns:') + 1) + '=' + value;
    name := 'xmlns';
  end;
end;

procedure closeMultiArgs(var oldValue: string; separator: string);
begin
  if strEndsWith(oldValue, separator) then
    delete(oldValue, length(oldValue) - length(separator) + 1, length(separator));
end;

function combineMultiArgs(var oldValue: string; appendValue, separator: string): string;
begin
  if (appendValue = '') then oldValue := ''
  else begin
    if appendValue[1] = '&' then delete(appendValue, 1, 1)
    else if not strEndsWith(oldValue, separator) then oldValue := '';

    oldValue := oldValue + appendValue + separator;
  end;
  Result := oldValue;
end;

procedure variableRead(pseudoself: TObject; sender: TObject; const name, value: string);
  procedure closeAllMultiArgs;
  begin
    closeMultiArgs(commandLineStackLastPostData, '&');
    closeMultiArgs(commandLineStackLastFormData, #0);
    closeMultiArgs(commandLineLastHeader, #13#10);
  end;
  procedure parseVariableArg;
  var
    temps: String;
    equalSign: SizeInt;
    vars: bbutils.TStringArray;
  begin
    equalSign := pos('=', value);
    if equalSign = 0 then temps := value
    else begin
      temps := copy(value, 1, equalSign - 1);
      if strEndsWith(temps, ':') then delete(temps, length(temps), 1);
    end;
    vars := strSplit(temps, ',');
    if (length(vars) <> 1) and (equalSign > 0) then raise EXidelException.Create('Cannot import multiple variables and specify a variable value at once. In '+value);
    for temps in vars do begin
      temps := trim(temps);
      if strBeginsWith(temps, '$') then delete(temps, 1, 1);
      if equalSign = 0 then htmlparser.variableChangeLog.add(temps, GetEnvironmentVariable(temps))
      else htmlparser.variableChangeLog.add(trim(temps), strCopyFrom(value, equalSign+1));
    end;
  end;

var
  specialized: string;
  temps: String;
begin
  if (name = 'follow') or (name = 'follow-file')             //follow always create new processing context
     or ((name = '') and (value <> '[') and (value <> ']')   //plain data/url
          and (((currentContext.parent <> nil) and (currentContext.parent.followTo = currentContext)) or (length(currentContext.actions) > 0))) //only creates new context, if previous context is not a follow context and had no action yet (otherwise it becomes new data source in the previous context)
  then begin

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
    closeAllMultiArgs;
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
  end else if name = 'post' then
    TCommandLineReaderBreaker(sender).overrideVar('post', combineMultiArgs(commandLineStackLastPostData, value, '&'))
  else if name = 'form' then
    TCommandLineReaderBreaker(sender).overrideVar('form', combineMultiArgs(commandLineStackLastFormData, value, #0))
  else if name = 'header' then
    TCommandLineReaderBreaker(sender).overrideVar('header', combineMultiArgs(commandLineLastHeader, value, #13#10))
  else if name = 'variable' then parseVariableArg
  else if name = 'xmlns' then begin
    temps:=trim(value);
    i := pos('=', temps);
    if i = 0 then xpathparser.StaticContext.defaultElementTypeNamespace := TNamespace.create(temps, '')
    else begin
      specialized := strSplitGet('=', temps);
      xpathparser.StaticContext.namespaces.add(TNamespace.create(temps, specialized));
    end;
  end else if (name = '') or (name = 'data') then begin
    if (name = '') and (value = '[') then begin
      pushCommandLineState;
      currentContext := TProcessingContext.Create;
      currentContext.readOptions(cmdlineWrapper);
      contextStack[high(contextStack)] := currentContext;
    end
    else if (name = '') and (value = ']') then begin
      if length(contextStack) <= 1 then raise EXidelException.Create('Closing ] without opening [');
      currentContext.readOptions(cmdlineWrapper);

      if ( (currentContext = contextStack[high(contextStack)]) and (length(currentContext.actions) = 0) and (length(currentContext.dataSources) > 0) ) //like in [ foobar xyz ]
         or ((currentContext.parent <> nil) and (currentContext.parent.follow <> '')
              and (length(currentContext.actions) = 0) and (length(contextStack[high(contextStack)].dataSources) > 0)) //like in [ http://example.org -e /tmp -f foobar -f //a ]
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

    end else begin
      closeAllMultiArgs;
      currentContext.readNewDataSource(TFollowTo.createFromRetrievalAddress(value), cmdlineWrapper);
    end;
  end;
end;

function getVersionString: string;
begin
  result := IntToStr(majorVersion)+'.'+IntToStr(minorVersion);
  if buildVersion <> 0 then result += '.'+IntToStr(buildVersion);
  if Result = '0.8.4' then result += ' (Balisage edition)'
end;

procedure printVersion;
begin
  writeln('Xidel '+getVersionString);
  writeln('');
  writeln('http://www.videlibri.de/xidel.html');
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
    if pc.follow <> '' then wswi('follow: ' + pc.follow);
    if pc.yieldDataToParent then wswi('yield to parent');
    if pc.followTo <> nil then begin
      writeln(stderr, indent + 'follow to: ');
      if pc.followTo = dp then writeln(stderr, indent + ' recursion')
      else debugPrintContext(pc.followTo, indent + '  ');
    end;
    if pc.nextSibling <> nil then begin
      writeln(stderr, indent + 'Next Sibling');
      debugPrintContext(pc.nextSibling, indent);
    end;
  end else if dp is TFollowToWrapper then begin
    wswi(     'follow to wrapper: ' + TFollowToWrapper(dp).followTo.ClassName);
    indent += '                   ';
    if TFollowToWrapper(dp).followTo is THTTPRequest then wswi(THTTPRequest(TFollowToWrapper(dp).followTo).url)
    else if TFollowToWrapper(dp).followTo is TFileRequest then wswi(TFileRequest(TFollowToWrapper(dp).followTo).url)
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
  registerModuleMath;
  {$ifdef win32}systemEncodingIsUTF8:=getACP = CP_UTF8;{$endif}

  htmlparser:=THtmlTemplateParserBreaker.create;

  mycmdline.onCustomOptionInterpretation := TOptionInterpretationEvent(procedureToMethod(TProcedure(@variableInterpret)));
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

  mycmdLine.declareString('follow', joined(['Expression selecting data from the page which will be followed.',
                                            'If the expression returns a sequence, all its elements are followed.',
                                            'If it is an HTML element with an resource property this property is used, e.g. from an A element it will follow to its @href property.',
                                            'If it is an object, its url property and its other properties can override command line arguments',
                                            'Otherwise, the string value is used as url.']));
  mycmdline.addAbbreviation('f');
  mycmdline.declareString('follow-kind', 'How the follow expression is evaluated. Like extract-kind');
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
    mycmdLine.declareString('post', joined(['Post request to send (url encoded). Multiple close occurrences are joined. If the new argument starts with &, it will always be joined. If it is empty, it will clear the previous parameters. ']));
    mycmdline.addAbbreviation('d');
    mycmdLine.declareString('form', 'Post request to send (multipart encoded). See --usage. Can be used multiple times like --post.');
    mycmdline.addAbbreviation('F');
    mycmdLine.declareString('method', 'HTTP method to use (e.g. GET, POST, PUT)', 'GET');
    mycmdLine.declareString('header', 'Additional header to include (e.g. "Set-Cookie: a=b"). Can be used multiple times like --post.'); mycmdline.addAbbreviation('H');
    mycmdLine.declareFlag('print-received-headers', 'Print the received headers');
    mycmdLine.declareString('error-handling', 'How to handle http errors, e.g. 403=ignore,4xx=abort,5xx=retry (default is xxx=abort)');
    mycmdLine.declareFlag('raw-url', 'Do not escape the url (preliminary)');
  end;

  mycmdLine.beginDeclarationCategory('Output options:');

  mycmdLine.declareFlag('silent','Do not print status information to stderr', 's');
  mycmdline.declareFlag('verbose', 'Print more status information');
  mycmdLine.declareString('default-variable-name', 'Variable name for values read in the template without explicitely given variable name', 'result');
  mycmdLine.declareString('print-variables', joined(['Which of the separate variable lists are printed', 'Comma separated list of:', '  log: Prints every variable value', '  final: Prints only the final value of a variable, if there are multiple assignments to it', '  condensed-log: Like log, but removes assignments to object properties(default)']), 'condensed-log');
  mycmdLine.declareFlag('print-type-annotations','Prints all variable values with type annotations (e.g. string: abc, instead of abc)');
  mycmdLine.declareFlag('hide-variable-names','Do not print the name of variables defined in an extract template');
  mycmdLine.declareString('variable','Declares a variable (value taken from environment if not given explicitely) (multiple variables are preliminary)');
  mycmdLine.declareString('xmlns','Declares a namespace');
  mycmdLine.declareString('printed-node-format', 'Format of an extracted node: text, html or xml');
  mycmdLine.declareString('output-format', 'Output format: adhoc (simple human readable), xml, html, xml-wrapped (machine readable version of adhoc), json-wrapped, bash (export vars to bash), or cmd (export vars to cmd.exe) ', 'adhoc');
  mycmdLine.declareString('output-encoding', 'Character encoding of the output. utf-8 (default), latin1, utf-16be, utf-16le, oem (windows console) or input (no encoding conversion)', 'utf-8');
  mycmdLine.declareString('output-declaration', 'Header for the output. (e.g. <!DOCTYPE html>, default depends on output-format)', '');
  mycmdLine.declareString('output-separator', 'Separator between multiple items (default: line break)', LineEnding);
  mycmdLine.declareString('output-header', '2nd header for the output. (e.g. <html>)', '');
  mycmdLine.declareString('output-footer', 'Footer for the output. (e.g. </html>)', '');
  mycmdLine.declareString('input-format', 'Input format: auto, html, xml, xml-strict, json', 'auto');
  mycmdLine.declareFlag('xml','Abbreviation for --input-format=xml --output-format=xml');
  mycmdLine.declareFlag('html','Abbreviation for --input-format=html --output-format=html');


  mycmdLine.beginDeclarationCategory('Debug options:');
  mycmdline.declareFlag('debug-arguments', 'Shows how the command line arguments were parsed');
  mycmdline.declareFlag('trace', 'Traces the evaluation of all queries');
  mycmdline.declareFlag('trace-stack', 'Traces the evaluation to print a backtrace in case of errors');
  mycmdline.declareFlag('trace-context', 'Like trace-stack, but for the context');
  mycmdline.declareFlag('trace-context-variables', 'Like trace-stack, but for the context variables');

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

  mycmdLine.declareFlag('version','Print version number ('+getVersionString+')');
  mycmdLine.declareFlag('usage','Print help, examples and usage information');
  mycmdLine.declareFlag('quiet','-quiet,-q is outdated. Use --silent,-s', 'q');

  currentContext := TProcessingContext.Create;
  baseContext := currentContext;
  SetLength(contextStack, 1);
  contextStack[0] := baseContext;

  xpathparser := htmlparser.QueryEngine;
  if xpathparser.StaticContext.namespaces = nil then htmlparser.QueryEngine.StaticContext.namespaces := TNamespaceList.Create;
  xpathparser.StaticContext.namespaces.add(XMLNamespace_Expath_File);
  xpathparser.OnParseDoc:= @htmlparser.parseDoc;
  xpathparser.OnImportModule:=TXQImportModuleEvent(procedureToMethod(TProcedure(@importModule)));
  xpathparser.OnTrace := TXQTraceEvent(procedureToMethod(TProcedure(@traceCall)));

  cmdlineWrapper := TOptionReaderFromCommandLine.create(mycmdline);

  TCommandLineReaderBreaker(mycmdLine).parseUTF8();

  if Assigned(onPostParseCmdLine) then onPostParseCmdLine();

  if mycmdline.readFlag('version') then
    printVersion;
  if mycmdline.readFlag('usage') then begin
    printUsage;
    baseContext.free;
    exit;
  end;
  if mycmdline.readFlag('quiet') then begin
    writeln(stderr, '-quiet,-q is outdated. Use --silent,-s');
    exit;
  end;

  currentContext.readOptions(cmdlineWrapper);
  if (length(currentContext.actions) <> 0) and not (currentContext.actions[high(currentContext.actions)] is TProcessingContext) then
    currentContext.actions[high(currentContext.actions)].readOptions(cmdlineWrapper); //last options wrap back, unless in [ ]

  if (currentContext.parent <> nil) and (length(currentContext.actions) = 0) and (length(currentContext.dataSources) = 0) then begin
    //this allows a trailing follow to recurse
    currentContext.follow := currentContext.parent.follow;
    currentContext.followKind := currentContext.parent.followKind;
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

  if mycmdline.readFlag('debug-arguments') then
    debugPrintContext(baseContext);

  if allowInternetAccess and assigned(onPrepareInternet) then
    onPrepareInternet(baseContext.userAgent, baseContext.proxy);

  cmdlineWrapper.Free;

  outputHeader := mycmdline.readString('output-declaration') + mycmdline.readString('output-header');
  if (outputHeader <> '') and not mycmdline.existsProperty('output-header') then outputHeader += LineEnding;
  outputSeparator := mycmdline.readString('output-separator');
  outputFooter := mycmdline.readString('output-footer');
  case mycmdLine.readString('output-format') of
    'adhoc': outputFormat:=ofAdhoc;
    'html': begin
      outputFormat:=ofRawHTML;
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<!DOCTYPE html>'+LineEnding+outputHeader;
    end;
    'xml': begin
      outputFormat:=ofRawXML;
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<?xml version="1.0" encoding="'+ encodingName(outputEncoding)+'"?>'+LineEnding+outputHeader;
    end;
    'xml-wrapped': begin
      outputFormat:=ofXMLWrapped;
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<?xml version="1.0" encoding="'+ encodingName(outputEncoding)+'"?>'+LineEnding+outputHeader;
    end;
    'json', 'json-wrapped': begin
      outputFormat:=ofJsonWrapped;
      if (mycmdLine.readString('output-format') = 'json') then writeln(stderr, 'Warning: Output-format json is deprecated, use json-wrapped instead');
    end;
    'bash': begin
      outputFormat:=ofBash;
    end;
    'cmd':  begin
      outputFormat:=ofWindowsCmd;
    end
    else raise EInvalidArgument.Create('Unknown output format: ' + mycmdLine.readString('output-format'));
  end;

  if mycmdline.readFlag('trace') or mycmdline.readFlag('trace-stack') or mycmdline.readFlag('trace-context') or mycmdline.readFlag('trace-context-variables') then begin
    tracer := TXQTracer.Create;
    tracer.all := mycmdline.readFlag('trace');
    tracer.backtrace := mycmdline.readFlag('trace-stack');
    tracer.context := mycmdline.readFlag('trace-context') or mycmdline.readFlag('trace-context-variables');
    tracer.contextVariables := mycmdline.readFlag('trace-context-variables');
    if tracer.contextVariables then tracer.varLog := TXQVariableChangeLog.create();
    XQOnGlobalDebugTracing := @tracer.globalTracing;
  end;

  if assigned(onPreOutput) then onPreOutput(guessExtractionKind(mycmdline.readString('extract')));

  if outputHeader <> '' then w(outputHeader);
  if outputFormat in [ofJsonWrapped, ofXMLWrapped] then needRawWrapper;


  htmlparser.TemplateParser.repairMissingEndTags:=false;
  htmlparser.TemplateParser.repairMissingStartTags:=false;

  htmlparser.KeepPreviousVariables:=kpvKeepValues;
  if allowInternetAccess then begin
    multipage := TTemplateReaderBreaker.create();
    multipage.parser:=htmlparser;
  end;
  if xqueryDefaultCollation <> '' then xpathparser.StaticContext.collation := TXQueryEngine.getCollation(xqueryDefaultCollation, '');

  if not mycmdline.readFlag('allow-repetitions') then
    globalDuplicationList := TFollowToList.Create;
  try
    baseContext.process(nil).free;
    baseContext.Free;
  except
    on e: ETreeParseException do begin
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
  alternativeXMLParser.Free;

  case outputFormat of
    {ofJsonWrapped:  wln(']');
    ofXMLWrapped: begin
      if not firstExtraction then wln('</e>');
      wln('</seq>');
    end;}
    ofWindowsCmd:
      for i := 0 to high(usedCmdlineVariables) do
        if usedCmdlineVariables[i].count > 1 then
          writeItem('SET #'+usedCmdlineVariables[i].name +'='+ inttostr(usedCmdlineVariables[i].count));
  end;

  if outputfooter <> '' then w(outputFooter)
  else if not mycmdline.existsProperty('output-footer') and not firstItem then wln();

  mycmdLine.free;
  tracer.free;
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
  result := jn.findBasicFunction('parse-json', length(args)).func(args);
end;

function xqFunctionBlocked(const context: TXQEvaluationContext; const args: TXQVArray): IXQValue;
begin
  raise EXQEvaluationException.create('pxp:cgi', 'function is not allowed in cgi mode');
  result := nil;
end;

procedure blockFileAccessFunctions;
var fn, pxp, jn: TXQNativeModule;
begin
  fn := TXQueryEngine.findNativeModule(XMLNamespaceURL_XPathFunctions);
  fn.findComplexFunction('doc', 1).func:=@xqFunctionBlocked;
  fn.findComplexFunction('doc-available', 1).func:=@xqFunctionBlocked;

  pxp := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensions);
  pxp.findComplexFunction('json', 1).func:=@xqFunctionJSONSafe;

  jn := TXQueryEngine.findNativeModule('http://jsoniq.org/functions');
  jn.findComplexFunction('json-doc', 1).func:=@xqFunctionBlocked;
end;

var pxp: TXQNativeModule;

{ TMultiParameterList }


initialization
  pxp := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensions);
  pxp.registerFunction('system', @xqfSystem, ['($arg as xs:string) as xs:string']);
  pxp.registerFunction('read', @xqfRead, ['() as xs:untypedAtomic']);
end.

