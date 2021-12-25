unit xidelcrt;

{$mode objfpc}{$H+}{$COperators on}

interface

uses rcmdline, sysutils, simplehtmlparser;

var
  allowFileAccess: boolean = true;

type

EXidelException = class(Exception);
EXidelInvalidArgument = class(EXidelException);


TConsoleColors = (ccNormal, ccWhiteBold, ccRedBold, ccGreenBold, ccBlueBold, ccPurpleBold, ccYellowBold, ccCyanBold,
                              ccRed, ccGreen, ccBlue, ccPurple, ccYellow
 );

procedure setTerminalColor(err: boolean; color: TConsoleColors);
procedure initOutput(mycmdline: TCommandLineReader);
procedure setOutputEncoding(e: string);
procedure setOutputFileName(n: string; mycmdline: TCommandLineReader);
procedure needRawWrapper(mycmdline: TCommandLineReader);
procedure endOutput(mycmdline: TCommandLineReader);


procedure w(const s: string);
procedure wln(const s: string = '');
type TColorOptions = (cAuto, cNever, cAlways, cJSON, cXML, cHTML);
procedure wcolor(const s: string; color: TColorOptions);
procedure writeLineBreakAfterDeclaration;
procedure writeItem(const s: string; color: TColorOptions = cNever);
procedure writeVarName(const s: string; color: TColorOptions = cNever);
procedure werr(const s: string);
procedure werrln(const s: string);

function strReadFromStdin: string;

type TOutputFormat = (ofAdhoc, ofJsonWrapped, ofXMLWrapped, ofRawXML, ofRawHTML, ofBash, ofWindowsCmd);
var //output options
    outputFormat: TOutputFormat;
    windowsCmdPercentageEscape: string;
    hasOutputEncoding: (oeAbsent,oeConvert,oePassRaw) = oeAbsent;
    outputEncoding: TSystemCodePage;
    outputHeader, outputFooter, outputSeparator: string;
    //outputArraySeparator: array[toutputformat] of string = ('',  ', ', '</e><e>', '', '', '', '');

var
  {$ifdef windows}systemEncodingIsUTF8: boolean = true;{$endif}

  lastConsoleColor: TConsoleColors = ccNormal;
  isStdinTTY: boolean = false;
  isStderrTTY: boolean = false;
  isStdoutTTY: boolean = false;


  xidelOutputFile: TextFile;
  xidelOutputFileName: string;

  colorizing: TColorOptions;

  firstItem: boolean = true;
  lastWrittenChar: char = #0;
  implicitLineBreakAfterDeclaration: boolean = false;
implementation
uses bbutils
  {$ifdef unix}, termio{$endif}
  {$ifdef windows} ,windows {$endif}
  ;


var
  {$ifdef windows}
  backgroundColor: integer = 0;
  stdoutTextAttributes: integer = 0;
  stderrTextAttributes: integer = 0;
  {$endif}

  stacklen: SizeInt;
  stack: TLongintArray;






procedure setTerminalColor(err: boolean; color: TConsoleColors);
{$ifdef unix}
const colorCodes: array[TConsoleColors] of string = (
   #27'[0m', #27'[1;37m', #27'[1;31m', #27'[1;32m', #27'[1;34m', #27'[1;35m', #27'[1;33m', #27'[1;36m',
                          #27'[0;31m', #27'[0;32m', #27'[0;34m', #27'[0;35m', #27'[0;33m'
   );
var
  f: TextFile;
{$endif}
{$ifdef windows}
const colorCodes: array[TConsoleColors] of integer = (
   FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE, FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY,
     FOREGROUND_RED or FOREGROUND_INTENSITY, FOREGROUND_GREEN or FOREGROUND_INTENSITY, FOREGROUND_BLUE or FOREGROUND_INTENSITY, FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY, FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY, FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY,
     FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE, FOREGROUND_RED or FOREGROUND_BLUE, FOREGROUND_RED or FOREGROUND_GREEN
   );
var handle: Integer;
{$endif}
begin
  if err and not isStderrTTY then exit;
  if not err and not isStdoutTTY then exit;
  if color <> lastConsoleColor then begin
    if err then Flush(stderr) else flush(xidelOutputFile);
    {$ifdef unix}
    if err then f := stderr else f := xidelOutputFile;
    write(f, colorCodes[color]);
    {$endif}
    {$ifdef windows}
    if err then handle := StdErrorHandle else handle := StdOutputHandle;
    SetConsoleTextAttribute(handle, colorCodes[color] or backgroundColor);
    {$endif}
    lastConsoleColor := color;
  end;
end;

function strEncodingFromNameXidel(const e: string): integer;
begin
  result := strEncodingFromName(e);
  if result = CP_NONE then
    writeln(stderr, 'Unknown encoding: ',e)
end;

procedure setOutputEncoding(e: string);
begin
  if e <> 'input' then begin
    hasOutputEncoding := oeConvert;
    outputEncoding := strEncodingFromNameXidel(e);
  end else begin
    hasOutputEncoding := oePassRaw;
    outputEncoding := CP_ACP; //all our strings claim to be ACP (=UTF8) so there should be no conversion?
  end;
  if xidelOutputFileName <> '' then
    SetTextCodePage(xidelOutputFile, outputEncoding);
end;


procedure initOutput(mycmdline: TCommandLineReader);
{$ifdef windows}
var
  consoleBuffer: TConsoleScreenBufferInfo;
{$endif}
begin
  outputSeparator := mycmdline.readString('output-separator');
  outputFooter := mycmdline.readString('output-footer');
  case mycmdLine.readString('output-format') of
    'adhoc': outputFormat:=ofAdhoc;
    'html': begin
      outputFormat:=ofRawHTML;
    end;
    'xml': begin
      outputFormat:=ofRawXML;
    end;
    'xml-wrapped': begin
      outputFormat:=ofXMLWrapped;
    end;
    'json', 'json-wrapped': begin
      outputFormat:=ofJsonWrapped;
      if (mycmdLine.readString('output-format') = 'json') then writeln(stderr, 'Warning: Output-format json is deprecated, use json-wrapped instead');
    end;
    'bash': begin
      outputFormat:=ofBash;
    end;
    'cmd':  begin //legacy. remove it?
      outputFormat:=ofWindowsCmd;
      windowsCmdPercentageEscape := '';
    end;
    'cmd-bat':  begin
      outputFormat:=ofWindowsCmd;
      windowsCmdPercentageEscape := '%'; //% must be escaped as %% in bat files
    end;
    'cmd-for':  begin
      outputFormat:=ofWindowsCmd;
      windowsCmdPercentageEscape := '^'; //for /f only accepts ^%, not %%
    end;
    else raise EXidelInvalidArgument.Create('Unknown output format: ' + mycmdLine.readString('output-format'));
  end;

  case mycmdline.readString('color') of
    'auto': colorizing := cAuto;
    'never': colorizing := cNever;
    'always': colorizing := cAlways;
    'json': colorizing := cJSON;
    'xml': colorizing := cXML;
    'html': colorizing := cHTML;
    else raise EXidelInvalidArgument.Create('Invalid color: '+mycmdline.readString('color'));
  end;
  {$ifdef unix}
  isStdinTTY := IsATTY(Input) <> 0;
  isStdoutTTY := IsATTY(stdout) <> 0;
  isStderrTTY := IsATTY(StdErr) <> 0;
  {$endif}
  {$ifdef windows}
  isStdinTTY :=  getfiletype(StdInputHandle) = FILE_TYPE_CHAR;
  isStdoutTTY := getfiletype(StdOutputHandle) = FILE_TYPE_CHAR;
  isStderrTTY := getfiletype(StdErrorHandle) = FILE_TYPE_CHAR;
  {$endif}
  if not (colorizing in [cNever,cAlways]) or (hasOutputEncoding = oeAbsent) then begin
    if not isStdoutTTY and (hasOutputEncoding = oeAbsent) then setOutputEncoding('utf-8');
    if not isStdinTTY or mycmdline.existsProperty('stdin-encoding') then SetTextCodePage(input, strEncodingFromNameXidel(mycmdline.readString('stdin-encoding')));
  end;

  case colorizing of
    cNever: begin
      isStderrTTY := false; //todo, coloring should not change this variable (but it is only used for coloring. rename it?)
      isStdoutTTY := false;
    end;
    cAlways: begin
      isStdoutTTY := true;
      isStderrTTY := true;
    end;
  end;
  case colorizing of
    cAuto, cAlways: begin
      case outputFormat of
        ofXMLWrapped, ofRawXML: colorizing := cXML;
        ofRawHTML: colorizing := cHTML;
        ofJsonWrapped: colorizing := cJSON;
      end;
    end;
  end;
  {$ifdef windows}
  if colorizing <> cNever then begin
    if isStderrTTY and GetConsoleScreenBufferInfo(StdErrorHandle, @consoleBuffer) then begin;
      stderrTextAttributes := consoleBuffer.wAttributes;
      backgroundColor := stderrTextAttributes and (BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY);
    end;

    if isStdoutTTY and GetConsoleScreenBufferInfo(StdOutputHandle, @consoleBuffer) then begin;
      stdoutTextAttributes := consoleBuffer.wAttributes;
      backgroundColor := stdoutTextAttributes and (BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY);
    end;
  end;
  {$endif}

end;

procedure writeOutputHeader(mycmdline: TCommandLineReader);
var
  outputDeclaration: String;
begin
  outputDeclaration := mycmdline.readString('output-declaration');
  outputHeader :=  mycmdline.readString('output-header');
  case outputFormat of
    ofRawHTML:
      if not mycmdline.existsProperty('output-declaration') then outputDeclaration:='<!DOCTYPE html>';
    ofRawXML, ofXMLWrapped:
      if not mycmdline.existsProperty('output-declaration') then outputDeclaration:='<?xml version="1.0" encoding="'+ strEncodingName(GetTextCodePage(xidelOutputFile))+'"?>';
  end;
  if (outputHeader <> '') and (outputDeclaration <> '') then
    outputDeclaration += LineEnding
  else if outputDeclaration <> '' then
    implicitLineBreakAfterDeclaration := (outputFormat in [ofRawHTML, ofRawXML, ofXMLWrapped]) or (mycmdline.readFlag('output-node-indent'));
  outputHeader := outputDeclaration + outputHeader;
  if outputHeader <> '' then wcolor(outputHeader, colorizing);
end;

procedure setOutputFileName(n: string; mycmdline: TCommandLineReader);
begin
  if xidelOutputFileName = n then exit;
  if xidelOutputFileName <> '' then begin
    if outputfooter <> '' then wcolor(outputFooter, colorizing)
    else if not mycmdline.existsProperty('output-footer') and not firstItem and not (lastWrittenChar in [#13,#10]) then wln();
    flush(xidelOutputFile);
    if not striBeginsWith(xidelOutputFileName, 'stdout:') then CloseFile(xidelOutputFile);
  end;

  xidelOutputFileName := n;
  if striBeginsWith(xidelOutputFileName, 'http://') or striBeginsWith(xidelOutputFileName, 'https://') then
    raise Exception.Create('Cannot output to webpage')
  else if striBeginsWith(xidelOutputFileName, 'stdout:') or (xidelOutputFileName = '') then begin
    xidelOutputFile := output;
  end else begin
    if not allowFileAccess then
      raise EXidelException.create('output file changing is not allowed in CGI mode');
    xidelOutputFileName := strRemoveFileURLPrefix(xidelOutputFileName);
    colorizing := cNever;
    AssignFile(xidelOutputFile, xidelOutputFileName);
    Rewrite(xidelOutputFile);
  end;
  if hasOutputEncoding <> oeAbsent then
    SetTextCodePage(xidelOutputFile, outputEncoding);

  if n = '' then exit;

  writeOutputHeader(mycmdline);
  if outputFormat in [ofJsonWrapped, ofXMLWrapped] then needRawWrapper(mycmdline);
end;

var hasRawWrapper: boolean = false;
procedure needRawWrapper(mycmdline: TCommandLineReader);
  procedure setHeaderFooter(const h, f: string);
  begin
    case outputFormat of
      ofJsonWrapped: wcolor(h, cJSON);
      ofRawHTML: wcolor(h, cHTML);
      else wcolor(h, cXML);
    end;
    if outputSeparator = LineEnding then wln();
    if not mycmdline.existsProperty('output-footer') then outputFooter := f + LineEnding;
  end;

var
  le: string;
begin
  if hasRawWrapper then exit;
  hasRawWrapper := true;
  writeLineBreakAfterDeclaration;
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

procedure endOutput(mycmdline: TCommandLineReader);
begin
  setOutputFileName('', mycmdline);
  {$ifdef windows}if colorizing <> cNever then begin
    if (stdoutTextAttributes <> 0) and isStdoutTTY then
      SetConsoleTextAttribute(StdOutputHandle, stdoutTextAttributes);
    if (stderrTextAttributes <> 0) and isStderrTTY then
      SetConsoleTextAttribute(StdErrorHandle, stderrTextAttributes);
  end;
  {$endif}
end;

procedure w(const s: string);
{$IFnDEF FPC_HAS_CPSTRING}{$ifdef win32}
var
  temp, temp2: String;
{$endif}{$endif}
begin
  if s = '' then exit;
  {$IFDEF FPC_HAS_CPSTRING}
  write(xidelOutputFile, s);
  {$ELSE}
  fpc 3 is required now
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
  {$ENDIF}
end;

procedure wln(const s: string = '');
begin
  w(s);
  w(LineEnding);
end;


type TMLHighlighter = class
  data: string;
  marker: pchar;
  procedure writeHighlighted(const s: string; mode: TColorOptions);
  procedure writeHighlighted(start: pchar; len: SizeInt; color: TConsoleColors);
  function enterTag(tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties):TParsingResult;
  function leaveTag(tagName: pchar; tagNameLen: SizeInt):TParsingResult;
  function commentNode(comment: pchar; commentLen: SizeInt):TParsingResult;
  procedure docType(name: pchar; nameLen: SizeInt; more: pchar; moreLen: SizeInt);
  function textNode(text: pchar; textLen: SizeInt; textFlags: TTextFlags):TParsingResult;
  function processingInstruction(text: pchar; textLen: SizeInt; textFlags: TTextFlags):TParsingResult;
end;
var globalMLHighlighter: TMLHighlighter;

const
  XML_COLOR_COMMENT: TConsoleColors = ccBlue;
  XML_COLOR_TAG: TConsoleColors = ccYellowBold;
  XML_COLOR_ATTRIB_NAME: TConsoleColors = ccPurpleBold;
  XML_COLOR_ATTRIB_VALUE: TConsoleColors = ccGreenBold;

procedure TMLHighlighter.writeHighlighted(const s: string; mode: TColorOptions);
var options: TParsingOptions;
begin
  if s = '' then exit;
  data := s;
  marker := @data[1];
  if mode = cHTML then options := [poRespectHTMLCDATAElements,poRespectHTMLProcessingInstructions]
  else options := [poRespectXMLProcessingInstructions];
  parseML(data, options, @enterTag, @leaveTag, @textNode, @commentNode, @processingInstruction, @docType);
  writeHighlighted(@data[1] + length(data), 0, ccNormal);
end;

procedure TMLHighlighter.writeHighlighted(start: pchar; len: SizeInt; color: TConsoleColors);
var
  overlap: sizeint;
begin
  if marker < start then begin
    setTerminalColor(false, ccNormal);
    w(strFromPchar(marker, start - marker));
  end else if marker >= start then begin
    overlap := marker - start;
    len := len - overlap;
    start := start + overlap;
  end;
  if len > 0 then begin
    setTerminalColor(false, color);
    w(strFromPchar(start, len));
  end;
  marker := start + len;
end;

function TMLHighlighter.enterTag(tagName: pchar; tagNameLen: SizeInt; properties: THTMLProperties): TParsingResult;
var
  i: sizeint;
begin
  result := prContinue;
  writeHighlighted(tagName, tagNameLen, XML_COLOR_TAG);
  for i := 0 to high(properties) do begin
    writeHighlighted(properties[i].name, properties[i].nameLen, XML_COLOR_ATTRIB_NAME);
    writeHighlighted(properties[i].value, properties[i].valueLen, XML_COLOR_ATTRIB_VALUE);
  end;
end;
function TMLHighlighter.leaveTag(tagName: pchar; tagNameLen: SizeInt): TParsingResult;
begin
  result := prContinue;
  writeHighlighted(tagName, tagNameLen, XML_COLOR_TAG);
end;
function TMLHighlighter.commentNode(comment: pchar; commentLen: SizeInt): TParsingResult;
begin
  result := prContinue;
  writeHighlighted(comment, commentLen, XML_COLOR_COMMENT);
end;
procedure TMLHighlighter.docType(name: pchar; nameLen: SizeInt; more: pchar; moreLen: SizeInt);
begin
  writeHighlighted(name, nameLen, XML_COLOR_ATTRIB_NAME);
end;
function TMLHighlighter.textNode(text: pchar; textLen: SizeInt; textFlags: TTextFlags): TParsingResult;
begin
  result := prContinue;
end;
function TMLHighlighter.processingInstruction(text: pchar; textLen: SizeInt; textFlags: TTextFlags): TParsingResult;
var
  nameEnd: sizeint;
begin
  result := prContinue;
  nameEnd := 0;
  while (nameEnd < textLen) and not (text[nameEnd] in WHITE_SPACE) do inc(nameEnd);
  writeHighlighted(text, nameEnd, XML_COLOR_TAG);
  writeHighlighted(text + nameEnd, textLen - nameEnd, XML_COLOR_ATTRIB_VALUE);
end;


procedure wcolor(const s: string; color: TColorOptions);
const JSON_COLOR_OBJECT_PAREN: TConsoleColors = ccYellowBold;
  JSON_COLOR_OBJECT_KEY: TConsoleColors = ccPurpleBold;
  JSON_COLOR_ARRAY_PAREN: TConsoleColors = ccGreenBold;
{$ifdef windows}
  JSON_COLOR_STRING: TConsoleColors = ccCyanBold; //green is ugly on windows
{$else}
  JSON_COLOR_STRING: TConsoleColors = ccGreen;
{$endif}

  JSON_STATE_ARRAY = 1;
  JSON_STATE_OBJECTVALUE = 2;
  JSON_STATE_OBJECTKEY = 3;



var pos, lastpos: integer;

procedure colorChange(c: TConsoleColors);
begin
w(copy(s, lastpos, pos - lastpos));
setTerminalColor(false, c);
lastpos:=pos;
end;

begin
case color of
cJSON: begin
  if stacklen = 0 then arrayAddFast(stack, stacklen, 0);
  pos := 1;
  lastpos := 1;

  while pos <= length(s) do begin
    case s[pos] of
      '{', '}': begin
        if s[pos] = '{' then arrayAddFast(stack, stacklen, JSON_STATE_OBJECTKEY)
        else if stacklen > 1 then dec(stacklen);
        colorChange(JSON_COLOR_OBJECT_PAREN);
        inc(pos);
        colorChange(ccNormal);
      end;
      '[', ']': begin
        if s[pos] = '[' then arrayAddFast(stack, stacklen, JSON_STATE_ARRAY)
        else if stacklen > 1 then dec(stacklen);
        colorChange(JSON_COLOR_ARRAY_PAREN);
        inc(pos);
        colorChange(ccNormal);
      end;
      ',', ':': begin
        case stack[stacklen-1] of
          JSON_STATE_OBJECTKEY, JSON_STATE_OBJECTVALUE: begin
            colorChange(JSON_COLOR_OBJECT_PAREN);
            if s[pos] = ',' then stack[stacklen-1] := JSON_STATE_OBJECTKEY
            else stack[stacklen-1] := JSON_STATE_OBJECTVALUE;
          end;
          JSON_STATE_ARRAY: colorChange(JSON_COLOR_ARRAY_PAREN);
        end;
        inc(pos);
        colorChange(ccNormal);
      end;
      '"': begin
        case stack[stacklen-1] of
          JSON_STATE_OBJECTKEY: colorChange(JSON_COLOR_OBJECT_KEY);
          else colorChange(JSON_COLOR_STRING);
        end;
        inc(pos);
        while (pos <= length(s)) and (s[pos] <> '"') do begin
          if s[pos] = '\' then inc(pos);
          inc(pos);
        end;
        inc(pos);
      end
      else inc(pos);
    end;
  end;
  colorChange(ccNormal)
end;
cXML,cHTML: begin
  if globalMLHighlighter = nil then globalMLHighlighter := TMLHighlighter.Create;
  globalMLHighlighter.writeHighlighted(s, color);
end;
else w(s);
end;
end;

procedure werr(const s: string);
begin
  if not firstItem then writeln(stderr);
  write(stderr, s);
end;

procedure werrln(const s: string);
begin
  if not firstItem then writeln(stderr);
  writeln(stderr, s);
end;



procedure writeLineBreakAfterDeclaration;
begin
  if implicitLineBreakAfterDeclaration then begin
    wln();
    lastWrittenChar := #10;
    implicitLineBreakAfterDeclaration := false;
  end;
end;

procedure writeItem(const s: string; color: TColorOptions = cNever);
begin
  if not firstItem then begin
    w(outputSeparator);
  end else if (outputHeader <> '') and (outputSeparator = LineEnding) and (s <> '') and not (s[1] in [#13,#10]) then writeLineBreakAfterDeclaration;
  wcolor(s, color);
  firstItem := false;
  if s <> '' then lastWrittenChar := s[length(s)];
end;

procedure writeVarName(const s: string; color: TColorOptions = cNever);
begin
  writeItem(s, color);
  firstItem := true; //prevent another line break / separator
end;


type
  FileFunc = Procedure(var t : TextRec);
{$PUSH}{$R-}
procedure nextTextRecBlock(var tr: TextRec; out from: pchar; out len: SizeInt);
begin
  if tr.bufpos >= tr.bufend then FileFunc(tr.inoutfunc)(tr);
  if tr.bufpos < tr.bufend then begin
    from := @tr.bufptr^[tr.bufpos];
    len := @tr.bufptr^[tr.bufend] - from;
  end else begin
    from := nil;
    len := 0;
  end;
  tr.bufpos := tr.bufend;
end;
{$POP}

function strReadFromStdin: string;
var s:string;
    sb: TStrBuilder;
    from: pchar;
    len: SizeInt;
begin
  sb.init(@result);
  if isStdinTTY then begin
    while not EOF(Input) do begin
      ReadLn(s);
      sb.append(s);
      sb.append(LineEnding);
    end;
  end else begin
    while not eof(Input) do begin
      nextTextRecBlock(TextRec(input), from, len);
      if len > 0 then
        sb.append(from, len);
    end;
  end;
  sb.final;
end;

finalization
  if globalMLHighlighter <> nil then globalMLHighlighter.free

end.

