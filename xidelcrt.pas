unit xidelcrt;

{$mode objfpc}{$H+}{$COperators on}

interface

uses rcmdline, sysutils;

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
type TColorOptions = (cAuto, cNever, cAlways, cJSON, cXML);
procedure wcolor(const s: string; color: TColorOptions);
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

  firstItem: boolean = true;





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
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<?xml version="1.0" encoding="'+ strEncodingName(GetTextCodePage(Output))+'"?>'+LineEnding+outputHeader;
    end;
    'xml-wrapped': begin
      outputFormat:=ofXMLWrapped;
      if not mycmdline.existsProperty('output-declaration') then outputHeader:='<?xml version="1.0" encoding="'+ strEncodingName(GetTextCodePage(Output))+'"?>'+LineEnding+outputHeader;
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
        ofXMLWrapped, ofRawHTML, ofRawXML: colorizing := cXML;
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

procedure setOutputFileName(n: string; mycmdline: TCommandLineReader);
begin
  if xidelOutputFileName = n then exit;
  if xidelOutputFileName <> '' then begin
    if outputfooter <> '' then wcolor(outputFooter, colorizing)
    else if not mycmdline.existsProperty('output-footer') and not firstItem then wln();
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

  if outputHeader <> '' then wcolor(outputHeader, colorizing);
  if outputFormat in [ofJsonWrapped, ofXMLWrapped] then needRawWrapper(mycmdline);
end;

var hasRawWrapper: boolean = false;
procedure needRawWrapper(mycmdline: TCommandLineReader);
  procedure setHeaderFooter(const h, f: string);
  begin
    if outputFormat = ofJsonWrapped then wcolor(h, cJSON)
    else wcolor(h, cXML);
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

  XML_COLOR_COMMENT: TConsoleColors = ccBlue;
  XML_COLOR_TAG: TConsoleColors = ccYellowBold;
  XML_COLOR_ATTRIB_NAME: TConsoleColors = ccPurpleBold;
  XML_COLOR_ATTRIB_VALUE: TConsoleColors = ccGreenBold;


var pos, lastpos: integer;

procedure colorChange(c: TConsoleColors);
begin
w(copy(s, lastpos, pos - lastpos));
setTerminalColor(false, c);
lastpos:=pos;
end;

var    quote: Char;
scriptSpecialCase: Boolean;
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
cXML: begin
  pos := 1;
  lastpos := 1;
  scriptSpecialCase := false;
  while pos <= length(s) do begin
    case s[pos] of
      '<': if scriptSpecialCase and not striBeginsWith(@s[pos], '</script') then inc(pos)
      else begin
        colorChange(XML_COLOR_TAG);
        if (pos + 1) <= length(s) then begin
          case s[pos+1] of
            '/', '?': inc(pos,2);
            '!': if strBeginsWith(@s[pos], '<!--') then begin
              colorChange(XML_COLOR_COMMENT);
              inc(pos,3);
              while (pos + 3 <= length(s)) and ((s[pos] <> '-') or (s[pos+1] <> '-')or (s[pos+2] <> '>')) do inc(pos);
              inc(pos);
              continue;
            end;
          end;
        end;
        scriptSpecialCase := striBeginsWith(@s[pos], '<script');
        while (pos <= length(s)) and not (s[pos] in ['>','/','?',#0..#32]) do inc(pos);
        while (pos <= length(s)) do begin
          case s[pos] of
            '>','/','?': begin
              colorChange(XML_COLOR_TAG);
              if s[pos] <> '>' then inc(pos);
              break;
            end;
             #0..#32: ;
             else begin
               colorChange(XML_COLOR_ATTRIB_NAME);
               while (pos <= length(s)) and not (s[pos] in ['=','/','>']) do inc(pos);
               colorChange(XML_COLOR_TAG);
               if s[pos] <> '=' then break;
               inc(pos);
               while (pos <= length(s)) and (s[pos] in [#0..#32]) do inc(pos);
               colorChange(XML_COLOR_ATTRIB_VALUE);
               if (pos <= length(s)) then
                 case s[pos] of
                   '''', '"': begin
                     quote := s[pos];
                     inc(pos);
                     while (pos <= length(s)) and (s[pos] <> quote) do inc(pos);
                     inc(pos);
                   end;
                   else while (pos <= length(s)) and not (s[pos] in [#0..#32]) do inc(pos);
                 end;
               continue;
             end;
          end;
          inc(pos);
        end;
        inc(pos);
        colorChange(ccNormal);
      end;
      else inc(pos);
    end;
  end;
  colorChange(ccNormal)
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




procedure writeItem(const s: string; color: TColorOptions = cNever);
begin
  if not firstItem then begin
    w(outputSeparator);
  end;
  wcolor(s, color);
  firstItem := false;
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


end.

