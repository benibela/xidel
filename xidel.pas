program xidel;

{$mode objfpc}{$H+}

{$if defined(win32) and not defined(USE_SYNAPSE_WRAPPER)}
{$define USE_WININET_WRAPPER}
{$else}
{$define USE_SYNAPSE_WRAPPER}
{$endif}

uses //heaptrc,
     internetaccess, multipagetemplate, bbutils,
     xidelbase,
     rcmdline, //<< if you don't have this command line parser unit, you can download it from www.benibela.de
     xquery_module_file
     {$ifdef USE_WININET_WRAPPER},w32internetaccess{$endif}
     {$ifdef USE_SYNAPSE_WRAPPER},synapseinternetaccess{$endif}
     ;


 { TTemplateReaderBreaker }


function prepareInternet(const userAgent, proxy: string; onReact: TTransferReactEvent): TInternetAccess;
begin
  defaultInternetConfiguration.userAgent:=userAgent;
  defaultInternetConfiguration.setProxy(proxy);
  if assigned(internetaccess.defaultInternet.internetConfig) then begin
    internetaccess.defaultInternet.internetConfig^.userAgent := userAgent;
    internetaccess.defaultInternet.internetConfig^.setProxy(proxy);
  end;
  result := internetaccess.defaultInternet;
  defaultInternetAccessClass := TInternetAccessClass( result.ClassType);
  result.OnTransferReact := onReact;
end;

function retrieve(const method, url, post, headers: string): string;
var
  trimmed: String;
begin
  defaultInternet.additionalHeaders.Text := headers;
  if cgimode then result := url //disallow remote access in cgi mode
  else if (post = '') and ((method = '') or (method = 'GET')) then begin
    trimmed := strTrim(url);
    case guessType(trimmed) of
      rtEmpty: exit('');
      rtRemoteURL: exit(internetaccess.httpRequest(trimmed));
      rtFile: exit(strLoadFromFileUTF8(trimmed));
      else exit(url);
    end;
  end else result:=internetaccess.httpRequest(method, url, post);
end;

//{$R *.res}

begin
  {$ifdef USE_WININET_WRAPPER}defaultInternetAccessClass := TW32InternetAccess;{$endif}
  {$ifdef USE_SYNAPSE_WRAPPER}defaultInternetAccessClass := TSynapseInternetAccess;{$endif}
  registerModuleFile;

  if Paramcount = 0 then begin
    writeln(stderr, 'Xidel XQuery/XPath/CSS/JSONiq engine and webscraper');
    writeln(stderr, 'Use --help for a list of available command line parameters.');
    writeln(stderr, 'Or --usage for the usage information of the readme.');
    ExitCode:=1;
    exit;
  end;

  xidelbase.cgimode := false;
  xidelbase.allowInternetAccess := true;
  xidelbase.mycmdline := TCommandLineReader.create;
  xidelbase.onPrepareInternet := @prepareInternet;
  xidelbase.onRetrieve := @retrieve;


  xidelbase.perform;
end.

