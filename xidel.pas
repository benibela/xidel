program xibri;

{$mode objfpc}{$H+}

uses Interfaces, simpleinternet, internetaccess, multipagetemplate, bbutils,
     xibribase,
     rcmdline  //<< if you don't have this command line parser unit, you can download it from www.benibela.de
     ;


 { TTemplateReaderBreaker }


function prepareInternet(const userAgent, proxy: string): TInternetAccess;
begin
  simpleinternet.needInternetAccess;
  if assigned(simpleinternet.defaultInternet.internetConfig) then begin
    simpleinternet.defaultInternet.internetConfig^.userAgent := userAgent;
    simpleinternet.defaultInternet.internetConfig^.setProxy(proxy);
  end;
  result := simpleinternet.defaultInternet;
end;

function retrieve(const url, post: string): string;
begin
  if cgimode then result := url //disallow remote access in cgi mode
  else if post = '' then result := simpleinternet.retrieve(url)
  else result := httpRequest(url, post);
end;

begin
  xibribase.cgimode := false;
  xibribase.allowInternetAccess := true;
  xibribase.mycmdline := TCommandLineReader.create;
  xibribase.onPrepareInternet := @prepareInternet;
  xibribase.onRetrieve := @retrieve;
  xibribase.defaultUserAgent := defaultInternetConfiguration.userAgent;


  xibribase.perform;
end.

