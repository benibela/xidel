program xidel;

{$mode objfpc}{$H+}

uses //heaptrc,
     simpleinternet, internetaccess, multipagetemplate, bbutils,
     xidelbase,
     rcmdline  //<< if you don't have this command line parser unit, you can download it from www.benibela.de
     ;


 { TTemplateReaderBreaker }


function prepareInternet(const userAgent, proxy: string): TInternetAccess;
begin
  defaultInternetConfiguration.userAgent:=userAgent;
  defaultInternetConfiguration.setProxy(proxy);
  simpleinternet.needInternetAccess;
  if assigned(simpleinternet.defaultInternet.internetConfig) then begin
    simpleinternet.defaultInternet.internetConfig^.userAgent := userAgent;
    simpleinternet.defaultInternet.internetConfig^.setProxy(proxy);
  end;
  result := simpleinternet.defaultInternet;
  defaultInternetAccessClass := TInternetAccessClass( result.ClassType);
end;

function retrieve(const method, url, post: string): string;
begin
  if cgimode then result := url //disallow remote access in cgi mode
  else if (post = '') and ((method = '') or (method = 'GET')) then result := simpleinternet.retrieve(url)
  else begin
    needInternetAccess;
    needInternetAccess();
    result:=defaultInternet.request(method, url, post);
  end;
end;

{$R *.res}

begin
  xidelbase.cgimode := false;
  xidelbase.allowInternetAccess := true;
  xidelbase.mycmdline := TCommandLineReader.create;
  xidelbase.onPrepareInternet := @prepareInternet;
  xidelbase.onRetrieve := @retrieve;


  xidelbase.perform;
end.

