program xibrisoap;

{$mode objfpc}{$H+}

uses
  xibribase,
  rcmdlinecgi
  { you can add units after this };

begin
  xibribase.cgimode := true;
  xibribase.allowInternetAccess := false;
  xibribase.mycmdline := TCommandLineReaderCGI.create;

  xibribase.perform;
end.

