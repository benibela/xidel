program xidelsoap;

{$mode objfpc}{$H+}

uses
  xidelbase,
  rcmdlinecgi
  { you can add units after this };

begin
  xidelbase.cgimode := true;
  xidelbase.allowInternetAccess := false;
  xidelbase.mycmdline := TCommandLineReaderCGI.create;

  xidelbase.perform;
end.

