unit htmlparserexample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,simpleinternet,internetaccess,LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function trimmy(s: string): string;
begin
  if length(s) = 0 then exit('');
  if s[length(s)] = #10 then delete(s, length(s), 1);
  result := s;
end;

procedure TForm1.Button1Click(Sender: TObject);
var v: IXQValue;
begin
  try
    memo1.Clear;
    for v in process(trimmy(edit1.text), trimmy(edit2.text)) do
      memo1.lines.add(v.toString);
    memo1.repaint;
  except
    on einternet: EInternetException do
      Application.MessageBox(pchar(einternet.Message), 'Xidel - Internet Error', MB_OK);
  end;
end;

end.

