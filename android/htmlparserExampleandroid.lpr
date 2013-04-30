library htmlparserExampleandroid;

{$mode objfpc}{$H+}


uses
  customdrawnint, Interfaces, Forms, htmlparserexample,
  customdrawn_android,customdrawndrawers,jni;

exports
  JNI_OnLoad name 'JNI_OnLoad',
  JNI_OnUnload name 'JNI_OnUnload';

procedure MyActivityOnCreate;
begin
  DefaultStyle := dsAndroid;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end;

begin
  CDWidgetset.ActivityClassName := 'com/pascal/xidel/LCLActivity';
  CDWidgetset.ActivityOnCreate := @MyActivityOnCreate;
end.

