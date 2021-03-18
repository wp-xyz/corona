program corona;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, cmain, cBasicFrame, cMapFrame, cTimeSeriesFrame;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

