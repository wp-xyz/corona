program corona;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms,
  {$IFDEF CORONA_DEBUG_LOG}
  LazLoggerBase,
  {$ENDIF}
  cmain, cBasicFrame, cMapFrame, cTimeSeriesFrame, cFixes,
  cDataModule;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainDatamodule, MainDatamodule);
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

