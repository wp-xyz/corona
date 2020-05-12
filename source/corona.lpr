program corona;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, //tachartlazaruspkg,
  cmain
  {, cDownloader, cRobertKochInstitut,
  cDataSource, cJohnsHopkinsUniversity, cGlobal, cSeries}
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

