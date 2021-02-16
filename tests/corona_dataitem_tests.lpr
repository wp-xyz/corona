program corona_dataitem_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, DataItemTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

