program corona_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, DataItemTests, UtilsTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

