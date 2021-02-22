unit UtilsTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TUtilsTests = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SplitTest;
  end;

implementation

uses
  cUtils;

procedure TUtilsTests.SplitTest;
const
  InputStr = ',"Korea, South",35.907757,127.766922,1,1,2,2';
var
  fields: TStrings;
begin
  fields := TStringList.Create;
  try
    Split(InputStr, fields, ',');
    CheckEquals(7, fields.Count, 'Fields.Count mismatch');
    CheckEquals('', fields[0], 'Fields[0] value mismatch');
    CheckEquals('"Korea, South"', fields[1], 'Fields[1] (quoted) mismatch');
    CheckEquals('35.907757', fields[2], 'Fields[2] value mismatch');
  finally
    fields.Free;
  end;
end;

procedure TUtilsTests.SetUp;
begin
  //
end;

procedure TUtilsTests.TearDown;
begin
  //
end;

initialization
  RegisterTest(TUtilsTests);

end.

