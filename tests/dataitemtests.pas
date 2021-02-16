unit DataItemTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  cGlobal, cDataSource;

type
  TData = record
    Date: TDate;
    Confirmed, Deaths, Recovered: Int64;
  end;

  TCoronaTestCase = class(TTestCase)
  protected
    FDataItem: TcDataItem;
    FTestData: array of TData;
    procedure ReadTestData;
    procedure TestDataToDataItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create; override;
  end;

  TDateTests = class(TCoronaTestCase)
  published
    procedure StartDateTest;
    procedure EndDateTest;
    procedure SomeDateTest;
  end;

  TCumulativeCasesTests = class(TCoronaTestCase)
  private
    procedure CaseCountTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure CaseCountTest_Confirmed_Date1;
    procedure CaseCountTest_Confirmed_Date2;
    procedure CaseCountTest_Confirmed_Date3;
    procedure CaseCountTest_Deaths_Date1;
    procedure CaseCountTest_Deaths_Date2;
    procedure CaseCountTest_Deaths_Date3;
    procedure CaseCountTest_Recovered_Date1;
    procedure CaseCountTest_Recovered_Date2;
    procedure CaseCountTest_Recovered_Date3;
    procedure CaseCountTest_Sick_Date1;
    procedure CaseCountTest_Sick_Date2;
    procedure CaseCountTest_Sick_Date3;
  end;

  TNewCasesTests = class(TCoronaTestCase)
  private
    procedure CaseCountTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure CaseCountTest_Confirmed_Date1;
    procedure CaseCountTest_Confirmed_Date2;
    procedure CaseCountTest_Confirmed_Date3;
    procedure CaseCountTest_Deaths_Date1;
    procedure CaseCountTest_Deaths_Date2;
    procedure CaseCountTest_Deaths_Date3;
    procedure CaseCountTest_Recovered_Date1;
    procedure CaseCountTest_Recovered_Date2;
    procedure CaseCountTest_Recovered_Date3;
    procedure CaseCountTest_Sick_Date1;
    procedure CaseCountTest_Sick_Date2;
    procedure CaseCountTest_Sick_Date3;
  end;

  TNormalizedCumulativeCasesTests = class(TCoronaTestCase)
  private
    procedure CaseCountTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure CaseCountTest_Confirmed_Date1;
    procedure CaseCountTest_Confirmed_Date2;
    procedure CaseCountTest_Confirmed_Date3;
    procedure CaseCountTest_Deaths_Date1;
    procedure CaseCountTest_Deaths_Date2;
    procedure CaseCountTest_Deaths_Date3;
    procedure CaseCountTest_Recovered_Date1;
    procedure CaseCountTest_Recovered_Date2;
    procedure CaseCountTest_Recovered_Date3;
    procedure CaseCountTest_Sick_Date1;
    procedure CaseCountTest_Sick_Date2;
    procedure CaseCountTest_Sick_Date3;
  end;

  TNormalizedNewCasesTests = class(TCoronaTestCase)
  private
    procedure CaseCountTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure CaseCountTest_Confirmed_Date1;
    procedure CaseCountTest_Confirmed_Date2;
    procedure CaseCountTest_Confirmed_Date3;
    procedure CaseCountTest_Deaths_Date1;
    procedure CaseCountTest_Deaths_Date2;
    procedure CaseCountTest_Deaths_Date3;
    procedure CaseCountTest_Recovered_Date1;
    procedure CaseCountTest_Recovered_Date2;
    procedure CaseCountTest_Recovered_Date3;
    procedure CaseCountTest_Sick_Date1;
    procedure CaseCountTest_Sick_Date2;
    procedure CaseCountTest_Sick_Date3;
  end;


implementation

uses
  DateUtils;

const
  TEST_POPULATION = 83783945;


constructor TCoronaTestCase.Create;
begin
  ReadTestData;
end;

procedure TCoronaTestCase.ReadTestData;
var
  L: TStringList;
  stream: TMemoryStream;
  sa: TStringArray;
  i, j: Integer;
begin
  L := TStringList.Create;
  try
    stream := TMemoryStream.Create;
    try
      stream.LoadFromFile('sample.csv');
      stream.Position := 0;
      L.LoadFromStream(stream);
    finally
      stream.Free;
    end;
    SetLength(FTestData, L.Count-1);
    for i := 1 to L.Count-1 do
    begin
      j := i-1;
      sa := L[i].Split(#9);
      FTestData[j].Date := ScanDateTime(SAVE_DATE_FORMAT, sa[0]);
      FTestData[j].Confirmed := StrToInt64(sa[1]);
      FTestData[j].Deaths := StrToInt64(sa[2]);
      FTestData[j].Recovered := StrToInt64(sa[3]);
    end;
  finally
    L.Free;
  end;
end;

procedure TCoronaTestCase.TestDataToDataItem;
var
  i: Integer;
  cases: TCaseArray;
begin
  SetLength(cases, Length(FTestData));

  for i := 0 to High(cases) do
    cases[i] := FTestData[i].Confirmed;
  FDataItem.SetCases(FTestData[0].Date, cases, pctConfirmed);

  for i := 0 to High(cases) do
    cases[i] := FTestData[i].Deaths;
  FDataItem.SetCases(FTestData[0].Date, cases, pctDeaths);

  for i := 0 to High(cases) do
    cases[i] := FTestData[i].Recovered;
  FDataItem.SetCases(FTestData[0].Date, cases, pctRecovered);

  FDataItem.Population := TEST_POPULATION;
end;

procedure TCoronaTestCase.SetUp;
begin
  FDataItem := TcDataItem.Create;
  TestDataToDataItem;
end;

procedure TCoronaTestCase.TearDown;
begin
  FDataItem.Free;
end;


{------------------------------------------------------------------------------}
{                             TDateTests                                       }
{------------------------------------------------------------------------------}
const
  SOLL_START_DATE = '2020-01-23';
  SOLL_END_DATE = '2021-02-15';
  SOLL_SOME_DATE = '2021-01-01';

procedure TDateTests.StartDateTest;
var
  idx: Integer;
  dSoll: TDate;
  dAct: TDate;
begin
  dSoll := ScanDateTime(SAVE_DATE_FORMAT, SOLL_START_DATE);
  idx := FDataItem.GetDateIndex(dSoll);
  dAct := FDataItem.Date[idx];
  AssertEquals('DataItem date mismatch ('+SOLL_START_DATE+')', dSoll, dAct);
  dAct := FTestData[idx].Date;
  AssertEquals('TestData date mismatch ('+SOLL_START_DATE+')', dSoll, dAct);
end;

procedure TDateTests.EndDateTest;
var
  idx: Integer;
  dSoll: TDate;
  dAct: TDate;
begin
  dSoll := ScanDateTime(SAVE_DATE_FORMAT, SOLL_END_DATE);
  idx := FDataItem.GetDateIndex(dSoll);
  dAct := FDataItem.Date[idx];
  AssertEquals('DataItem date mismatch ('+SOLL_END_DATE+')', dSoll, dAct);
  dAct := FTestData[idx].Date;
  AssertEquals('TestData date mismatch ('+SOLL_END_DATE+')', dSoll, dAct);
end;

procedure TDateTests.SomeDateTest;
var
  idx: Integer;
  dSoll: TDate;
  dAct: TDate;
begin
  dSoll := ScanDateTime(SAVE_DATE_FORMAT, SOLL_SOME_DATE);
  idx := FDataItem.GetDateIndex(dSoll);
  dAct := FDataItem.Date[idx];
  AssertEquals('DataItem date mismatch ('+SOLL_SOME_DATE+')', dSoll, dAct);
  dAct := FTestData[idx].Date;
  AssertEquals('TestData date mismatch ('+SOLL_SOME_DATE+')', dSoll, dAct);
end;


{------------------------------------------------------------------------------}
{                         Cumulative case tests                                }
{------------------------------------------------------------------------------}
const
  CUMULATIVE_TEST_DATES: Array[0..2] of String      = ('2020-03-01', '2021-01-01',  '2021-02-15');
  SOLL_CUMULATIVE_TOTAL: Array[0..2] of Integer     = (         130,     1762637,        2346876);
  SOLL_CUMULATIVE_DEATHS: Array[0..2] of Integer    = (           0,       34145,          65288);
  SOLL_CUMULATIVE_RECOVERED: Array[0..2] of Integer = (          16,     1388744,        2148269);
  SOLL_CUMULATIVE_SICK: Array[0..2] of Integer      = (         114,      339748,         133319);

procedure TCumulativeCasesTests.CaseCountTest(ATestIndex: Integer;
  ACaseType: TCaseType);
var
  idx: Integer;
begin
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, CUMULATIVE_TEST_DATES[ATestIndex]));

  case ACaseType of
    ctConfirmed:
      AssertEquals('DataItem Cumulative confirmed cases mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_TOTAL[ATestIndex], FDataItem.GetDataArray(ctConfirmed, dtCumulative)[idx]);

    ctDeaths:
      AssertEquals('DataItem Cumulative deaths mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_DEATHS[ATestIndex], FDataItem.GetDataArray(ctDeaths, dtCumulative)[idx]);

    ctRecovered:
      AssertEquals('DataItem Cumulative recovered cases mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_RECOVERED[ATestIndex], FDataItem.GetDataArray(ctRecovered, dtCumulative)[idx]);

    ctSick:
      AssertEquals('DataItem Cumulative recovered cases mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_SICK[ATestIndex], FDataItem.GetDataArray(ctSick, dtCumulative)[idx]);
  end;
end;

procedure TCumulativeCasesTests.CaseCountTest_Confirmed_Date1;
begin
  CaseCountTest(0, ctConfirmed);
end;
procedure TCumulativeCasesTests.CaseCountTest_Confirmed_Date2;
begin
  caseCountTest(1, ctConfirmed);
end;
procedure TCumulativeCasesTests.CaseCountTest_Confirmed_Date3;
begin
  CaseCountTest(2, ctConfirmed);
end;

procedure TCumulativeCasesTests.CaseCountTest_Deaths_Date1;
begin
  CaseCountTest(0, ctDeaths);
end;
procedure TCumulativeCasesTests.CaseCountTest_Deaths_Date2;
begin
  caseCountTest(1, ctDeaths);
end;
procedure TCumulativeCasesTests.CaseCountTest_Deaths_Date3;
begin
  CaseCountTest(2, ctDeaths);
end;

procedure TCumulativeCasesTests.CaseCountTest_Recovered_Date1;
begin
  CaseCountTest(0, ctRecovered);
end;
procedure TCumulativeCasesTests.CaseCountTest_Recovered_Date2;
begin
  caseCountTest(1, ctRecovered);
end;
procedure TCumulativeCasesTests.CaseCountTest_Recovered_Date3;
begin
  CaseCountTest(2, ctRecovered);
end;

procedure TCumulativeCasesTests.CaseCountTest_Sick_Date1;
begin
  CaseCountTest(0, ctSick);
end;
procedure TCumulativeCasesTests.CaseCountTest_Sick_Date2;
begin
  caseCountTest(1, ctSick);
end;
procedure TCumulativeCasesTests.CaseCountTest_Sick_Date3;
begin
  CaseCountTest(2, ctSick);
end;

{------------------------------------------------------------------------------}
{                             New cases tests                                  }
{------------------------------------------------------------------------------}
const
  // Differences calculated by Excel
  NEW_TEST_DATES: Array[0..2] of String      = ('2020-03-01', '2021-01-01', '2021-02-15');
  SOLL_NEW_TOTAL: Array[0..2] of Integer     = (          51,         2117,         5132);
  SOLL_NEW_DEATHS: Array[0..2] of Integer    = (           0,          354,          181);
  SOLL_NEW_RECOVERED: Array[0..2] of Integer = (           0,        38036,        11336);
  SOLL_NEW_SICK: Array[0..2] of Integer      = (          51,       -36273,        -6385);

procedure TNewCasesTests.CaseCountTest(ATestIndex: Integer; ACaseType: TCaseType);
var
  idx: Integer;
begin
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, NEW_TEST_DATES[ATestIndex]));

  case ACaseType of
    ctConfirmed:
      AssertEquals('DataItem new confirmed cases mismatch on ' + NEW_TEST_DATES[ATestIndex],
        SOLL_NEW_TOTAL[ATestIndex], FDataItem.GetDataArray(ctConfirmed, dtNewCases)[idx]);

    ctDeaths:
      AssertEquals('DataItem new deaths mismatch on ' + NEW_TEST_DATES[ATestIndex],
        SOLL_NEW_DEATHS[ATestIndex], FDataItem.GetDataArray(ctDeaths, dtNewCases)[idx]);

    ctRecovered:
      AssertEquals('DataItem new recovered cases mismatch on ' + NEW_TEST_DATES[ATestIndex],
        SOLL_NEW_RECOVERED[ATestIndex], FDataItem.GetDataArray(ctRecovered, dtNewCases)[idx]);

    ctSick:
      AssertEquals('DataItem new sick cases mismatch on ' + NEW_TEST_DATES[ATestIndex],
        SOLL_NEW_SICK[ATestIndex], FDataItem.GetDataArray(ctSick, dtNewCases)[idx]);
  end;
end;

procedure TNewCasesTests.CaseCountTest_Confirmed_Date1;
begin
  CaseCountTest(0, ctConfirmed);
end;
procedure TNewCasesTests.CaseCountTest_Confirmed_Date2;
begin
  CaseCountTest(1, ctConfirmed);
end;
procedure TNewCasesTests.CaseCountTest_Confirmed_Date3;
begin
  CaseCountTest(2, ctConfirmed);
end;

procedure TNewCasesTests.CaseCountTest_Deaths_Date1;
begin
  caseCountTest(0, ctDeaths);
end;
procedure TNewCasesTests.CaseCountTest_Deaths_Date2;
begin
  caseCountTest(1, ctDeaths);
end;
procedure TNewCasesTests.CaseCountTest_Deaths_Date3;
begin
  caseCountTest(2, ctDeaths);
end;

procedure TNewCasesTests.CaseCountTest_Recovered_Date1;
begin
  CaseCountTest(0, ctRecovered);
end;
procedure TNewCasesTests.CaseCountTest_Recovered_Date2;
begin
  CaseCountTest(1, ctRecovered);
end;
procedure TNewCasesTests.CaseCountTest_Recovered_Date3;
begin
  CaseCountTest(2, ctRecovered);
end;

procedure TNewCasesTests.CaseCountTest_Sick_Date1;
begin
  CaseCountTest(0, ctSick);
end;
procedure TNewCasesTests.CaseCountTest_Sick_Date2;
begin
  CaseCountTest(1, ctSick);
end;
procedure TNewCasesTests.CaseCountTest_Sick_Date3;
begin
  CaseCountTest(2, ctSick);
end;

{------------------------------------------------------------------------------}

procedure TNormalizedCumulativeCasesTests.CaseCountTest(ATestIndex: Integer;
  ACaseType: TCaseType);
var
  idx: Integer;
  data: TValueArray;
  f: Double;
begin
  f := REF_POPULATION / TEST_POPULATION;
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, CUMULATIVE_TEST_DATES[ATestIndex]));

  //data := FDataItem.GetDataArray(ctConfirmed, dtNormalizedCumulative);

  case ACaseType of
    ctConfirmed:
      AssertEquals('DataItem normalized cumulative confirmed cases mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_TOTAL[ATestIndex]*f, FDataItem.GetDataArray(ctConfirmed, dtNormalizedCumulative)[idx]);

    ctDeaths:
      AssertEquals('DataItem normalized cumulative deaths mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_DEATHS[ATestIndex]*f, FDataItem.GetDataArray(ctDeaths, dtNormalizedCumulative)[idx]);

    ctRecovered:
      AssertEquals('DataItem normalized cumulative recovered cases mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_RECOVERED[ATestIndex]*f, FDataItem.GetDataArray(ctRecovered, dtNormalizedCumulative)[idx]);

    ctSick:
      AssertEquals('DataItem normalized cumulative sick cases mismatch on ' + CUMULATIVE_TEST_DATES[ATestIndex],
        SOLL_CUMULATIVE_SICK[ATestIndex]*f, FDataItem.GetDataArray(ctSick, dtNormalizedCumulative)[idx]);
  end;
end;

procedure TNormalizedCumulativeCasesTests.CaseCountTest_Confirmed_Date1;
begin
  CaseCountTest(0, ctConfirmed);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Confirmed_Date2;
begin
  CaseCountTest(1, ctConfirmed);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Confirmed_Date3;
begin
  CaseCountTest(2, ctConfirmed);
end;

procedure TNormalizedCumulativeCasesTests.CaseCountTest_Deaths_Date1;
begin
  caseCountTest(0, ctDeaths);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Deaths_Date2;
begin
  caseCountTest(1, ctDeaths);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Deaths_Date3;
begin
  caseCountTest(2, ctDeaths);
end;

procedure TNormalizedCumulativeCasesTests.CaseCountTest_Recovered_Date1;
begin
  CaseCountTest(0, ctRecovered);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Recovered_Date2;
begin
  CaseCountTest(1, ctRecovered);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Recovered_Date3;
begin
  CaseCountTest(2, ctRecovered);
end;

procedure TNormalizedCumulativeCasesTests.CaseCountTest_Sick_Date1;
begin
  CaseCountTest(0, ctSick);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Sick_Date2;
begin
  CaseCountTest(1, ctSick);
end;
procedure TNormalizedCumulativeCasesTests.CaseCountTest_Sick_Date3;
begin
  CaseCountTest(2, ctSick);
end;

{------------------------------------------------------------------------------}

const
  NORMALIZED_NEW_TEST_DATES: Array[0..2] of String      = ('2020-03-01', '2021-01-01', '2021-02-15');
  // Weeky sums of new cases are calculated by Excel
  SOLL_NORMALIZED_NEW_TOTAL: Array[0..2] of Integer     = (114, 129901, 50553);
  SOLL_NORMALIZED_NEW_DEATHS: Array[0..2] of Integer    = (  0,   4565,  3097);
  SOLL_NORMALIZED_NEW_RECOVERED: Array[0..2] of Integer = (  2, 152776, 93929);
  SOLL_NORMALIZED_NEW_SICK: Array[0..2] of Integer      = (112, -27440, -46473);

procedure TNormalizedNewCasesTests.CaseCountTest(ATestIndex: Integer;
  ACaseType: TCaseType);
var
  idx: Integer;
//  data: TValueArray;
  f: Double;
begin
  f := REF_POPULATION / TEST_POPULATION;
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, NORMALIZED_NEW_TEST_DATES[ATestIndex]));

  //data := FDataItem.GetDataArray(ctConfirmed, dtNormalizedNewCases);

  case ACaseType of
    ctConfirmed:
      AssertEquals('DataItem normalized cumulative confirmed cases mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_TOTAL[ATestIndex]*f, FDataItem.GetDataArray(ctConfirmed, dtNormalizedNewCases)[idx]);

    ctDeaths:
      AssertEquals('DataItem normalized cumulative deaths mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_DEATHS[ATestIndex]*f, FDataItem.GetDataArray(ctDeaths, dtNormalizedNewCases)[idx]);

    ctRecovered:
      AssertEquals('DataItem normalized cumulative recovered cases mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_RECOVERED[ATestIndex]*f, FDataItem.GetDataArray(ctRecovered, dtNormalizedNewCases)[idx]);

    ctSick:
      AssertEquals('DataItem normalized cumulative sick cases mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_SICK[ATestIndex]*f, FDataItem.GetDataArray(ctSick, dtNormalizedNewCases)[idx]);
  end;
end;

procedure TNormalizedNewCasesTests.CaseCountTest_Confirmed_Date1;
begin
  CaseCountTest(0, ctConfirmed);
end;

procedure TNormalizedNewCasesTests.CaseCountTest_Confirmed_Date2;
begin
  CaseCountTest(1, ctConfirmed);
end;

procedure TNormalizedNewCasesTests.CaseCountTest_Confirmed_Date3;
begin
  CaseCountTest(2, ctConfirmed);
end;

procedure TNormalizedNewCasesTests.CaseCountTest_Deaths_Date1;
begin
  caseCountTest(0, ctDeaths);
end;
procedure TNormalizedNewCasesTests.CaseCountTest_Deaths_Date2;
begin
  caseCountTest(1, ctDeaths);
end;
procedure TNormalizedNewCasesTests.CaseCountTest_Deaths_Date3;
begin
  caseCountTest(2, ctDeaths);
end;

procedure TNormalizedNewCasesTests.CaseCountTest_Recovered_Date1;
begin
  CaseCountTest(0, ctRecovered);
end;
procedure TNormalizedNewCasesTests.CaseCountTest_Recovered_Date2;
begin
  CaseCountTest(1, ctRecovered);
end;
procedure TNormalizedNewCasesTests.CaseCountTest_Recovered_Date3;
begin
  CaseCountTest(2, ctRecovered);
end;

procedure TNormalizedNewCasesTests.CaseCountTest_Sick_Date1;
begin
  CaseCountTest(0, ctSick);
end;
procedure TNormalizedNewCasesTests.CaseCountTest_Sick_Date2;
begin
  CaseCountTest(1, ctSick);
end;
procedure TNormalizedNewCasesTests.CaseCountTest_Sick_Date3;
begin
  CaseCountTest(2, ctSick);
end;


initialization
  RegisterTest(TDateTests);
  RegisterTest(TCumulativeCasesTests);
  RegisterTest(TNewCasesTests);
  RegisterTest(TNormalizedCumulativeCasesTests);
  RegisterTest(TNormalizedNewCasesTests);

end.

