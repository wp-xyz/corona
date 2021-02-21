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
    procedure GetDataArrayTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure GetDataArrayTest_Confirmed_Date1;
    procedure GetDataArrayTest_Confirmed_Date2;
    procedure GetDataArrayTest_Confirmed_Date3;
    procedure GetDataArrayTest_Deaths_Date1;
    procedure GetDataArrayTest_Deaths_Date2;
    procedure GetDataArrayTest_Deaths_Date3;
    procedure GetDataArrayTest_Recovered_Date1;
    procedure GetDataArrayTest_Recovered_Date2;
    procedure GetDataArrayTest_Recovered_Date3;
    procedure GetDataArrayTest_Sick_Date1;
    procedure GetDataArrayTest_Sick_Date2;
    procedure GetDataArrayTest_Sick_Date3;
  end;

  TNewCasesTests = class(TCoronaTestCase)
  private
    procedure GetDataArrayTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure GetDataArrayTest_Confirmed_Date1;
    procedure GetDataArrayTest_Confirmed_Date2;
    procedure GetDataArrayTest_Confirmed_Date3;
    procedure GetDataArrayTest_Deaths_Date1;
    procedure GetDataArrayTest_Deaths_Date2;
    procedure GetDataArrayTest_Deaths_Date3;
    procedure GetDataArrayTest_Recovered_Date1;
    procedure GetDataArrayTest_Recovered_Date2;
    procedure GetDataArrayTest_Recovered_Date3;
    procedure GetDataArrayTest_Sick_Date1;
    procedure GetDataArrayTest_Sick_Date2;
    procedure GetDataArrayTest_Sick_Date3;
  end;

  TNormalizedCumulativeCasesTests = class(TCoronaTestCase)
  private
    procedure GetDataArrayTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure GetDataArrayTest_Confirmed_Date1;
    procedure GetDataArrayTest_Confirmed_Date2;
    procedure GetDataArrayTest_Confirmed_Date3;
    procedure GetDataArrayTest_Deaths_Date1;
    procedure GetDataArrayTest_Deaths_Date2;
    procedure GetDataArrayTest_Deaths_Date3;
    procedure GetDataArrayTest_Recovered_Date1;
    procedure GetDataArrayTest_Recovered_Date2;
    procedure GetDataArrayTest_Recovered_Date3;
    procedure GetDataArrayTest_Sick_Date1;
    procedure GetDataArrayTest_Sick_Date2;
    procedure GetDataArrayTest_Sick_Date3;
  end;

  TNormalizedNewCasesTests = class(TCoronaTestCase)
  private
    procedure CalcNormalizedNewCasesTest(ATestIndex: Integer; ACaseType: TCaseType);
    procedure GetDataArrayTest(ATestIndex: Integer; ACaseType: TCaseType);
  published
    procedure CalcNormalizedNewCases_Confirmed_Date1;
    procedure CalcNormalizedNewCases_Confirmed_Date2;
    procedure CalcNormalizedNewCases_Confirmed_Date3;
    procedure CalcNormalizedNewCases_Deaths_Date1;
    procedure CalcNormalizedNewCases_Deaths_Date2;
    procedure CalcNormalizedNewCases_Deaths_Date3;
    procedure CalcNormalizedNewCases_Recovered_Date1;
    procedure CalcNormalizedNewCases_Recovered_Date2;
    procedure CalcNormalizedNewCases_Recovered_Date3;
    procedure CalcNormalizedNewCases_Sick_Date1;
    procedure CalcNormalizedNewCases_Sick_Date2;
    procedure CalcNormalizedNewCases_Sick_Date3;

    procedure GetDataArrayTest_Confirmed_Date1;
    procedure GetDataArrayTest_Confirmed_Date2;
    procedure GetDataArrayTest_Confirmed_Date3;
    procedure GetDataArrayTest_Deaths_Date1;
    procedure GetDataArrayTest_Deaths_Date2;
    procedure GetDataArrayTest_Deaths_Date3;
    procedure GetDataArrayTest_Recovered_Date1;
    procedure GetDataArrayTest_Recovered_Date2;
    procedure GetDataArrayTest_Recovered_Date3;
    procedure GetDataArrayTest_Sick_Date1;
    procedure GetDataArrayTest_Sick_Date2;
    procedure GetDataArrayTest_Sick_Date3;
  end;

  TDoublingTimeTests = class(TCoronaTestCase)
  private
    procedure GetDataArrayTest(ATestIndex: Integer; ADataType: TDataType);
  published
    procedure GetDataArrayTest_Cumulative_Date1;
    procedure GetDataArrayTest_Cumulative_Date2;
    procedure GetDataArrayTest_Cumulative_Date3;
    procedure GetDataArrayTest_NewCases_Date1;
    procedure GetDataArrayTest_NewCases_Date2;
{
    procedure NewCasesDoublingTime_Date3;
    }
  end;

  TRValueTests = class(TCoronaTestCase)
  private
    procedure GetDataArrayTest(ATestIndex: Integer);
    procedure CalcRValueTest(ATestIndex: Integer);
  published
    procedure GetDataArrayTest_Date1;
    procedure GetDataArrayTest_Date2;
    procedure GetDataArrayTest_Date3;
    procedure CalcRValueTest_Date1;
    procedure CalcRValueTest_Date2;
    procedure CalcRValueTest_Date3;
  end;


implementation

uses
  DateUtils, Math;

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

procedure TCumulativeCasesTests.GetDataArrayTest(ATestIndex: Integer;
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

procedure TCumulativeCasesTests.GetDataArrayTest_Confirmed_Date1;
begin
  GetDataArrayTest(0, ctConfirmed);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Confirmed_Date2;
begin
  GetDataArrayTest(1, ctConfirmed);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Confirmed_Date3;
begin
  GetDataArrayTest(2, ctConfirmed);
end;

procedure TCumulativeCasesTests.GetDataArrayTest_Deaths_Date1;
begin
  GetDataArrayTest(0, ctDeaths);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Deaths_Date2;
begin
  GetDataArrayTest(1, ctDeaths);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Deaths_Date3;
begin
  GetDataArrayTest(2, ctDeaths);
end;

procedure TCumulativeCasesTests.GetDataArrayTest_Recovered_Date1;
begin
  GetDataArrayTest(0, ctRecovered);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Recovered_Date2;
begin
  GetDataArrayTest(1, ctRecovered);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Recovered_Date3;
begin
  GetDataArrayTest(2, ctRecovered);
end;

procedure TCumulativeCasesTests.GetDataArrayTest_Sick_Date1;
begin
  GetDataArrayTest(0, ctSick);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Sick_Date2;
begin
  GetDataArrayTest(1, ctSick);
end;
procedure TCumulativeCasesTests.GetDataArrayTest_Sick_Date3;
begin
  GetDataArrayTest(2, ctSick);
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

procedure TNewCasesTests.GetDataArrayTest(ATestIndex: Integer; ACaseType: TCaseType);
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

procedure TNewCasesTests.GetDataArrayTest_Confirmed_Date1;
begin
  GetDataArrayTest(0, ctConfirmed);
end;
procedure TNewCasesTests.GetDataArrayTest_Confirmed_Date2;
begin
  GetDataArrayTest(1, ctConfirmed);
end;
procedure TNewCasesTests.GetDataArrayTest_Confirmed_Date3;
begin
  GetDataArrayTest(2, ctConfirmed);
end;

procedure TNewCasesTests.GetDataArrayTest_Deaths_Date1;
begin
  GetDataArrayTest(0, ctDeaths);
end;
procedure TNewCasesTests.GetDataArrayTest_Deaths_Date2;
begin
  GetDataArrayTest(1, ctDeaths);
end;
procedure TNewCasesTests.GetDataArrayTest_Deaths_Date3;
begin
  GetDataArrayTest(2, ctDeaths);
end;

procedure TNewCasesTests.GetDataArrayTest_Recovered_Date1;
begin
  GetDataArrayTest(0, ctRecovered);
end;
procedure TNewCasesTests.GetDataArrayTest_Recovered_Date2;
begin
  GetDataArrayTest(1, ctRecovered);
end;
procedure TNewCasesTests.GetDataArrayTest_Recovered_Date3;
begin
  GetDataArrayTest(2, ctRecovered);
end;

procedure TNewCasesTests.GetDataArrayTest_Sick_Date1;
begin
  GetDataArrayTest(0, ctSick);
end;
procedure TNewCasesTests.GetDataArrayTest_Sick_Date2;
begin
  GetDataArrayTest(1, ctSick);
end;
procedure TNewCasesTests.GetDataArrayTest_Sick_Date3;
begin
  GetDataArrayTest(2, ctSick);
end;

{------------------------------------------------------------------------------}

procedure TNormalizedCumulativeCasesTests.GetDataArrayTest(ATestIndex: Integer;
  ACaseType: TCaseType);
var
  idx: Integer;
  f: Double;
begin
  f := REF_POPULATION / TEST_POPULATION;
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, CUMULATIVE_TEST_DATES[ATestIndex]));

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

procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Confirmed_Date1;
begin
  GetDataArrayTest(0, ctConfirmed);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Confirmed_Date2;
begin
  GetDataArrayTest(1, ctConfirmed);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Confirmed_Date3;
begin
  GetDataArrayTest(2, ctConfirmed);
end;

procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Deaths_Date1;
begin
  GetDataArrayTest(0, ctDeaths);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Deaths_Date2;
begin
  GetDataArrayTest(1, ctDeaths);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Deaths_Date3;
begin
  GetDataArrayTest(2, ctDeaths);
end;

procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Recovered_Date1;
begin
  GetDataArrayTest(0, ctRecovered);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Recovered_Date2;
begin
  GetDataArrayTest(1, ctRecovered);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Recovered_Date3;
begin
  GetDataArrayTest(2, ctRecovered);
end;

procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Sick_Date1;
begin
  GetDataArrayTest(0, ctSick);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Sick_Date2;
begin
  GetDataArrayTest(1, ctSick);
end;
procedure TNormalizedCumulativeCasesTests.GetDataArrayTest_Sick_Date3;
begin
  GetDataArrayTest(2, ctSick);
end;

{------------------------------------------------------------------------------}

const
  NORMALIZED_NEW_TEST_DATES: Array[0..2] of String      = ('2020-03-01', '2021-01-01', '2021-02-15');
  // Weeky sums of new cases are calculated by Excel
  SOLL_NORMALIZED_NEW_TOTAL: Array[0..2] of Integer     = (114, 129901, 50553);
  SOLL_NORMALIZED_NEW_DEATHS: Array[0..2] of Integer    = (  0,   4565,  3097);
  SOLL_NORMALIZED_NEW_RECOVERED: Array[0..2] of Integer = (  2, 152776, 93929);
  SOLL_NORMALIZED_NEW_SICK: Array[0..2] of Integer      = (112, -27440, -46473);


procedure TNormalizedNewCasesTests.CalcNormalizedNewCasesTest(ATestIndex: Integer;
  ACaseType: TCaseType);
var
  idx: Integer;
  expCount, actCount: Double;
  f: Double;
begin
  f := REF_POPULATION / TEST_POPULATION;
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, NORMALIZED_NEW_TEST_DATES[ATestIndex]));

  actCount := FDataItem.CalcNormalizedNewCases(idx, ACaseType);
  case ACaseType of
    ctConfirmed:  expCount := SOLL_NORMALIZED_NEW_TOTAL[ATestIndex]*f;
    ctDeaths: expCount := SOLL_NORMALIZED_NEW_DEATHS[ATestIndex]*f;
    ctRecovered: expCount := SOLL_NORMALIZED_NEW_RECOVERED[ATestIndex]*f;
    ctSick: expCount :=SOLL_NORMALIZED_NEW_SICK[ATestIndex]*f;
  end;

  AssertEquals(Format(
    'DataItem normalized new %s mismatch on %s',
      [LONG_CASETYPE_NAMES[ACaseType], NORMALIZED_NEW_TEST_DATES[ATestIndex]]),
    expCount,
    actCount
  );
end;

procedure TNormalizedNewCasesTests.GetDataArrayTest(ATestIndex: Integer;
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
      AssertEquals('DataItem normalized new confirmed cases mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_TOTAL[ATestIndex]*f, FDataItem.GetDataArray(ctConfirmed, dtNormalizedNewCases)[idx]);

    ctDeaths:
      AssertEquals('DataItem normalized new deaths mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_DEATHS[ATestIndex]*f, FDataItem.GetDataArray(ctDeaths, dtNormalizedNewCases)[idx]);

    ctRecovered:
      AssertEquals('DataItem normalized new recovered cases mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_RECOVERED[ATestIndex]*f, FDataItem.GetDataArray(ctRecovered, dtNormalizedNewCases)[idx]);

    ctSick:
      AssertEquals('DataItem normalized new sick cases mismatch on ' + NORMALIZED_NEW_TEST_DATES[ATestIndex],
        SOLL_NORMALIZED_NEW_SICK[ATestIndex]*f, FDataItem.GetDataArray(ctSick, dtNormalizedNewCases)[idx]);
  end;
end;

procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Confirmed_Date1;
begin
  CalcNormalizedNewCasesTest(0, ctConfirmed);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Confirmed_Date2;
begin
  CalcNormalizedNewCasesTest(1, ctConfirmed);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Confirmed_Date3;
begin
  CalcNormalizedNewCasesTest(2, ctConfirmed);
end;

procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Deaths_Date1;
begin
  CalcNormalizedNewCasesTest(0, ctDeaths);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Deaths_Date2;
begin
  CalcNormalizedNewCasesTest(1, ctDeaths);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Deaths_Date3;
begin
  CalcNormalizedNewCasesTest(2, ctDeaths);
end;

procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Recovered_Date1;
begin
  CalcNormalizedNewCasesTest(0, ctRecovered);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Recovered_Date2;
begin
  CalcNormalizedNewCasesTest(1, ctRecovered);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Recovered_Date3;
begin
  CalcNormalizedNewCasesTest(2, ctRecovered);
end;

procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Sick_Date1;
begin
  CalcNormalizedNewCasesTest(0, ctSick);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Sick_Date2;
begin
  CalcNormalizedNewCasesTest(1, ctSick);
end;
procedure TNormalizedNewCasesTests.CalcNormalizedNewCases_Sick_Date3;
begin
  CalcNormalizedNewCasesTest(2, ctSick);
end;


procedure TNormalizedNewCasesTests.GetDataArrayTest_Confirmed_Date1;
begin
  GetDataArrayTest(0, ctConfirmed);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Confirmed_Date2;
begin
  GetDataArrayTest(1, ctConfirmed);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Confirmed_Date3;
begin
  GetDataArrayTest(2, ctConfirmed);
end;

procedure TNormalizedNewCasesTests.GetDataArrayTest_Deaths_Date1;
begin
  GetDataArrayTest(0, ctDeaths);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Deaths_Date2;
begin
  GetDataArrayTest(1, ctDeaths);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Deaths_Date3;
begin
  GetDataArrayTest(2, ctDeaths);
end;

procedure TNormalizedNewCasesTests.GetDataArrayTest_Recovered_Date1;
begin
  GetDataArrayTest(0, ctRecovered);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Recovered_Date2;
begin
  GetDataArrayTest(1, ctRecovered);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Recovered_Date3;
begin
  GetDataArrayTest(2, ctRecovered);
end;

procedure TNormalizedNewCasesTests.GetDataArrayTest_Sick_Date1;
begin
  GetDataArrayTest(0, ctSick);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Sick_Date2;
begin
  GetDataArrayTest(1, ctSick);
end;
procedure TNormalizedNewCasesTests.GetDataArrayTest_Sick_Date3;
begin
  GetDataArrayTest(2, ctSick);
end;


{------------------------------------------------------------------------------}
{                      TDoublingTimeTest                                       }
{------------------------------------------------------------------------------}
const
  // Doubling times determined by using Excel
  DOUBLINGTIME_CUMULATIVE_TEST_DATES: Array[0..2] of String  = ('2020-03-01', '2021-01-01', '2021-02-15');
  SOLL_DOUBLINGTIME_CUMULATIVE_CONFIRMED: Array[0..2] of Double = (NaN, 41.76701856, 69.58856461);

  DOUBLINGTIME_NEW_TEST_DATES: array[0..1] of String     = ('2020-10-01', '2021-02-01');
  SOLL_DOUBLINGTIME_NEW_CONFIRMED: Array[0..1] of Double = ( 49.29334677,          NaN);
                                                     //      growing        shrinking

procedure TDoublingTimeTests.GetDataArrayTest(ATestIndex: Integer;
  ADataType: TDataType);
const
  EPS = 1E-4;
var
  idx: Integer;
  actT2, expT2: Double;
  sct, sdt, sd: String;
begin
  if not (ADataType in [dtCumulativeCasesDoublingTime, dtNewCasesDoublingTime]) then
    fail('DataType not supposed to be tested here.');

  case ADataType of
    dtCumulativeCasesDoublingTime:
      begin
        sd := DOUBLINGTIME_CUMULATIVE_TEST_DATES[ATestIndex];
        idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, sd));
        actT2 := FDataItem.GetDataArray(ctConfirmed, ADataType)[idx];
        expT2 := SOLL_DOUBLINGTIME_CUMULATIVE_CONFIRMED[ATestIndex];
        sct := 'confirmed';
        sdt := 'cumulative';
      end;
    dtNewCasesDoublingTime:
      begin
        sd := DOUBLINGTIME_NEW_TEST_DATES[ATestIndex];
        idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, sd));
        actT2 := FDataItem.GetDataArray(ctConfirmed, ADataType)[idx];
        expT2 := SOLL_DOUBLINGTIME_NEW_CONFIRMED[ATestIndex];
        sct := 'confirmed';
        sdt := 'new';
      end;
  end;

  if IsNaN(expT2) xor IsNaN(actT2) then
    // Only one of the doubling times is NaN  --> error
    AssertEquals(Format(
      'Doubling time of %s %s cases mismatch on %s', [sct, sdt, sd]),
      false, IsNaN(expT2) xor IsNaN(actT2)
    )
  else
  if not (IsNaN(expT2) and IsNaN(actT2)) then
    AssertEquals(Format(
      'Doubling time of %s %s cases mismatch on %s', [sct, sdt, sd]),
      expT2, actT2, EPS
    );
end;

procedure TDoublingTimeTests.GetDataArrayTest_Cumulative_Date1;
begin
  GetDataArrayTest(0, dtCumulativeCasesDoublingTime);
end;
procedure TDoublingTimeTests.GetDataArrayTest_Cumulative_Date2;
begin
  GetDataArrayTest(1, dtCumulativeCasesDoublingTime);
end;
procedure TDoublingTimeTests.GetDataArrayTest_Cumulative_Date3;
begin
  GetDataArrayTest(2, dtCumulativeCasesDoublingTime);
end;

procedure TDoublingTimeTests.GetDataArrayTest_NewCases_Date1;
begin
  GetDataArrayTest(0, dtNewCasesDoublingTime);
end;
procedure TDoublingTimeTests.GetDataArrayTest_NewCases_Date2;
begin
  GetDataArrayTest(1, dtNewCasesDoublingTime);
end;


{------------------------------------------------------------------------------}
{                            R value tests                                     }
{------------------------------------------------------------------------------}
const                                      //  too small     R > 1                  R < 1
  RVALUE_TEST_DATES: array[0..2] of String = ('2020-02-01',  '2020-10-01',          '2021-02-15');
  SOLL_RVALUE: Array[0..2] of Double       = (NaN,           2027.57143/1727.71429, 7221.85714/8399.0);
  SOLL_RVALUE_ERROR: array[0..2] of Double = (NaN,            0.02052205,        0.007370425);

procedure TRValueTests.GetDataArrayTest(ATestIndex: Integer);
const
  EPS = 1E-4;
var
  idx: Integer;
  actR, expR: Double;
begin
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, RVALUE_TEST_DATES[ATestIndex]));
  expR := SOLL_RVALUE[ATestIndex];
  actR := FDataItem.GetDataArray(ctConfirmed, dtRValue)[idx];

  if IsNaN(expR) xor IsNaN(actR) then
    // Only one of the R values is NaN --> error
    AssertEquals('R value mismatch on %s' + RVALUE_TEST_DATES[ATestIndex],
      false, IsNaN(expR) xor IsNaN(actR)
    )
  else
  if not (IsNaN(expR) and IsNaN(actR)) then
    // None of the R values is NaN
    AssertEquals('RValue mismatch on ' + RVALUE_TEST_DATES[ATestIndex],
      expR, actR, EPS);
    // In the else part both values are NaN which would mean "no error".
end;

procedure TRValueTests.GetDataArrayTest_Date1;
begin
  GetDataArrayTest(0);
end;
procedure TRValueTests.GetDataArrayTest_Date2;
begin
  GetDataArrayTest(1);
end;
procedure TRValueTests.GetDataArrayTest_Date3;
begin
  GetDataArrayTest(2);
end;

procedure TRValueTests.CalcRValueTest(ATestIndex: Integer);
const
  EPS = 1E-4;
var
  idx: Integer;
  actR, expR: Double;
  actRerr: Double;
  ok: Boolean;
begin
  idx := FDataItem.GetDateIndex(ScanDateTime(SAVE_DATE_FORMAT, RVALUE_TEST_DATES[ATestIndex]));
  expR := SOLL_RVALUE[ATestIndex];
  ok := FDataItem.CalcRValue(idx, actR, actRErr);

  if IsNaN(expR) xor IsNaN(actR) then
    // Only one of the R values is NaN --> error
    AssertEquals('R value mismatch on %s' + RVALUE_TEST_DATES[ATestIndex],
      false, IsNaN(expR) xor IsNaN(actR)
    )
  else
  if not (IsNaN(expR) and IsNaN(actR)) then
  begin
    // None of the R values is NaN
    AssertEquals('R value mismatch on ' + RVALUE_TEST_DATES[ATestIndex],
      expR, actR, EPS);
     AssertEquals('R value error mismatch on ' + RVALUE_TEST_DATES[ATestIndex],
       SOLL_RVALUE_ERROR[ATestIndex], actRErr, EPS);
  end;
  // In the else part both values are NaN which would mean "no error".
end;

procedure TRValueTests.CalcRValueTest_Date1;
begin
  CalcRValueTest(0);
end;
procedure TRValueTests.CalcRValueTest_Date2;
begin
  CalcRValueTest(1);
end;
procedure TRValueTests.CalcRValueTest_Date3;
begin
  CalcRValueTest(2);
end;

initialization
  RegisterTest(TDateTests);
  RegisterTest(TCumulativeCasesTests);
  RegisterTest(TNewCasesTests);
  RegisterTest(TNormalizedCumulativeCasesTests);
  RegisterTest(TNormalizedNewCasesTests);
  RegisterTest(TDoublingTimeTests);
  RegisterTest(TRValueTests);

end.

