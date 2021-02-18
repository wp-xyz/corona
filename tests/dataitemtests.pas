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

  TDoublingTimeTests = class(TCoronaTestCase)
  private
    procedure DoublingTimeTest(ATestIndex: Integer; ACaseType: TCaseType; ADataType: TDataType);
  published
    procedure CumulativeCasesDoublingTime_Date1;
    procedure CumulativeCasesDoublingTime_Date2;
    procedure CumulativeCasesDoublingTime_Date3;
    procedure NewCasesDoublingTime_Date1;
    procedure NewCasesDoublingTime_Date2;
{
    procedure NewCasesDoublingTime_Date3;
    }
  end;

  TRValueTests = class(TCoronaTestCase)
  private
    procedure RValueTest(ATestIndex: Integer);
    procedure CalcRValueTest(ATestIndex: Integer);
  published
    procedure RValue_Date1;
    procedure RValue_Date2;
    procedure RValue_Date3;
    procedure CalcRValue_Date1;
    procedure CalcRValue_Date2;
    procedure CalcRValue_Date3;
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

procedure TDoublingTimeTests.DoublingTimeTest(ATestIndex: Integer;
  ACaseType: TCaseType; ADataType: TDataType);
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
        actT2 := FDataItem.GetdataArray(ctConfirmed, ADataType)[idx];
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

procedure TDoublingTimeTests.CumulativeCasesDoublingTime_Date1;
begin
  DoublingTimeTest(0, ctConfirmed, dtCumulativeCasesDoublingTime);
end;
procedure TDoublingTimeTests.CumulativeCasesDoublingTime_Date2;
begin
  DoublingTimeTest(1, ctConfirmed, dtCumulativeCasesDoublingTime);
end;
procedure TDoublingTimeTests.CumulativeCasesDoublingTime_Date3;
begin
  DoublingTimeTest(2, ctConfirmed, dtCumulativeCasesDoublingTime);
end;

procedure TDoublingTimeTests.NewCasesDoublingTime_Date1;
begin
  DoublingTimeTest(0, ctConfirmed, dtNewCasesDoublingTime);
end;
procedure TDoublingTimeTests.NewCasesDoublingTime_Date2;
begin
  DoublingTimeTest(1, ctConfirmed, dtNewCasesDoublingTime);
end;


{------------------------------------------------------------------------------}
{                            R value tests                                     }
{------------------------------------------------------------------------------}
const                                      //  too small     R > 1                  R < 1
  RVALUE_TEST_DATES: array[0..2] of String = ('2020-02-01',  '2020-10-01',          '2021-02-15');
  SOLL_RVALUE: Array[0..2] of Double       = (NaN,           2027.57143/1727.71429, 7221.85714/8399.0);
  SOLL_RVALUE_ERROR: array[0..2] of Double = (NaN,            0.02052205,        0.007370425);

procedure TRValueTests.RValueTest(ATestIndex: Integer);
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

procedure TRValueTests.RValue_Date1;
begin
  RValueTest(0);
end;
procedure TRValueTests.RValue_Date2;
begin
  RValueTest(1);
end;
procedure TRValueTests.RValue_Date3;
begin
  RValueTest(2);
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

procedure TRValueTests.CalcRValue_Date1;
begin
  CalcRValueTest(0);
end;
procedure TRValueTests.CalcRValue_Date2;
begin
  CalcRValueTest(1);
end;
procedure TRValueTests.CalcRValue_Date3;
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

