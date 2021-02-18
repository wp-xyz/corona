unit cDataSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  cGlobal;

type
  TCaseCount = Int64;
  TCaseArray = array of TCaseCount;
  TValueArray = array of Double;

  TcDataItem = class
  private
    FID: Integer;
    FName: String;
    FParentName: String;
    FLongitude: Double;
    FLatitude: Double;
    FPopulation: Int64;
    FFirstDate: TDate;
    FConfirmed: TCaseArray;
    FDeaths: TCaseArray;
    FRecovered: TCaseArray;
    FRawData: array[TPrimaryCaseType] of TCaseArray;

    function GetCount(ACaseType: TPrimaryCaseType): Integer;

    function GetCumulativeConfirmed(AIndex: Integer): TCaseCount;
    function GetCumulativeDeaths(AIndex: Integer): TCaseCount;
    function GetCumulativeRecovered(AIndex: Integer): TCaseCount;
    function GetCumulativeSick(AIndex: Integer): TCaseCount;

    function GetDate(AIndex: Integer): TDate;

    function GetNewConfirmed(AIndex: Integer): TCaseCount;
    function GetNewDeaths(AIndex: Integer): TCaseCount;
    function GetNewRecovered(AIndex: Integer): TCaseCount;
    function GetNewSick(AIndex: Integer): TCaseCount;

    function GetNormalizedNewConfirmed(AIndex: Integer): Double;
    function GetNormalizedNewDeaths(AIndex: Integer): Double;
    function GetNormalizedNewRecovered(AIndex: Integer): Double;

  public
    function GetDateIndex(ADate: TDate): Integer;
    function GetFirstDate: TDate;
    function GetLastDate: TDate;
    function HasData(ACaseType: TPrimaryCaseType): Boolean;

    function GetDataArray(ACaseType: TCaseType; ADataType: TDataType): TValueArray;
    function GetSmoothedDataArray(ACaseType: TCaseType; ADataType: TDataType; SmoothingInterval: Integer): TValueArray;
    function CalcRValue(AIndex: Integer; out R, dR: Double): Boolean;

    procedure SetCases(AFirstDate: TDate; const ACases: TCaseArray;
      ACaseType: TPrimaryCaseType);

    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property ParentName: String read FParentName write FParentName;
    property Longitude: Double read FLongitude write FLongitude;
    property Latitude: Double read FLatitude write FLatitude;
    property Population: Int64 read FPopulation write FPopulation;

    property FirstDate: TDate read FFirstDate;
    property Date[AIndex: Integer]: TDate read GetDate;

    property Count[ACaseType: TPrimaryCaseType]: Integer read GetCount;

    property CumulativeConfirmed[AIndex: Integer]: TCaseCount read GetCumulativeConfirmed;
    property CumulativeDeaths[AIndex: Integer]: TCaseCount read GetCumulativeDeaths;
    property CumulativeRecovered[AIndex: Integer]: TCaseCount read GetCumulativeRecovered;
    property CumulativeSick[AIndex: Integer]: TCaseCount read GetCumulativeSick;

    property NewConfirmed[AIndex: Integer]: TCaseCount read GetNewConfirmed;
    property NewDeaths[AIndex: Integer]: TCaseCount read GetNewDeaths;
    property NewRecovered[AIndex: Integer]: TCaseCount read GetNewRecovered;
    property NewSick[AIndex: Integer]: TCaseCount read GetNewSick;

    property NormalizedNewConfirmed[AIndex: Integer]: double read GetNormalizedNewConfirmed;
    property NormalizedNewDeaths[AIndex: Integer]: Double read GetNormalizedNewDeaths;
    property NormalizedNewRecovered[AIndex: Integer]: Double read GetNormalizedNewRecovered;
  end;

  TStatusbarEvent = procedure (Sender: TObject; const AMsg1, AMsg2: String) of object;
  TDownloadEvent = procedure (Sender: TObject; const AMsg1, AMsg2: string; APercentage: Integer) of object;

  TcDataSource = class
  private
    FOnDownloadMsg: TDownloadEvent;
    FOnStatusMsg: TStatusbarEvent;
  protected
    FCacheDir: String;
    procedure DoDownloadMsg(const AMsg1, AMsg2: String; APercentage: Integer);
    procedure DoStatusMsg(const AMsg1, AMsg2: String);
  public
    constructor Create(ACacheDir: String); virtual;

    // Downloads the data files from the primary online site to a local cache.
    procedure DownloadToCache; virtual; abstract;

    { Extracts the line with the data value from the cache file associated with
      the clicked tree node }
    function GetDataString(const ACountry, AState, ACity: String; ACaseType: TCaseType;
      out AHeader, ACounts: String): Boolean; virtual; abstract;

    { Loads the case count data for each location in the tree view from the data file }
    function LoadData(ATreeView: TTreeView): Boolean; virtual; abstract;

    { Loads the locations from the specified cache directory into a treeview.
      Clearing, Begin/EndUpdate is done by the calling routine. }
    function LoadLocations(ATreeView: TTreeView): Boolean; virtual; abstract;

    property OnDownloadMsg: TDownloadEvent read FOnDownloadMsg write FOnDownloadMsg;
    property OnStatusMsg: TStatusbarEvent read FOnStatusMsg write FOnStatusMsg;

  end;

  TcDataSourceClass = class of TcDataSource;


implementation

uses
  Math,
  LazFileUtils;

const
  REFERENCE_POPULATION = 100000;

{ TcDataItem }

{ Calculates the R value (number of infections produced by one infected person)
  dR is an error estimate of the R value.
  The function result is false when R cannot be calculated (division by 0) }
function TcDataItem.CalcRValue(AIndex: Integer; out R, dR: Double): Boolean;
var
  i: Integer;
  sum_now, sum_earlier: Int64;
  dSum_now, dSum_earlier: Double;
begin
  Result := false;
  R := NaN;
  dR := NaN;

  // Calculate sum of new cases over a given time interval (--> SmoothingRange)
  // Iterate back in history, starting at the specified index

  // Speed trick: When n(i) is the accumulated value for day i, then n(i)-n(i-1)
  // are the new cases of this day. Calculating the sum over, say, 3 days, means
  //   sum(i, 3) = (n(i)-n(i-1)) + n(i-1)-n(i-2) + (n(i-2)-n(i-3).
  // In this term the pairs of inner terms cancel. What is left is
  //   sum(i, 3) = n(i) - n(i-3)
  if AIndex < SmoothingRange then
    exit;
  sum_now := FConfirmed[AIndex] - FConfirmed[AIndex - SmoothingRange];
  // Sum of new cases one generation time earlier (--> InfectiousPeriod)
  if AIndex < InfectiousPeriod + SmoothingRange then
    exit;
  sum_earlier := FConfirmed[AIndex - InfectiousPeriod] - FConfirmed[AIndex - InfectiousPeriod - SmoothingRange];

  // The formula for R is sum_now / sum_earlier.

  // When sum_earlier is zero, R cannot be calculated
  if sum_earlier = 0 then
    exit;

  // Error estimate: the error of each sum is approximately equal to sqrt(sum)
  dSum_now := sqrt(sum_now);
  dSum_earlier := sqrt(sum_earlier);

  // The relative error of R is the sum of the relative errors of both sums (dSum/sum)
  // The relative error of the sum cannot be calculated when the sum is zero:
  if sum_now = 0 then
    exit;

  R := sum_now / sum_earlier;
  dR := R * (dSum_now/sum_now + dSum_earlier/sum_earlier);
  Result := true;
end;

function TcDataItem.GetCount(ACaseType: TPrimaryCaseType): Integer;
begin
  case ACaseType of
    pctConfirmed: Result := Length(FConfirmed);
    pctDeaths: Result := Length(FDeaths);
    pctRecovered: Result := Length(FRecovered);
  end;
end;

function TcDataItem.GetCumulativeConfirmed(AIndex: Integer): TCaseCount;
begin
  if (AIndex >= 0) and (AIndex < Length(FConfirmed)) then
    Result := FConfirmed[AIndex]
  else
    Result := 0;
end;

function TcDataItem.GetCumulativeDeaths(AIndex: Integer): TCaseCount;
begin
  if (AIndex >= 0) and (AIndex < Length(FDeaths)) then
    Result := FDeaths[AIndex]
  else
    Result := 0;
end;

function TcDataItem.GetCumulativeRecovered(AIndex: Integer): TCaseCount;
var
  n: Integer;
begin
  n := Length(FRecovered);
  if n = 0 then
    Result := -1
  else
  if Aindex < n then
    Result := FRecovered[AIndex]
  else
    Result := 0;
end;

function TcDataItem.GetCumulativeSick(AIndex: Integer): TCaseCount;
var
  nc, nd, nr: Integer;
begin
  nc := GetCumulativeConfirmed(AIndex);
  nd := GetCumulativeDeaths(AIndex);
  nr := GetCumulativeRecovered(AIndex);   // no recovered for JHU US data
  if nr > 0 then
    Result := nc - nd - nr
  else
    Result := -1;
end;

function TcDataItem.GetDataArray(ACaseType: TCaseType;
  ADataType: TDataType): TValueArray;

  procedure WeeklySum(AValues: TValueArray);
  var
    i, j: Integer;
    sum: Double;
    v: Double;
  begin
    sum := 0;
    for i := High(AValues) downto High(AValues)-6 do
      if i > -1 then
        sum := sum + AValues[i];
    v := AValues[High(AValues)];
    AValues[High(AValues)] := sum;
    for i := High(AValues)-1 downto 0 do
    begin
      j := i - 6;  // 7 days earlier
      if j < 0 then
        sum := sum - v
      else
        sum := sum - v + AValues[j];
      v := AValues[i];
      AValues[i] := sum;
    end;
  end;

var
  pct: TPrimaryCaseType;
  factor: Double;
  i, j: Integer;
  Y, YHalf, Y0, dY: Double;
  nDesc: Integer;
begin
  if ACaseType <> ctSick then
    pct := TPrimaryCaseType(ACaseType);
  case ADataType of
    dtCumulative:
      if ACaseType = ctSick then
      begin
        SetLength(Result, Length(FRawData[pctConfirmed]));
        for i := 0 to High(Result) do
          Result[i] := FRawData[pctConfirmed, i] - FRawData[pctDeaths, i] - FRawData[pctRecovered, i];
      end else
      begin
        SetLength(Result, Length(FRawData[pct]));
        for i := 0 to High(Result) do
          Result[i] := FRawData[pct, i];
      end;
    dtNewCases:
      begin
        Result := GetDataArray(ACaseType, dtCumulative);
        for i := High(Result) downto 1 do
          Result[i] := Result[i] - Result[i-1];
      end;
    dtNormalizedCumulative:
      begin
        Result := GetDataArray(ACaseType, dtCumulative);
        factor := REF_POPULATION / FPopulation;
        for i := 0 to High(Result) do
          Result[i] := Result[i] * factor;
      end;
    dtNormalizedNewCases:
      begin
        Result := GetDataArray(ACaseType, dtNewCases);
        WeeklySum(Result);
        factor := REF_POPULATION / FPopulation;
        for i := 0 to High(Result) do
          Result[i] := Result[i] * factor;
      end;
    dtCumulativeCasesDoublingTime,
    dtNewCasesDoublingTime:
      begin
        if ADataType = dtCumulativeCasesDoublingTime then
          Result := GetSmoothedDataArray(ACaseType, dtCumulative, SmoothingRange)
        else
          Result := GetSmoothedDataArray(ACaseType, dtNewCases, SmoothingRange);
        for i := High(Result) downto 0 do begin
          Y := Result[i];
          if Y < 100 then    // Skip too noisy part
            Y := NaN
          else begin
            Y0 := Y;
            YHalf := Y0/2;
            Y := NaN;
            nDesc := 0;     // Data points found on descending part
            for j := i-1 downto 0 do
            begin
              if (Result[j] <= Y0) then      // Use ascending part only
              begin
                if (Result[j] <= Yhalf) then  // half value found
                begin
                  // Coming from the high side, j is the first point below the half
                  // j+1 is the first point above the half
                  Y := i - j - 1;
                  if Result[j] <> Result[j+1] then
                    Y := Y + (Result[j+1]-Yhalf)/(Result[j+1]-Result[j]);
                  break;
                end
                else
                  nDesc := 0;
              end else
                // Count values on descending part.
                inc(nDesc);
              // If there is a certain number of points on descending part --> ignore point
              if nDesc > 10 then
              begin
                Y := NaN;
                break;
              end;
            end;
          end;
          Result[i] := Y;
        end;
      end;
    dtRValue:
      begin
        Result := GetSmoothedDataArray(ctConfirmed, dtNewCases, SmoothingRange);
        for i := High(Result) downto 0 do
        begin
          Y := Result[i];
          Result[i] := NaN;
          if Y <= 0 then Continue;
          if i < InfectiousPeriod then Continue;
          Y0 := Result[i - InfectiousPeriod];
          if Y0 <= 0 then Continue;

          // relative error estimate: rel error of quotient is sum of
          // the relative errors of divisor and divident. Those relative errors
          // are estimated to be 1/sqrt(Y)
          dY := 1.0 / sqrt(Y) + 1.0 / sqrt(Y0);
          // Accept only values with <50% error.
          if dY < 0.5 then
            Result[i] := Y / Y0;
        end;
      end;
    else
      raise Exception.Create('Data type not supported.');
  end;
end;

function TcDataItem.GetDate(AIndex: Integer): TDate;
begin
  Result := FFirstDate + AIndex;
end;

function TcDataItem.GetDateIndex(ADate: TDate): Integer;
begin
  Result := trunc(ADate) - trunc(FFirstDate);
end;

function TcDataItem.GetFirstDate: TDate;
begin
  Result := FFirstDate;
end;

function TcDataItem.GetLastDate: TDate;
begin
  Result := FFirstDate + High(FConfirmed);
end;

function TcDataitem.GetNewConfirmed(AIndex: Integer): TCaseCount;
begin
  if AIndex > 0 then
    Result := GetCumulativeConfirmed(AIndex) - GetCumulativeConfirmed(AIndex-1)
  else
    Result := GetCumulativeConfirmed(0);
end;

function TcDataitem.GetNewDeaths(AIndex: Integer): TCaseCount;
begin
  if AIndex > 0 then
    Result := GetCumulativeDeaths(AIndex) - GetCumulativeDeaths(AIndex-1)
  else
    Result := GetCumulativeDeaths(0);
end;

function TcDataitem.GetNewRecovered(AIndex: Integer): TCaseCount;
begin
  if AIndex > 0 then
    Result := GetCumulativeRecovered(AIndex) - GetCumulativeRecovered(AIndex-1)
  else
    Result := GetCumulativeRecovered(0);
end;

function TcDataitem.GetNewSick(AIndex: Integer): TCaseCount;
begin
  if AIndex > 0 then
    Result := GetCumulativeSick(AIndex) - GetCumulativeSick(AIndex-1)
  else
    Result := GetCumulativeSick(0);
end;

function TcDataItem.GetNormalizedNewConfirmed(AIndex: Integer): Double;
var
  i, j: Integer;
begin
  Result := 0;
  if FPopulation <= 0 then
    exit;
  j := High(FConfirmed);
  for i := AIndex - 6 to AIndex do
    if InRange(i, 0, j) then
      Result := Result + NewConfirmed[i];
  Result := Result / FPopulation * REFERENCE_POPULATION;
end;

function TcDataItem.GetNormalizedNewDeaths(AIndex: Integer): Double;
var
  i, j: Integer;
begin
  Result := 0;
  if FPopulation <= 0 then
    exit;
  j := High(FDeaths);
  for i := AIndex - 6 to AIndex do
    if InRange(i, 0, j) then
      Result := Result + NewDeaths[i];
  Result := Result / FPopulation * REFERENCE_POPULATION;
end;

function TcDataItem.GetNormalizedNewRecovered(AIndex: Integer): Double;
var
  i, j: Integer;
begin
  Result := 0;
  if FPopulation <= 0 then
    exit;
  j := High(FRecovered);
  for i := AIndex - 6 to AIndex do
    if InRange(i, 0, j) then
      Result := Result + NewRecovered[i];
  Result := Result / FPopulation * 100000
end;

{ Smoothes the value array specified by ACaseType and ADataType. Smoothing
  occurs by a backward moving-average algorithm. }
function TcDataItem.GetSmoothedDataArray(ACaseType: TCaseType;
  ADataType: TDataType; SmoothingInterval: Integer): TValueArray;
var
  i, j: Integer;
  sum: Double;
  v: Double;
  n: Integer;
begin
  Result := GetDataArray(ACaseType, ADataType);

  sum := 0;
  for i := High(Result) downto High(Result) - SmoothingInterval+1 do
    if i > -1 then
      sum := sum + Result[i];
  v := Result[High(Result)];
  if High(Result) >= SmoothingInterval then
    n := SmoothingInterval
  else
    n := Length(Result);
  Result[High(Result)] := sum/n;

  for i := High(Result)-1 downto 0 do begin
    if i >= SmoothingInterval - 1 then
      sum := sum - v + Result[i - SmoothingInterval + 1]
    else begin
      sum := sum - v + Result[0];
      dec(n);
    end;
    v := Result[i];
    Result[i] := sum/n;
  end;
end;

function TcDataItem.HasData(ACaseType: TPrimaryCaseType): Boolean;
var
  i: Integer;
begin
  Result := true;
  case ACaseType of
    pctConfirmed:
      for i := 0 to Length(FConfirmed) - 1 do
        if FConfirmed[i] > 0 then exit;
    pctDeaths:
      for i := 0 to Length(FDeaths) - 1 do
        if FDeaths[i] > 0 then exit;
    pctRecovered:
      for i := 0 to Length(FRecovered) - 1 do
        if FRecovered[i] > 0 then exit;
  end;
  Result := false;
end;

procedure TcDataItem.SetCases(AFirstDate: TDate;
  const ACases: TCaseArray; ACaseType: TPrimaryCaseType);
begin
  FFirstDate := AFirstDate;
  SetLength(FRawData[ACaseType], Length(ACases));
  Move(ACases[0], FRawData[ACaseType, 0], Length(FRawData[ACaseType]) * SizeOf(TCaseCount));

  case ACaseType of
    pctConfirmed:
      begin
        SetLength(FConfirmed, Length(ACases));
        if Length(FConfirmed) > 0 then
          Move(ACases[0], FConfirmed[0], Length(FConfirmed) * SizeOf(TCaseCount));
      end;
    pctDeaths:
      begin
        SetLength(FDeaths, Length(ACases));
        if Length(FDeaths) > 0 then
          Move(ACases[0], FDeaths[0], Length(FDeaths) * SizeOf(TCaseCount));
      end;
    pctRecovered:
      begin
        SetLength(FRecovered, Length(ACases));
        if Length(FRecovered) > 0 then
          Move(ACases[0], FRecovered[0], Length(FRecovered) * Sizeof(TCaseCount));
      end;
  end;
end;


{ TcDataSource }

constructor TcDataSource.Create(ACacheDir: String);
begin
  FCacheDir := AppendPathDelim(ACacheDir);
end;

procedure TcDataSource.DoDownloadMsg(const AMsg1, AMsg2: String;
  APercentage: Integer);
begin
  if Assigned(FOnDownloadMsg) then
    FOnDownloadMsg(Self, AMsg1, AMsg2, APercentage);
end;

procedure TcDataSource.DoStatusMsg(const AMsg1, AMsg2: String);
begin
  if Assigned(FOnStatusMsg) then
    FOnStatusMsg(Self, AMsg1, AMsg2);
end;

end.

