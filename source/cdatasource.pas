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

  { TcDataItem collects all Covid data for one global entity (country, state, etc)
    beginning at FirstDate, one record per day following.
    It also stores the resource name of the map which will be displayed when this
    item is active in the applications tree view. }
  TcDataItem = class
  private
    FGeoID: TGeoID;
    FID: Integer; // Needed by RKI
    FName: String;
    FParentName: String;
    FLongitude: Double;
    FLatitude: Double;
    FPopulation: Int64;
    FFirstDate: TDate;
    FMapResource: String;
    FOtherMapResource: String;
    FUseOtherMapResource: Boolean;
    FMapDataLevelDist: Integer;
    FOtherMapDataLevelDist: Integer;
    FMapDataAtChildLevel: Boolean;
    FOtherMapDataAtChildLevel: Boolean;
    FRawData: array[TPrimaryCaseType] of TCaseArray;  // Cumulative cases!
    function GetCount(ACaseType: TPrimaryCaseType): Integer;
    function GetDate(AIndex: Integer): TDate;
    function GetRawData(ACaseType: TPrimaryCaseType): TCaseArray;

  public
    function GetDateIndex(ADate: TDate): Integer;
    function GetFirstDate: TDate;
    function GetLastDate: TDate;
    function HasData(ACaseType: TPrimaryCaseType): Boolean;

    function GetDataArray(ACaseType: TCaseType; ADataType: TDataType): TValueArray;
    function GetSmoothedDataArray(ACaseType: TCaseType; ADataType: TDataType; SmoothingInterval: Integer): TValueArray;

    function CalcNormalizedNewCases(AIndex: Integer; ACaseType: TCaseType): Double;
    function CalcRValue(AIndex: Integer; out R, dR: Double): Boolean; overload;
    function CalcRValue(AIndex: Integer): Double; overload;

    procedure SetCases(AFirstDate: TDate; const ACases: TCaseArray;
      ACaseType: TPrimaryCaseType);

    // Unique ID of the country/state/county assigned to this data item.
    property GeoID: TGeoID read FGeoID write FGeoID;
    // ID needed by RKI
    property ID: Integer read FID write FID;
    // Name of the country/state/county
    property Name: String read FName write FName;
    property ParentName: String read FParentName write FParentName;
    property Longitude: Double read FLongitude write FLongitude;
    property Latitude: Double read FLatitude write FLatitude;
    property Population: Int64 read FPopulation write FPopulation;

    // Name of the resource with the map
    property MapResource: String read FMapResource write FMapResource;
    // US has two maps: one with states, the other one with counties - this is the counties map
    property OtherMapResource: String read FOtherMapResource write FOtherMapResource;
    // Determins that this data item needs the "other" map
    property UseOtherMapResource: boolean read FUseOtherMapResource write FUseOtherMapResource;
    // Count of tree levels between the map node and the nodes containing the mapped data
    property MapDataLevelDist: Integer read FMapDataLevelDist write FMapDataLevelDist;
    property OtherMapDataLevelDist: Integer read FOtherMapDataLevelDist write FOtherMapDataLevelDist;
    // When MapDataAtChildLevel is true the mapped data are one level below the nodes defined
    // by MapDataLevelDist
    property MapDataAtChildLevel: Boolean read FMapDataAtChildLevel write FMapDataAtChildLevel;
    property OtherMapDataAtChildLevel: Boolean read FOtherMapDataAtChildLevel write FOtherMapDataAtChildLevel;

    // Date of the first data record. There must be no gaps in the date till the last record.
    property FirstDate: TDate read FFirstDate;
    // Date of the data record at the specified index.
    property Date[AIndex: Integer]: TDate read GetDate;

    property Count[ACaseType: TPrimaryCaseType]: Integer read GetCount;
    property RawData[ACaseType: TPrimaryCaseType]: TCaseArray read GetRawData;
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
                             (*
    { Extracts the line with the data value from the cache file associated with
      the clicked tree node }
    function GetDataString(const ACountry, AState, ACity: String; ACaseType: TCaseType;
      out AHeader, ACounts: String): Boolean; virtual; abstract;
                              *)
    { Loads the case count data for each location in the tree view from the data file
      When ANode is nil all data are read, otherwise only those for the given node. }
    function LoadData(ATreeView: TTreeView; ANode: TTreeNode): Boolean; virtual; abstract;

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

function TcDataItem.CalcNormalizedNewCases(AIndex: Integer; ACaseType: TCaseType): Double;
var
  accum_Now, accum_WeekBefore: TCaseCount;
  j: Integer;
begin
  Result := 0;

  if Length(FRawData[pctConfirmed]) = 0 then
    exit;

  if AIndex < 0 then
    AIndex := 0;
  if AIndex > High(FRawData[pctConfirmed]) then
    AIndex := High(FRawData[pctConfirmed]);
  j := AIndex - 7;
  if j < 0 then j := 0;

  if ACaseType = ctSick then
  begin
    accum_Now := FRawData[pctConfirmed,AIndex] - FRawData[pctDeaths, AIndex] - FRawData[pctRecovered, AIndex];
    accum_WeekBefore := FRawData[pctConfirmed, j] - FRawData[pctDeaths, j] - FRawData[pctRecovered, j];
  end else
  begin
    accum_Now := FRawData[TPrimaryCaseType(ACaseType), AIndex];
    accum_WeekBefore := FRawdata[TPrimaryCaseType(ACaseType), j];
  end;

  Result := (accum_Now - accum_WeekBefore) / FPopulation * REF_POPULATION;
end;

{ Calculates the R value (number of infections produced by one infected person)
  dR is an error estimate of the R value.
  The function result is false when R cannot be calculated (division by 0) }
function TcDataItem.CalcRValue(AIndex: Integer; out R, dR: Double): Boolean;
var
  i: Integer;
  sum_now, sum_earlier: TCaseCount;
  dSum_now, dSum_earlier: Double;
begin
  Result := false;
  R := NaN;
  dR := NaN;

  if Length(FRawData[pctConfirmed]) = 0 then
    exit;

  // Calculate sum of new cases over a given time interval (--> SmoothingRange)
  // Iterate back in history, starting at the specified index

  // Speed trick: When n(i) is the accumulated value for day i, then n(i)-n(i-1)
  // are the new cases of this day. Calculating the sum over, say, 3 days, means
  //   sum(i, 3) = (n(i)-n(i-1)) + n(i-1)-n(i-2) + (n(i-2)-n(i-3).
  // In this term the pairs of inner terms cancel. What is left is
  //   sum(i, 3) = n(i) - n(i-3)
  if AIndex < SmoothingRange then
    exit;
  sum_now := FRawData[pctConfirmed, AIndex] - FRawData[pctConfirmed, AIndex - SmoothingRange];
  // Sum of new cases one generation time earlier (--> InfectiousPeriod)
  if AIndex < InfectiousPeriod + SmoothingRange then
    exit;
  sum_earlier := FRawData[pctConfirmed, AIndex - InfectiousPeriod] - FRawData[pctConfirmed, AIndex - InfectiousPeriod - SmoothingRange];

  // The formula for R is sum_now / sum_earlier.

  // When sum_earlier is zero, R cannot be calculated
  if sum_earlier <= 0 then
    exit;

  // The relative error of R is the sum of the relative errors of both sums (dSum/sum)
  // The relative error of the sum cannot be calculated when the sum is zero:
  if sum_now <= 0 then
    exit;

  // Error estimate: the error of each sum is approximately equal to sqrt(sum)
  dSum_now := sqrt(sum_now);
  dSum_earlier := sqrt(sum_earlier);

  R := sum_now / sum_earlier;
  dR := R * (dSum_now/sum_now + dSum_earlier/sum_earlier);
  Result := true;
end;

function TcDataItem.CalcRValue(AIndex: Integer): Double;
var
  dR: Double;
begin
  CalcRValue(AIndex, Result, dR);
end;

function TcDataItem.GetCount(ACaseType: TPrimaryCaseType): Integer;
begin
  Result := Length(FRawData[ACaseType]);
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
  if Length(FRawData[pctConfirmed]) = 0 then
  begin
    Result := nil;
    exit;
  end;

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
  Result := FFirstDate + High(FRawData[pctConfirmed]);
end;

function TcDataItem.GetRawData(ACaseType: TPrimaryCaseType): TCaseArray;
begin
  Result := FRawData[ACaseType];
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
  for i := 0 to High(FRawData[ACaseType]) do
    if FRawData[ACaseType, i] > 0 then
      exit;
  Result := false;
end;

procedure TcDataItem.SetCases(AFirstDate: TDate;
  const ACases: TCaseArray; ACaseType: TPrimaryCaseType);
begin
  FFirstDate := AFirstDate;
  SetLength(FRawData[ACaseType], Length(ACases));
  if Length(ACases) > 0 then
    Move(ACases[0], FRawData[ACaseType, 0], Length(FRawData[ACaseType]) * SizeOf(TCaseCount));
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

