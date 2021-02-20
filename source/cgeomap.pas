unit cGeoMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  TAGraph, TAChartUtils, cPolygonSeries;

type
  EcGeoError = class(EChartError);

  TcGeoMap = class;

  TcGeoItem = record
    Name: String;
    Polygon: array of TDoublePoint;
    RingStart: array of Integer;
  end;

  TcGeoProjection = (
    gpSimple,       // simply uses longitude and latitude as in-plane coordinates
    gpMercator,     // projection on cylinder around the equator
    gpMiller,       // Mercator with latitudes scales by 4/5 = ESRI:54003 - World Miller Cylindrical
    gpHammerAitoff, //
    gpGallPeters,   // preserves area
    gpCustom        // use projection defined by OnProjection event
  );

  TcGeoProjectionProc = procedure (ALongitude, ALatitude: Double; out X, Y: Double);
  TcGeoProjectionInvProc = procedure (X, Y: Double; out ALongitude, ALatitude: Double);

  TcGeoProjectionEvent = procedure (Sender: TObject;
    ALongitude, ALatitude: Double; out X, Y: Double) of object;
  TcGeoProjectionInvEvent = procedure (Sender: TObject;
    X, y: Double; out ALongitude, ALatitude: Double) of object;

  { TcGeoMap }

  TcGeoMap = class(TComponent)
  private
    FChart: TChart;
    FActive: Boolean;
    FFileName: String;
    FProjectionProc: TcGeoProjectionProc;
    FProjectionInvProc: TcGeoProjectionInvProc;
    FOnProjection: TcGeoProjectionEvent;
    FOnProjectionInv: TcGeoProjectionInvEvent;

    function GetSeries(const AName: String): TcPolygonSeries;
    function GetProjection: TcGeoProjection;
    procedure SetActive(AValue: Boolean);
    procedure SetChart(AValue: TChart);
    procedure SetFileName(AValue: TFileName);
    procedure SetProjection(AValue: TcGeoProjection);

  protected
    procedure DoSetProjection(AValue: TcGeoProjection);
    function LoadFile: Boolean;

  public
    constructor Create(AOwner: TComponent); override;

    procedure AddGeoPolygon(AName: String; const APoints: TDoublePointArray;
      AFillColor: TColor = clTAColor; ABorderColor: TColor = clTAColor);
    procedure Clear;
    procedure ListUniquePolygonNames(AList: TStrings);
    property Series[const AName: String]: TcPolygonSeries read GetSeries;

  published
    property Active: Boolean read FActive write SetActive default false;
    property Chart: TChart read FChart write SetChart;
    property FileName: TFileName read FFileName write SetFileName;
    property Projection: TcGeoProjection read GetProjection write SetProjection default gpMercator;
    property OnProjection: TcGeoProjectionEvent read FOnProjection write FOnProjection;
    property OnProjectionInverse: TcGeoProjectionInvEvent read FOnProjectionInv write FOnProjectionInv;
  end;

  TcGeoMapSeries = class(TcPolygonSeries)
  protected
    FGeoMap: TcGeoMap;
  public
    property GeoMap: TcGeoMap read FGeoMap write FGeoMap;
  end;

  TcGeoReader = class
  public
    class function Check(AStream: TStream): Boolean; virtual; abstract;
    procedure ReadFromFile(AFileName: String; AMap: TcGeoMap);
    procedure ReadFromStream(AStream: TStream; AMap: TcGeoMap); virtual; abstract;
  end;
  TcGeoReaderClass = class of TcGeoReader;

procedure RegisterGeoReader(AName, AExtensions: String; AClass: TcGeoReaderClass);

function GetGeoReaderClassForExtension(AExtension: String): TcGeoReaderClass;
function GetGeoReaderClassForFile(AFileName: String): TcGeoReaderClass;
function GetGeoReaderClassForStream(AStream: TStream): TcGeoReaderClass;


implementation

uses
  Math;

const
  D2R = pi / 180;
  R2D = 180 / pi;
  EARTH_RADIUS = 6.371;  // kilometers
  Two_R = 2.0 * EARTH_RADIUS;
  PItRo180 = pi * EARTH_RADIUS / 180;  // "pi" "t"imes "R"adius "o"ver "180"
  PIo360 = pi / 360;                   // "pi" "o"ver "360"
  PIo4 = pi / 4;
  SQRT2 = sqrt(2.0);
  TWO_SQRT2 = 2.0 * SQRT2;


{-------------------------------------------------------------------------------
                           Projections
-------------------------------------------------------------------------------}

// Set projected planar coordinate values equal to the spherical angular coordinates
procedure SimpleProjection(In1, In2: Double; out Out1, Out2: Double);
begin
  Out1 := In1;
  Out2 := In2;
end;

// https://forum.openstreetmap.org/viewtopic.php?id=20097
procedure Mercator(ALongitude, ALatitude: Double; out X, Y: Double);
begin
  X := ALongitude * PItRo180;
  Y := ln(tan((90 + ALatitude) * PIo360)) * R2D;
  Y := Y * PItRo180;
end;

procedure InvMercator(X, Y: Double; out ALongitude, ALatitude: Double);
begin
  ALongitude := (X / 20037508.34) * 180;
  ALatitude := (Y / 20037508.34) * 180;
  ALatitude := 180 / PI * (2*arctan(exp(ALatitude * PI / 180)) - PI/2);
end;


// https://en.wikipedia.org/wiki/Gall%E2%80%93Peters_projection
procedure GallPeters(ALongitude, ALatitude: Double; out X, Y: Double);
begin
  X := PItRo180 * ALongitude;            // R * lambda
  Y := TWO_R * sin(D2R * ALatitude);     // 2 * r * sin(phi)
end;

procedure InvGallPeters(X, Y: Double; out ALongitude, ALatitude: Double);
begin
  ALongitude := X / PItRo180;
  ALatitude := arcsin(Y / TWO_R) * R2D;
end;


// https://en.wikipedia.org/wiki/Hammer_projection
procedure HammerAitoff(ALongitude, ALatitude: Double; out X, Y: Double);
var
  lambda2, phi: Double;
  sinPhi, cosPhi, sinLambda2, cosLambda2: double;
  denom: Double;
begin
  lambda2 := ALongitude * D2R * 0.5;
  phi := ALatitude * D2R;
  SinCos(lambda2, sinLambda2, cosLambda2);
  SinCos(phi, sinPhi, cosPhi);
  denom := sqrt(1 + cosPhi * cosLambda2);
  X := (TWO_SQRT2 * cosPhi * sinLambda2) / denom;
  Y := SQRT2 * sinPhi / denom;
end;

procedure InvHammerAitoff(X, Y: Double; out ALongitude, ALatitude: Double);
var
  z: Double;
  phi, lambda: Double;
begin
  z := sqrt(1 - sqr(X/4) - sqr(Y/2));
  lambda := 2 * arctan(z*X / (2*(2*sqr(z) - 1)));
  phi := arcsin(z*Y);
  ALongitude := R2D * lambda;
  ALatitude := R2D * phi;
end;


// https://en.wikipedia.org/wiki/Miller_cylindrical_projection
procedure Miller(ALongitude, ALatitude: Double; out X, Y: Double);
var
  phi, lambda: Double;
begin
  lambda := ALongitude * D2R;
  phi := ALatitude * D2R;

  X := lambda * EARTH_RADIUS;
  Y := tan(PIo4 + 2*phi/5);
  if Y > 0 then
    Y := ln(Y) * EARTH_RADIUS
  else
    Y := NaN;
end;

procedure InvMiller(X, Y: Double; out ALongitude, ALatitude: Double);
var
  phi, tmp: Double;
begin
  ALongitude := X / PItRo180;
  Y := Y / EARTH_RADIUS;
  tmp := exp(4/5*Y);
  phi := 5/2 * arctan(tmp) - 5/8 * pi;
  ALatitude := R2D * phi;
end;


{-------------------------------------------------------------------------------
                              TcGeoReader
--------------------------------------------------------------------------------}

procedure TcGeoReader.ReadFromFile(AFileName: String; AMap: TcGeoMap);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    ReadFromStream(stream, AMap);
  finally
    stream.Free;
  end;
end;


{-------------------------------------------------------------------------------
                           GeoReader registration
-------------------------------------------------------------------------------}

type
  TcGeoReaderListItem = class
    FName: String;
    FExtensions: String;
    FReaderClass: TcGeoReaderClass;
  end;

  TcGeoReaderList = class(TFPList)
  public
    function IndexOfReader(AClass: TcGeoReaderClass): Integer;
  end;

var
  GeoReaders: TcGeoReaderList = nil;

function TcGeoReaderList.IndexOfReader(AClass: TcGeoReaderClass): Integer;
var
  i: Integer;
  item: TcGeoReaderListItem;
begin
  Result := -1;
  for i := 0 to Count-1 do
  begin
    item := TcGeoReaderListItem(Items[i]);
    if item.FReaderClass = AClass then
    begin
      Result := i;
      exit;
    end;
  end;
end;

procedure RegisterGeoReader(AName, AExtensions: String; AClass: TcGeoReaderClass);
var
  idx: Integer;
  item: TcGeoReaderListItem;
begin
  if GeoReaders = nil then
    GeoReaders := TcGeoReaderList.Create;
  idx := GeoReaders.IndexOfReader(AClass);
  if idx = -1 then
  begin
    item := TcGeoReaderListItem.Create;
    item.FName := AName;
    item.FExtensions := AExtensions;
    item.FReaderClass := AClass;
    GeoReaders.Add(item);
  end;
end;

function GetGeoReaderClassForExtension(AExtension: String): TcGeoReaderClass;
var
  i: Integer;
  L: TStrings;
  item: TcGeoReaderListItem;
begin
  Result := nil;
  if GeoReaders = nil then
    exit;

  AExtension := Lowercase(AExtension);
  L := TStringList.Create;
  try
    L.Delimiter := '|';
    L.StrictDelimiter := true;
    for i := 0 to GeoReaders.Count-1 do
    begin
      item := TcGeoReaderListItem(GeoReaders[i]);
      L.DelimitedText := Lowercase(item.FExtensions);
      if L.IndexOf(AExtension) <> -1 then
      begin
        Result := item.FReaderClass;
        exit;
      end;
    end;
  finally
    L.Free;
  end;
end;

function GetGeoReaderClassForStream(AStream: TStream): TcGeoReaderClass;
var
  i: Integer;
  item: TcGeoReaderlistItem;
begin
  Result := nil;
  if GeoReaders = nil then
    exit;

  for i := 0 to GeoReaders.Count-1 do begin
    item := TcGeoReaderListItem(GeoReaders[i]);
    if item.FReaderClass.Check(AStream) then
    begin
      Result := item.FReaderClass;
      exit;
    end;
  end;
end;

function GetGeoReaderClassForFile(AFileName: String): TcGeoReaderClass;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    Result := GetGeoReaderClassForStream(stream);
  finally
    stream.Free;
  end;
end;

procedure FreeGeoReaders;
var
  i: Integer;
begin
  if GeoReaders <> nil then
  begin
    for i := 0 to GeoReaders.Count-1 do
      TcGeoReaderListItem(GeoReaders[i]).Free;
    FreeAndNil(GeoReaders);
  end;
end;


{-------------------------------------------------------------------------------
                                 TcGeoMap
-------------------------------------------------------------------------------}

constructor TcGeoMap.Create(AOwner: TComponent);
begin
  inherited;
  DoSetProjection(gpSimple);
end;


{ Adds a polygon to the map. The polygon consists of longitude/latitude pairs
  where angles are given in degrees. When the polygon exactly meets a point
  already used it is assumed that the polygon consists of several parts. }
procedure TcGeoMap.AddGeoPolygon(AName: String; const APoints: TDoublePointArray;
  AFillColor: TColor = clTAColor; ABorderColor: TColor = clTAColor);
var
  ser: TcGeoMapSeries;
  i: Integer;
  x, y: Double;
  lon, lat: Double;
begin
  if FChart = nil then
    raise EChartError.Create('No chart connected to GeoMap');

  ser := GetSeries(AName) as TcGeoMapSeries;
  if ser = nil then
  begin
    ser := TcGeoMapSeries.Create(FChart);
    ser.Title := AName;
    ser.Brush.Color := AFillColor;
    ser.Pen.Color := ABorderColor;
  end;

  if (GetProjection = gpCustom) then
  begin
    if Assigned(FOnProjection) then
    for i := 0 to High(APoints) do
    begin
      lon := APoints[i].X;
      lat := APoints[i].Y;
      FOnProjection(self, lon, lat, x, y);
      ser.AddXY(x, y);
    end;
  end else
  begin
    for i := 0 to High(APoints) do
    begin
      lon := APoints[i].X;
      lat := APoints[i].Y;
      FProjectionProc(lon, lat, x, y);
      ser.AddXY(x, y);
    end;
  end;

  FChart.AddSeries(ser);
end;

procedure TcGeoMap.Clear;
var
  i: Integer;
begin
  if FChart = nil then
    exit;

  for i := FChart.SeriesCount-1 downto 0 do
    if (FChart.Series[i] is TcGeoMapSeries) and (TcGeoMapseries(FChart.Series[i]).GeoMap = self) then
      FChart.Series[i].Free;
end;

function TcGeoMap.GetSeries(const AName: String): TcPolygonSeries;
var
  i: Integer;
begin
  if FChart = nil then
    exit(nil);

  for i := 0 to FChart.SeriesCount-1 do
    if (FChart.Series[i] is TcGeoMapSeries) and
       (TcGeoMapSeries(FChart.Series[i]).Title = AName) then
    begin
      Result := TcGeoMapSeries(FChart.Series[i]);
      exit;
    end;

  Result := nil;
end;

function TcGeoMap.GetProjection: TcGeoProjection;
begin
  if FProjectionProc = @SimpleProjection then
    Result := gpSimple
  else
  if FProjectionProc = @Mercator then
    Result := gpMercator
  else
  if FProjectionProc = @Miller then
    Result := gpMiller
  else
  if FProjectionProc = @HammerAitoff then
    Result := gpHammerAitoff
  else
  if FProjectionProc = @GallPeters then
    Result := gpGallPeters
  else
    Result := gpCustom;
end;

procedure TcGeoMap.ListUniquePolygonNames(AList: TStrings);
var
  i: Integer;
  title: String;
begin
  for i := 0 to FChart.SeriesCount-1 do
    if FChart.Series[i] is TcGeoMapSeries then
    begin
      title := TcGeoMapSeries(FChart.Series[i]).Title;
      if AList.IndexOf(title) = -1 then
        AList.Add(title);
    end;
end;

function TcGeoMap.LoadFile: Boolean;
var
  readerClass: TcGeoReaderClass;
  reader: TcGeoReader;
begin
  Result := false;

  if FFileName = '' then exit;

  if not FileExists(FFileName) then
  begin
    raise EcGeoError.CreateFmt('File "%s" not found.', [FFileName]);
    exit;
  end;

  readerClass := GetGeoReaderClassForFile(FFileName);
  if readerClass = nil then
  begin
    raise EcGeoError.CreateFmt('No reader found for "%s"', [FFileName]);
    exit;
  end;

  Clear;
  reader := readerClass.Create;
  try
    reader.ReadFromFile(FFileName, self);
    Result := true;
  finally
    reader.Free;
  end;
end;


procedure TcGeoMap.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    exit;

  FActive := AValue and (FChart <> nil) and (FFileName <> '');

  if FActive then
    FActive := LoadFile;

  if not FActive then
    Clear;
end;


procedure TcGeoMap.SetChart(AValue: TChart);
begin
  if FChart = AValue then
    exit;
  if FActive then
    raise EcGeoError.Create('Cannot change chart while GeoMap is active.');
  FChart := AValue;
end;

procedure TcGeoMap.SetFileName(AValue: TFileName);
begin
  if AValue = FFileName then
    exit;

  if FActive then
    raise EcGeoError.Create('Cannot change filename while GeoMap is active.');

  FFileName := AValue;
end;

procedure TcGeoMap.DoSetProjection(AValue: TcGeoProjection);
begin
  case AValue of
    gpSimple:
      begin
        FProjectionProc := @SimpleProjection;
        FProjectionInvProc := @SimpleProjection;
      end;
    gpMercator:
      begin
        FProjectionProc := @Mercator;
        FProjectionInvProc := @InvMercator;
      end;
    gpMiller:
      begin
        FProjectionProc := @Miller;
        FProjectionInvProc := @InvMiller;
      end;
    gpHammerAitoff:
      begin
        FProjectionProc := @HammerAitoff;
        FProjectionInvProc := @InvHammerAitoff;
      end;
    gpGallPeters:
      begin
        FProjectionProc := @GallPeters;
        FProjectionInvProc := @InvGallPeters;
      end;
    gpCustom:
      begin
        FProjectionProc := nil;
        FProjectionInvProc := nil;
      end;
  end;
end;

procedure TcGeoMap.SetProjection(AValue: TcGeoProjection);
begin
  if GetProjection <> AValue then
    DoSetProjection(AValue);
end;

finalization
  FreeGeoReaders;

end.

