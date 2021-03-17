unit cGeoMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Contnrs,
  TAGraph, TAChartUtils, cGlobal, cPolygonSeries;

type
  EcGeoError = class(EChartError);

  TcGeoMap = class;

  TcGeoItem = record
    Name: String;
    GeoID: Int64;
    Polygon: array of TDoublePoint;
    RingStart: array of Integer;
  end;

  TcGeoProjection = (
    gpSimple,       // simply uses longitude and latitude as in-plane coordinates
    gpMercator,     // projection on cylinder around the equator
    gpMiller,       // Mercator with latitudes scales by 4/5 = ESRI:54003 - World Miller Cylindrical
    gpHammerAitoff, //
    gpGallPeters,   // preserves area
    gpOrthographic, // orthographic projection
    gpCustom        // use projection defined by OnProjection event
  );

  TcGeoProjectionProc = procedure (ALongitude, ALatitude, ARefLongitude, ARefLatitude: Double;
    out X, Y: Double);
  TcGeoProjectionInvProc = procedure (X, Y, ARefLongitude, ARefLatitude: Double;
    out ALongitude, ALatitude: Double);

  TcGeoProjectionEvent = procedure (Sender: TObject;
    ALongitude, ALatitude, ARefLongitude, ARefLatitude: Double;
    out X, Y: Double) of object;
  TcGeoProjectionInvEvent = procedure (Sender: TObject;
    X, y, ARefLongitude, ARefLatitude: Double;
    out ALongitude, ALatitude: Double) of object;

  TcGeoMapItem = class
    Name: String;
    GeoID: TGeoID;
    Points: TDoublePointArray;
  end;

  { TcGeoMap }

  TcGeoMap = class(TComponent)
  private
    FChart: TChart;
    FActive: Boolean;
    FFileName: String;
    FPenColor: TColor;
    FBrushColor: TColor;
    FGeoIDOffset: TGeoID;
    FItems: TFPObjectList;
    FProjectionProc: TcGeoProjectionProc;
    FProjectionInvProc: TcGeoProjectionInvProc;
    FOnProjection: TcGeoProjectionEvent;
    FOnProjectionInv: TcGeoProjectionInvEvent;

    function GetSeriesByID(const AGeoID: TGeoID): TcPolygonSeries;
    function GetSeriesByName(const AName: String): TcPolygonSeries;
    function GetProjection: TcGeoProjection;
    procedure SetActive(AValue: Boolean);
    procedure SetChart(AValue: TChart);
    procedure SetFileName(AValue: TFileName);
    procedure SetProjection(AValue: TcGeoProjection);

  protected
    procedure CalcCenter(out ARefLongitude, ARefLatitude: Double);
    procedure DoSetProjection(AValue: TcGeoProjection);
    function LoadFile: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddGeoPolygon(AName: String; AGeoID: TGeoID; const APoints: TDoublePointArray);
    procedure Clear;
    procedure ClearSeries;
    procedure ListUniquePolygonNames(AList: TStrings);
    procedure Plot;
    property SeriesByID[const AGeoID: Int64]: TcPolygonSeries read GetSeriesByID;
    property SeriesByName[const AName: String]: TcPolygonSeries read GetSeriesByName;

  published
    property Active: Boolean read FActive write SetActive default false;
    property Chart: TChart read FChart write SetChart;
    property DefaultBrushColor: TColor read FBrushColor write FBrushColor default clWhite;
    property DefaultPenColor: TColor read FPenColor write FPenColor default clBlack;
    property FileName: TFileName read FFileName write SetFileName;
    property GeoIDOffset: TGeoID read FGeoIDOffset write FGeoIDOffset default 0;
    property Projection: TcGeoProjection read GetProjection write SetProjection default gpMercator;
    property OnProjection: TcGeoProjectionEvent read FOnProjection write FOnProjection;
    property OnProjectionInverse: TcGeoProjectionInvEvent read FOnProjectionInv write FOnProjectionInv;
  end;

  TcGeoMapSeries = class(TcPolygonSeries)
  protected
    FGeoMap: TcGeoMap;
    FGeoID: TGeoID;
  public
    property GeoMap: TcGeoMap read FGeoMap write FGeoMap;
    property GeoID: TGeoID read FGeoID write FGeoID;
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
procedure SimpleProjection(In1, In2, ARefLongitude, ARefLatitude: Double;
  out Out1, Out2: Double);
begin
  Out1 := In1;
  Out2 := In2;
end;

// https://forum.openstreetmap.org/viewtopic.php?id=20097
procedure Mercator(ALongitude, ALatitude, ARefLongitude, ARefLatitude: Double;
  out X, Y: Double);
begin
  X := ALongitude * PItRo180;
  Y := ln(tan((90 + ALatitude) * PIo360)) * R2D;
  Y := Y * PItRo180;
end;

procedure InvMercator(X, Y, ARefLongitude, ARefLatitude: Double;
  out ALongitude, ALatitude: Double);
begin
  ALongitude := (X / 20037508.34) * 180;
  ALatitude := (Y / 20037508.34) * 180;
  ALatitude := 180 / PI * (2*arctan(exp(ALatitude * PI / 180)) - PI/2);
end;


// https://en.wikipedia.org/wiki/Gall%E2%80%93Peters_projection
procedure GallPeters(ALongitude, ALatitude, ARefLongitude, ARefLatitude: Double;
  out X, Y: Double);
begin
  X := PItRo180 * ALongitude;            // R * lambda
  Y := TWO_R * sin(D2R * ALatitude);     // 2 * r * sin(phi)
end;

procedure InvGallPeters(X, Y, ARefLongitude, ARefLatitude: Double;
  out ALongitude, ALatitude: Double);
begin
  ALongitude := X / PItRo180;
  ALatitude := arcsin(Y / TWO_R) * R2D;
end;


// https://en.wikipedia.org/wiki/Hammer_projection
procedure HammerAitoff(ALongitude, ALatitude, ARefLongitude, ARefLatitude: Double;
  out X, Y: Double);
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

procedure InvHammerAitoff(X, Y, ARefLongitude, ARefLatitude: Double;
  out ALongitude, ALatitude: Double);
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
procedure Miller(ALongitude, ALatitude, ARefLongitude, ARefLatitude: Double;
  out X, Y: Double);
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

procedure InvMiller(X, Y, ARefLongitude, ARefLatitude: Double;
  out ALongitude, ALatitude: Double);
var
  phi, tmp: Double;
begin
  ALongitude := X / PItRo180;
  Y := Y / EARTH_RADIUS;
  tmp := exp(4/5*Y);
  phi := 5/2 * arctan(tmp) - 5/8 * pi;
  ALatitude := R2D * phi;
end;


// https://en.wikipedia.org/wiki/Orthographic_map_projection
procedure Orthographic(ALongitude, ALatitude, ARefLongitude, ARefLatitude: Double;
  out X, Y: Double);
var
  phi, cosPhi, sinPhi: Double;
  phi0, cosPhi0, sinPhi0: double;
  lambda, lambda0, cosLambda, sinLambda: Double;
  c, cPhi, sPhi: Double;
begin
  lambda := ALongitude * D2R;
  phi := ALatitude * D2R;
  lambda0 := ARefLongitude * D2R;
  phi0 := ARefLatitude * D2R;
  SinCos(phi, sinPhi, cosPhi);
  SinCos(phi0, sinPhi0, cosPhi0);
  SinCos(lambda - lambda0, sinLambda, cosLambda);
  c := sinPhi0 * sinPhi + cosPhi0 * cosPhi * cosLambda;
  if c > 0 then
  begin
    X := EARTH_RADIUS * cosPhi * sinLambda;
    Y := EARTH_RADIUS * (cosPhi0 * sinPhi - sinPhi0 * cosPhi * cosLambda);
  end else
  begin
    // Point on other side of sphere must be clipped
    X := Infinity;  // To do: Find better values
    Y := Infinity;
  end;
end;

procedure InvOrthographic(X, Y, ARefLongitude, ARefLatitude: Double;
  out ALongitude, ALatitude: Double);
var
  rho: Double;
  c, sinC, cosC: Double;
  phi, phi0, sinPhi0, cosPhi0: Double;
  lambda, lambda0: Double;
begin
  lambda0 := ARefLongitude * D2R;

  phi0 := ARefLatitude * D2R;
  SinCos(phi0, sinPhi0, cosPhi0);

  rho := sqrt(X*X + Y*Y);
  c := arcsin(rho/EARTH_RADIUS);
  SinCos(c, sinC, cosC);

  phi := arcsin(cosC * sinPhi0 + (Y * sinC * cosPhi0) / rho);
  lambda := lambda0 + arctan2(X * sinC, rho * cosC * cosPhi0 - Y * sinC * sinPhi0);

  ALongitude := lambda / D2R;
  ALatitude := phi / D2R;
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
  FItems := TFPObjectList.Create;
  FBrushColor := clWhite;
  FPenColor := clBlack;
  DoSetProjection(gpSimple);
end;

destructor TcGeoMap.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ Adds a polygon to the map. The polygon consists of longitude/latitude pairs
  where angles are given in degrees. When the polygon exactly meets a point
  already used it is assumed that the polygon consists of several parts. }
procedure TcGeoMap.AddGeoPolygon(AName: String; AGeoID: TGeoID;
  const APoints: TDoublePointArray);
var
  item: TcGeoMapItem;
begin
  item := TcGeoMapItem.Create;
  item.Name := AName;
  item.GeoID := AGeoID + FGeoIDOffset;
  item.Points := APoints;
  FItems.Add(item);
end;

procedure TcGeoMap.CalcCenter(out ARefLongitude, ARefLatitude: double);
var
  i, j: Integer;
  item: TcGeoMapItem;
  minLon, maxLon, minLat, maxLat: Double;
begin
  minLon := Infinity;
  maxLon := -Infinity;
  minLat := Infinity;
  maxLat := -Infinity;
  for i := 0 to FItems.Count-1 do
  begin
    item := TcGeoMapItem(FItems[i]);
    for j := 0 to High(item.Points) do
    begin
      UpdateMinMax(item.Points[j].X, minLon, maxLon);
      UpdateMinMax(item.Points[j].Y, minLat, maxLat);
    end;
  end;
  ARefLongitude := (maxLon + minLon) / 2;
  ARefLatitude := (maxLat + minLat) / 2;

  {
  AveLongitude := 0;
  AveLatitude := 0;
  n := 0;
  for i := 0 to FItems.Count-1 do begin
    item := TcGeoMapItem(FItems[i]);
    for j := 0 to High(item.Points) do
    begin
      AveLongitude := AveLongitude + item.Points[j].X;
      AveLatitude := AveLatitude + item.Points[j].Y;
      inc(n);
    end;
  end;
  AveLongitude := AveLongitude / n;
  AveLatitude := AveLatitude / n;
  }
end;

procedure TcGeoMap.Clear;
begin
  FItems.Clear;
  ClearSeries;
end;

procedure TcGeoMap.ClearSeries;
var
  i: Integer;
begin
  if FChart = nil then
    exit;

  for i := FChart.Series.Count-1 downto 0 do
    if (FChart.Series[i] is TcGeoMapSeries) and (TcGeoMapSeries(FChart.Series[i]).GeoMap = self) then
      FChart.Series[i].Free;
end;

function TcGeoMap.GetSeriesByID(const AGeoID: TGeoID): TcPolygonSeries;
var
  i: Integer;
begin
  if FChart = nil then
    exit(nil);

  for i := 0 to FChart.SeriesCount-1 do
    if (FChart.Series[i] is TcGeoMapSeries) and
       (TcGeoMapSeries(FChart.Series[i]).GeoID = AGeoID) then
    begin
      Result := TcGeoMapSeries(FChart.Series[i]);
      exit;
    end;

  Result := nil;
end;

function TcGeoMap.GetSeriesByName(const AName: String): TcPolygonSeries;
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
  if FProjectionProc = @Orthographic then
    Result := gpOrthographic
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

procedure TcGeoMap.Plot;
var
  ser: TcGeoMapSeries;
  item: TcGeoMapItem;
  i, j: Integer;
  x, y: Double;
  lon, lat: Double;
  refLon, refLat: Double;
begin
  if FChart = nil then
    raise EChartError.Create('No chart connected to GeoMap');

  CalcCenter(refLon, refLat);
  ClearSeries;

  for i := 0 to FItems.Count-1 do begin
    item := TcGeoMapItem(FItems[i]);

    ser := GetSeriesByID(item.GeoID) as TcGeoMapSeries;  // Append to existing polygon(s)
    if ser = nil then
      ser := TcGeoMapSeries.Create(FChart);
    ser.GeoID := item.GeoID;

    ser.Title := item.Name;
    ser.Brush.Color := FBrushColor;
    ser.Pen.Color := FPenColor;
    ser.GeoMap := self;
    if (GetProjection = gpCustom) then
    begin
      if Assigned(FOnProjection) then
        for j := 0 to High(item.Points) do
        begin
          lon := item.Points[j].X;
          lat := item.Points[j].Y;
          FOnProjection(Self, lon, lat, refLon, refLat, x, y);
          ser.AddXY(x, y);
        end;
    end else
      for j := 0 to High(item.Points) do
      begin
        lon := item.Points[j].x;
        lat := item.Points[j].y;
        FProjectionProc(lon, lat, refLon, refLat, x, y);
        if not (IsInfinite(abs(x)) or IsInfinite(abs(y))) then
          ser.AddXY(x, y);
      end;
    FChart.AddSeries(ser);
  end;
  {

  aveLon := 0;
  aveLat := 0;
  n := 0;
  for i := 0 to High(APoints) do
  begin
    aveLon := aveLon + APoints[i].X;
    aveLat := avelat + APoints[i].Y;
    inc(n);
  end;
  aveLon := aveLon / n;
  aveLat := aveLat / n;

  ser := GetSeries(AName) as TcGeoMapSeries;
  if ser = nil then
  begin
    ser := TcGeoMapSeries.Create(FChart);
    ser.Title := AName;
    ser.Brush.Color := FBrushColor;
    ser.Pen.Color := FPenColor;
  end;

  if (GetProjection = gpCustom) then
  begin
    if Assigned(FOnProjection) then
    for i := 0 to High(APoints) do
    begin
      lon := APoints[i].X;
      lat := APoints[i].Y;
      FOnProjection(self, lon, lat, aveLon, aveLat, x, y);
      ser.AddXY(x, y);
    end;
  end else
  begin
    for i := 0 to High(APoints) do
    begin
      lon := APoints[i].X;
      lat := APoints[i].Y;
      FProjectionProc(lon, lat, aveLon, aveLat, x, y);
      ser.AddXY(x, y);
    end;
  end;

  FChart.AddSeries(ser);
  }
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
    gpOrthographic:
      begin
        FProjectionProc := @Orthographic;
        FProjectionInvProc := @InvOrthographic;
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

