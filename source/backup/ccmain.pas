unit ccMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, TAGraph, TAIntervalSources, TASeries, TAChartListbox, TALegend,
  TACustomSeries, TATransformations, TASources, TATools;

type
  TDataType = (dtConfirmed, dtDeaths, dtRecovered);

  { TMainForm }

  TMainForm = class(TForm)
    btnUpdate: TButton;
    btnClear: TButton;
    Chart: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    LeftAxisTransformations: TChartAxisTransformations;
    LogAxisTransform: TLogarithmAxisTransform;
    ChartListbox: TChartListbox;
    cbLogarithmic: TCheckBox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    cgCases: TCheckGroup;
    LeftPanel: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    TreeView: TTreeView;
    procedure btnClearClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure cgCasesItemClick(Sender: TObject; Index: integer);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure cbLogarithmicChange(Sender: TObject);
    procedure ChartToolset1DataPointCrosshairTool1Draw(
      ASender: TDataPointDrawTool);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
  private
    procedure GetDataString(ANode: TTreeNode; ADataType: TDataType; var AHeader, ACounts: String);
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState: String);
    function GetSeries(ANode: TTreeNode; ADataType: TDataType): TLineSeries;

    procedure LoadLocations;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  FpHttpClient, OpenSSL,
  TATypes, TACustomSource;

const
  DATATYPE_NAMES: array [TDataType] of string = ('confirmed', 'deaths', 'recovered');

  BASE_URL = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/';
  FILENAME_CONFIRMED = 'time_series_19-covid-Confirmed.csv';
  FILENAME_DEATHS = 'time_series_19-covid-Deaths.csv';
  FILENAME_RECOVERED = 'time_series_19-covid-Recovered.csv';

  DATA_DIR = 'data/';

  COLORS: array[0..8] of TColor = (clRed, clBlue, clFuchsia, clLime, clTeal,
    clSkyBlue, clPurple, clBlack, clMedGray);

function BeginsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[1] = '"');
end;

function EndsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[Length(s)] = '"');
end;

function DownloadFile(const Url: string; AStream: TStream): Boolean;
var
  http: TFpHttpClient;
begin
  Result := true;
  InitSSLInterface;
  http := TFpHttpClient.Create(nil);
  try
    http.AllowRedirect := true;
    http.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
    try
      http.Get(Url, AStream);
      Result := (http.ResponseStatusCode = 200);
    except
      on EInOutError do raise;
      on EHTTPClient do Result := false;
    end;
    AStream.Position := 0;
  finally
    http.Free;
  end;
end;


{ TMainForm }

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  Chart.ClearSeries;
end;

procedure TMainForm.btnUpdateClick(Sender: TObject);
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    Statusbar.SimpleText := 'Download from ' + BASE_URL + FILENAME_CONFIRMED + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_CONFIRMED, stream);
    stream.Position := 0;
    stream.SaveToFile(DATA_DIR + FILENAME_CONFIRMED);

    stream.Position := 0;
    Statusbar.SimpleText := 'Download from ' + BASE_URL + FILENAME_DEATHS + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_DEATHS, stream);
    stream.Position := 0;
    stream.SaveToFile(DATA_DIR + FILENAME_DEATHS);

    stream.Position := 0;
    Statusbar.SimpleText := 'Download from ' + BASE_URL + FILENAME_RECOVERED + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_RECOVERED, stream);
    stream.Position := 0;
    stream.SavetoFile(DATA_DIR + FILENAME_RECOVERED);

    LoadLocations;
  finally
    stream.Free;
  end;
end;

procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
var
  dt: TDataType;
begin
  if (not ASeries.Active) and (TChartSeries(ASeries).Count = 0) then
    for dt in TDataType do
      if (pos(DATATYPE_NAMES[dt], ASeries.Title) > 0) then
      begin
        ASkip := true;
        exit;
      end;
end;

procedure TMainForm.ChartToolset1DataPointCrosshairTool1Draw(
  ASender: TDataPointDrawTool);
var
  ser: TLineSeries;
  x, y: Double;
  sy: String;
begin
  if ASender.Series = nil then
    Statusbar.SimpleText := ''
  else
  if ASender.Series is TLineSeries then
  begin
    ser := TLineSeries(ASender.Series);
    x := ser.GetXValue(ASender.PointIndex);
    y := ser.GetYValue(ASender.PointIndex);
    if y = 1 then
      sy := '1 case'
    else
      sy := Format('%.0f cases', [y]);
    Statusbar.SimpleText := Format('%s: %s, %s', [ser.Title, DateToStr(x), sy]);
  end;
end;

procedure TMainForm.cbLogarithmicChange(Sender: TObject);
begin
  if cbLogarithmic.Checked then
  begin
    with Chart.LeftAxis do begin
      Transformations := LeftAxisTransformations;
      Intervals.Options := Intervals.Options + [aipGraphCoords];
      Intervals.MaxLength := 150;
      Intervals.MinLength := 50;
      Intervals.Tolerance := 100;
    end;
    Chart.Extent.UseYMin := true;
  end else
  begin
    with Chart.LeftAxis do begin
      Transformations := nil;
      Intervals.Options := Intervals.Options - [aipGraphCoords];
      Intervals.MaxLength := 50;
      Intervals.MinLength := 10;
      Intervals.Tolerance := 0;
    end;
    Chart.Extent.UseYMin := false;
  end;
end;

procedure TMainForm.cgCasesItemClick(Sender: TObject; Index: integer);
begin
  TreeViewClick(nil);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  LeftPanel.Constraints.MinWidth := cgCases.Width + btnUpdate.Width + 8;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cgCases.Checked[0] := true;
  LoadLocations;
end;

procedure TMainForm.GetDataString(ANode: TTreeNode; ADataType: TDataType;
  var AHeader, ACounts: String);
var
  L: TStrings;
  country, state: String;
  fn: String;
  i: integer;
  sa: TStringArray;
begin
  case ADataType of
    dtConfirmed : fn := DATA_DIR + FILENAME_CONFIRMED;
    dtDeaths    : fn := DATA_DIR + FILENAME_DEATHS;
    dtRecovered : fn := DATA_DIR + FILENAME_RECOVERED;
  end;
  GetLocation(ANode, country, state);

  L := TStringList.Create;
  try
    L.LoadFromFile(fn);
    AHeader := L[0];
    for i:=1 to L.Count-1 do begin
      if L[i] = '' then
        Continue;
      if BeginsWithQuote(L[i]) then
        Continue;
      sa := L[i].Split(',', '"');
      sa[0] := AnsiDequotedStr(sa[0], '"');
      sa[1] := AnsiDequotedStr(sa[1], '"');
      if (sa[1] = country) and (sa[0] = state) then
      begin
        ACounts := L[i];
        exit;
      end;
    end;
  finally
    L.Free;
  end;
end;

procedure TMainForm.GetLocation(ANode: TTreeNode; out ACountry, AState: String);
begin
  if ANode.Parent = nil then begin
    ACountry := ANode.Text;
    AState := '';
  end else
  begin
    ACountry := ANode.Parent.Text;
    AState := ANode.Text;
  end;
end;

function TMainForm.GetLocation(ANode: TTreeNode): String;
var
  country, state: String;
begin
  GetLocation(ANode, country, state);
  if state = '' then
    Result := country
  else
    Result := country + ' / ' + state;
end;

function TMainForm.GetSeries(ANode: TTreeNode; ADataType: TDataType): TLineSeries;
var
  i: Integer;
  serTitle: String;
  ser: TLineSeries;
  dt: TDataType;
begin
  serTitle := Format('%s (%s)', [GetLocation(ANode), DATATYPE_NAMES[ADataType]]);

  for i:=0 to Chart.SeriesCount-1 do
    if (Chart.Series[i] is TLineSeries) then
    begin
      Result := TLineSeries(Chart.Series[i]);
      if (Result <> nil) and (pos(serTitle, Result.Title) = 1) then
      begin
        Result.Active := true;
        exit;
      end;
    end;

  // The node does not yet have associated series. Create 3 series for
  // the cases "confirmed", "deaths" and "recovered". Return the one for
  // ADatatype and hide the others.
  serTitle := GetLocation(ANode);
  for dt in TDataType do begin
    ser := TLineSeries.Create(Chart);
    ser.Title := serTitle + ' (' + DATATYPE_NAMES[dt] + ')';
    ser.AxisIndexX := 1;
    ser.AxisIndexY := 0;
    ser.ShowPoints := true;
    ser.LinePen.Color := COLORS[(Chart.SeriesCount div 3) mod Length(COLORS)];
    ser.Pointer.Pen.Color := ser.LinePen.Color;
    case dt of
      dtConfirmed:
        begin
          ser.Pointer.Style := psCircle;
          ser.Pointer.Brush.Color := ser.LinePen.Color;
        end;
      dtDeaths:
        begin
          ser.Pointer.Style := psCross;
        end;
      dtRecovered:
        begin
          ser.Pointer.Style := psRectangle;
          ser.Pointer.Brush.Color := clWhite;
        end;
    end;
    if dt = ADataType then
      Result := ser
    else
      ser.Active := false;
    Chart.AddSeries(ser);
  end;
end;

procedure TMainForm.LoadLocations;
var
  fn: String;
  L: TStrings;
  i: Integer;
  sa: TStringArray;
  node: TTreeNode;
begin
  fn := DATA_DIR + FILENAME_CONFIRMED;
  if not FileExists(fn) then
    exit;

  L := TStringList.Create;
  try
    L.LoadfromFile(fn);
    TreeView.Items.BeginUpdate;
    try
      TreeView.Items.Clear;
      for i:=1 to L.Count-1 do begin
        if L[i] = '' then
          Continue;
        if BeginsWithQuote(L[i]) then
          Continue;
        sa := L[i].Split(',', '"');
        sa[0] := AnsiDequotedStr(sa[0], '"');
        sa[1] := AnsiDequotedStr(sa[1], '"');
        node := TreeView.Items.FindTopLvlNode(sa[1]);
        if node = nil then
          node := TreeView.Items.AddChild(nil, sa[1]);
        if sa[0] <> '' then
          node := TreeView.Items.AddChild(node, sa[0]);
      end;
    finally
      TreeView.AlphaSort;
      TreeView.Items.EndUpdate;
      Statusbar.SimpleText := 'Locations loaded.';
    end;
  finally
    L.Free;
  end;
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
var
  fs: TFormatSettings;
  serTitle: String;
  counts: array[TDataType] of string = ('', '', '');
  dates: array[TDataType] of string = ('', '', '');
  dt: TDataType;
  saX, saY: TStringArray;
  i: Integer;
  ser: TLineSeries;
  Y: Double;
begin
  if TreeView.Selected = nil then
    exit;
  if (TreeView.Selected.Parent = nil) and TreeView.Selected.HasChildren then
    exit;

  fs := FormatSettings;
  fs.ShortDateFormat := 'mm/dd/yyy';
  fs.DateSeparator := '/';

  serTitle := GetLocation(TreeView.Selected);
  for dt := Low(TDataType) to High(TDataType) do
  begin
    if cgCases.Checked[ord(dt)] then
    begin
      GetDataString(TreeView.Selected, dt, dates[dt], counts[dt]);
      saX := dates[dt].Split(',', '"');
      saY := counts[dt].Split(',','"');
      ser := GetSeries(TreeView.Selected, dt);
      ser.ListSource.BeginUpdate;
      try
        ser.Clear;
        for i := 4 to High(say) do
        begin
          Y := StrToInt(saY[i]);
          if Y = 0 then Y := 0.1;
          ser.AddXY(StrToDate(saX[i], fs), Y);
        end;
      finally
        ser.EndUpdate;
      end;
    end else
    begin
      dates[dt] := '';
      counts[dt] := '';
    end;
  end;
end;
(*



  L: TStrings;
  dates: TStringArray;
  sa: TStringArray;
  country, state: String;
  node: TTreeNode;
  values: array of Integer;
  fs: TFormatSettings;
  i,j: Integer;
  ser: TLineSeries;
  serTitle: String;
begin
  if TreeView.Selected = nil then
    exit;

  if (TreeView.Selected.Parent = nil) and Treeview.Selected.HasChildren then
    exit;

  fs := FormatSettings;
  fs.ShortDateFormat := 'm/dd/yy';
  fs.DateSeparator := '/';

  GetLocation(TreeView.Selected, country, state);

  if (country = state) or (state = '') then
    serTitle := country
  else
    serTitle := country + ' / ' + state;

  L := TStringList.Create;
  try
    L.LoadFromFile(DATA_DIR + FILENAME_CONFIRMED);
    dates := L[0].Split(',');
    for i:=1 to L.Count-1 do begin
      if L[i] = '' then
        Continue;
      if BeginsWithQuote(L[i]) then
        Continue;
      sa := L[i].Split(',', '"');
      sa[0] := AnsiDequotedStr(sa[0], '"');
      sa[1] := AnsiDequotedStr(sa[1], '"');
      if sa[1] <> country then Continue;
      if sa[0] <> state then Continue;

      ser := nil;
      for j := 0 to Chart.SeriesCount-1 do begin
        if (Chart.Series[j] is TLineSeries) and (TLineSeries(Chart.Series[j]).Title = serTitle) then
        begin
          ser := TLineSeries(Chart.Series[j]);
          ser.Clear;
          break;
        end;
      end;
      if ser = nil then begin
        ser := TLineSeries.Create(Chart);
        ser.Title := serTitle;
        ser.ShowPoints := true;
        ser.Pointer.Style := psCircle;
        ser.Pointer.Brush.Color := COLORS[Chart.SeriesCount mod Length(COLORS)];
        ser.LinePen.Color := COLORS[Chart.SeriesCount mod Length(COLORS)];
        Chart.AddSeries(ser);
      end;

      for j := 4 to High(sa) do
        ser.AddXY(StrToDate(dates[j], fs), StrToInt(sa[j]));
    end;
  finally
    L.Free;
  end;
end;
   *)

end.

