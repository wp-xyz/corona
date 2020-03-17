unit cMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, TAGraph, TAIntervalSources, TASeries, TAChartListbox,
  TALegend, TACustomSeries, TATransformations, TATools, TAFuncSeries,
  TADataTools, Types;

type
  TDataType = (dtConfirmed, dtDeaths, dtRecovered);

  { TMainForm }

  TMainForm = class(TForm)
    btnUpdate: TButton;
    btnClear: TButton;
    Chart: TChart;
    ChartToolset: TChartToolset;
    cbCumulative: TCheckBox;
    PanDragTool: TPanDragTool;
    ZoomDragTool: TZoomDragTool;
    MeasurementTool: TDataPointDistanceTool;
    UserdefinedTool: TUserDefinedTool;
    CrossHairTool: TDataPointCrosshairTool;
    ImageList1: TImageList;
    LeftAxisTransformations: TChartAxisTransformations;
    LogAxisTransform: TLogarithmAxisTransform;
    ChartListbox: TChartListbox;
    cbLogarithmic: TCheckBox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    cgCases: TCheckGroup;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    btnAbout: TSpeedButton;
    LeftSplitter: TSplitter;
    RightSplitter: TSplitter;
    StatusBar: TStatusBar;
    TreeView: TTreeView;
    procedure btnAboutClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure cbCumulativeChange(Sender: TObject);
    procedure cbLogarithmicChange(Sender: TObject);
    procedure cgCasesItemClick(Sender: TObject; Index: integer);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListboxCheckboxClick(ASender: TObject; AIndex: Integer);
    procedure CrossHairToolDraw(ASender: TDataPointDrawTool);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure MeasurementToolGetDistanceText(ASender: TDataPointDistanceTool;
      var AText: String);
    procedure MeasurementToolMeasure(ASender: TDataPointDistanceTool);
    procedure TreeViewClick(Sender: TObject);

  private
    FMeasurementSeries: TFuncSeries;
    FFitCoeffs: array[0..1] of Double;
    function CalcFit(ASeries: TBasicChartSeries; xmin, xmax: Double): Boolean;
    procedure CalcFitHandler(const AX: Double; out AY: Double);
    procedure Clear;
    procedure CreateMeasurementSeries;
    procedure GetDataString(ANode: TTreeNode; ADataType: TDataType; var AHeader, ACounts: String);
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState: String);
    function GetSeries(ANode: TTreeNode; ADataType: TDataType): TChartSeries;
    procedure LayoutBars;
    procedure LoadLocations;
    procedure Logarithmic(AEnable: Boolean);
    procedure UpdateAffectedSeries;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  FpHttpClient, OpenSSL, Math,
  TATypes, TAMath, TAFitUtils, TACustomSource, TAFitLib,
  cAbout;

const
  DATATYPE_NAMES: array [TDataType] of string = ('confirmed', 'deaths', 'recovered');

  BASE_URL = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/';
  FILENAME_CONFIRMED = 'time_series_19-covid-Confirmed.csv';
  FILENAME_DEATHS = 'time_series_19-covid-Deaths.csv';
  FILENAME_RECOVERED = 'time_series_19-covid-Recovered.csv';

  DATA_DIR = 'data/';

  COLORS: array[0..8] of TColor = (clRed, clBlue, clFuchsia, clLime, clTeal,
    clSkyBlue, clPurple, clBlack, clMedGray);

  BARWIDTH_PERCENT = 80;

var
  BaseDate: TDate;

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

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  with TAboutForm.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TMainForm.btnUpdateClick(Sender: TObject);
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    Statusbar.Panels[1].Text := 'Download from ' + BASE_URL + FILENAME_CONFIRMED + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_CONFIRMED, stream);
    stream.Position := 0;
    stream.SaveToFile(DATA_DIR + FILENAME_CONFIRMED);

    stream.Position := 0;
    Statusbar.Panels[1].Text := 'Download from ' + BASE_URL + FILENAME_DEATHS + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_DEATHS, stream);
    stream.Position := 0;
    stream.SaveToFile(DATA_DIR + FILENAME_DEATHS);

    stream.Position := 0;
    Statusbar.Panels[1].Text := 'Download from ' + BASE_URL + FILENAME_RECOVERED + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_RECOVERED, stream);
    stream.Position := 0;
    stream.SavetoFile(DATA_DIR + FILENAME_RECOVERED);

    LoadLocations;
  finally
    stream.Free;
  end;
end;

// It is assumed that xmin < xmax.
function TMainForm.CalcFit(ASeries: TBasicChartSeries;
  xmin, xmax: Double): Boolean;
const
  EPS = 1E-9;
var
  x, y: TArbFloatArray;
  n, i: Integer;
  xval, yval: Double;
  fitParams: TFitParamArray;
  fitRes: TFitResults;
  ser: TChartSeries;
begin
  Result := false;
  FFitCoeffs[0] := NaN;
  FFitCoeffs[1] := NaN;

  if not (ASeries is TChartSeries) then
    exit;
  ser := TChartSeries(ASeries);
  if ser.Count = 0 then
    exit;

  SetLength(x, ser.Count);
  SetLength(y, ser.Count);
  n := 0;
  for i := 0 to ser.Count-1 do begin
    xval := ser.XValue[i];
    if not SameValue(xval, xmax, EPS) and (xval > xmax) then
      break;
    if not SameValue(xval, xmin) and (xval < xmin) then
      continue;
    yval := ser.YValue[i];
    if yval <= 0 then
      continue;
    x[n] := xval - BaseDate;
    y[n] := ln(yval);
    inc(n);
  end;
  SetLength(x, n);
  SetLength(y, n);
  if n < 2 then
    exit;

  SetLength(fitParams, 2);
  for i := 0 to High(fitParams) do begin
    fitParams[i].Fixed := false;
    fitParams[i].Value := 0.0; //NaN;
    fitParams[i].Func := @FitBaseFunc_Poly
  end;

  fitRes := LinearFit(x, y, nil, fitParams);

  if fitRes.ErrCode <> fitOK then
    exit;

  // These are the coeffs for the LINEAR fit, i.e. coeff[0] still must be transformed.
  FFitCoeffs[0] := fitRes.ParamValues[0];
  FFitCoeffs[1] := fitRes.ParamValues[1];

  Result := true;
end;

procedure TMainForm.CalcFitHandler(const AX: Double; out AY: Double);
begin
  if not (IsNaN(FFitCoeffs[0]) or IsNaN(FFitCoeffs[1])) then
    AY := FFitCoeffs[0] * exp(FFitCoeffs[1] * (AX - BaseDate))
  else
    AY := NaN;
end;

procedure TMainForm.cbCumulativeChange(Sender: TObject);
begin
  Clear;
  cbLogarithmic.Enabled := cbCumulative.Checked;
  if cbCumulative.Checked then
  begin
    Chart.LeftAxis.Title.Caption := 'Cases';
    Logarithmic(cbLogarithmic.Checked);
    MeasurementTool.Enabled := true;
  end else
  begin
    Chart.LeftAxis.Title.Caption := 'New Cases';
    Logarithmic(false);
    MeasurementTool.Enabled := false;
  end;
end;

procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
var
  dt: TDataType;
begin
  if ASeries is TFuncSeries then begin
    ASkip := true;
    exit;
  end;

  if (not ASeries.Active) and (TLineSeries(ASeries).Count = 0) then
    for dt in TDataType do
      if (pos(DATATYPE_NAMES[dt], ASeries.Title) > 0) then
      begin
        ASkip := true;
        exit;
      end;
end;

procedure TMainForm.ChartListboxCheckboxClick(ASender: TObject; AIndex: Integer);
begin
  LayoutBars;
  UpdateAffectedSeries;
end;

procedure TMainForm.CrossHairToolDraw(
  ASender: TDataPointDrawTool);
const
  NEW_: array[boolean] of string = ('new ', '');
var
  ser: TChartSeries;
  x, y: Double;
  sy: String;
begin
  if ASender.Series = nil then
    Statusbar.Panels[0].Text := ''
  else
  if ASender.Series is TChartSeries then
  begin
    ser := TChartSeries(ASender.Series);
    x := ser.GetXValue(ASender.PointIndex);
    y := ser.GetYValue(ASender.PointIndex);
    if y = 1 then
      sy := Format('1 %scase', [NEW_[cbCumulative.Checked]])
    else
      sy := Format('%.0f %scases', [y, NEW_[cbCumulative.Checked]]);
    Statusbar.Panels[0].Text := Format('%s: %s, %s', [ser.Title, DateToStr(x), sy]);
    ASender.Handled;
  end;
end;

procedure TMainForm.cbLogarithmicChange(Sender: TObject);
begin
  Logarithmic(cbLogarithmic.Checked);
end;

procedure TMainForm.cgCasesItemClick(Sender: TObject; Index: integer);
begin
  TreeViewClick(nil);
end;

procedure TMainForm.Clear;
begin
  TreeView.Selected := nil;
  Chart.ClearSeries;
  CreateMeasurementSeries;
end;

procedure TMainForm.CreateMeasurementSeries;
begin
  FMeasurementSeries := TFuncSeries.Create(Chart);
  FMeasurementSeries.Active := false;
  FMeasurementSeries.OnCalculate := @CalcFitHandler;
  FMeasurementSeries.Legend.Visible := false;
  FMeasurementSeries.AxisIndexX := 1;
  FMeasurementseries.AxisIndexY := 0;
  FMeasurementSeries.Pen.Width := 3;
  FMeasurementSeries.Pen.Color := clLime;
  Chart.AddSeries(FMeasurementSeries);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  LeftPanel.Constraints.MinWidth := cgCases.Width + btnUpdate.Width + 8;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  cgCases.Checked[0] := true;
  CreateMeasurementSeries;
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

function TMainForm.GetSeries(ANode: TTreeNode; ADataType: TDataType): TChartSeries;
var
  i: Integer;
  serTitle: String;
  ser: TChartSeries;
  dt: TDataType;
begin
  serTitle := Format('%s (%s)', [GetLocation(ANode), DATATYPE_NAMES[ADataType]]);

  for i:=0 to Chart.SeriesCount-1 do
    if (Chart.Series[i] is TChartSeries) then
    begin
      Result := TChartSeries(Chart.Series[i]);
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
    if cbCumulative.Checked then
    begin
      ser := TLineSeries.Create(Chart);
      with TLineSeries(ser) do
      begin
        ShowPoints := true;
        LinePen.Color := COLORS[((Chart.SeriesCount - 1) div 3) mod Length(COLORS)];
        Pointer.Pen.Color := LinePen.Color;
        case dt of
          dtConfirmed:
            begin
              Pointer.Style := psCircle;
              Pointer.Brush.Color := LinePen.Color;
            end;
          dtDeaths:
            begin
              Pointer.Style := psCross;
            end;
          dtRecovered:
            begin
              Pointer.Style := psRectangle;
              Pointer.Brush.Color := clWhite;
            end;
        end;
      end;
    end else
    begin
      ser := TBarSeries.Create(Chart);
      with TBarSeries(ser) do begin
        BarBrush.Color := COLORS[((Chart.SeriesCount-1) div 3) mod Length(COLORS)];
        BarBrush.Style := bsSolid;
        BarPen.Color := BarBrush.Color;
        case dt of
          dtConfirmed : BarBrush.Style := bsSolid;
          dtDeaths    : BarBrush.Color := clWhite;
          dtRecovered : BarBrush.Style := bsDiagCross;
        end;
      end;
    end;
    ser.Title := serTitle + ' (' + DATATYPE_NAMES[dt] + ')';
    ser.AxisIndexX := 1;
    ser.AxisIndexY := 0;
    if dt = ADataType then
      Result := ser
    else
      ser.Active := false;
    Chart.AddSeries(ser);
  end;

  UpdateAffectedSeries;
end;

procedure TMainForm.LayoutBars;
var
  i, j, n: Integer;
  ser: TBarSeries;
begin
  if cbCumulative.Checked then
    exit;

  n := 0;
  for i := 0 to Chart.SeriesCount-1 do
    if (Chart.Series[i] is TBarSeries) and Chart.Series[i].Active then
      inc(n);

  j := 0;
  for i:=0 to Chart.SeriesCount-1 do
    if (Chart.Series[i] is TBarSeries) and Chart.Series[i].Active then
    begin
      ser := TBarSeries(Chart.Series[i]);
      ser.BarWidthPercent := round(BARWIDTH_PERCENT / n);
      ser.BarOffsetPercent := round((j - (n-1)/2) * ser.BarWidthPercent);
      inc(j);
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
      Statusbar.Panels[0].Text := 'Locations loaded.';
      Statusbar.Panels[1].Text := '';
    end;
  finally
    L.Free;
  end;
end;

procedure TMainForm.Logarithmic(AEnable: Boolean);
begin
  if AEnable then
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

procedure TMainForm.MeasurementToolAfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
begin
  FMeasurementSeries.Active := false;
  Statusbar.Panels[1].Text := '';
//  ATool.Handled;
end;

procedure TMainForm.MeasurementToolGetDistanceText(
  ASender: TDataPointDistanceTool; var AText: String);
var
  min, max: Double;
  ok: Boolean;
  s: String;
begin
  AText := '';

  min := ASender.PointStart.GraphPos.X;
  max := ASender.PointEnd.GraphPos.X;
  if min = max then
    exit;
  EnsureOrder(min, max);

  if not (ASender.PointStart.Series is TChartSeries) then
    exit;

  ok := false;
  s := '';
  if CalcFit(ASender.PointStart.Series, min, max) then
    if (FFitCoeffs[1] <> 0) then
    begin
      FFitCoeffs[0] := exp(FFitCoeffs[0]);
      FMeasurementSeries.DomainExclusions.Clear;
      FMeasurementSeries.DomainExclusions.AddRange(-Infinity, min);
      FMeasurementSeries.DomainExclusions.AddRange(max, Infinity);
      s := Format('Doubles every %.1f days', [-ln(0.5) / FFitCoeffs[1]]);
      ok := true;
    end;
  FMeasurementSeries.Active := ok;
  Statusbar.Panels[1].Text := s;
  Statusbar.Refresh;
end;

procedure TMainForm.MeasurementToolMeasure(ASender: TDataPointDistanceTool);
begin
  FMeasurementSeries.Active := false;
//  Statusbar.Panels[1].Text := '';
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
var
  fs: TFormatSettings;
  counts: array[TDataType] of string = ('', '', '');
  dates: array[TDataType] of string = ('', '', '');
  dt: TDataType;
  saX, saY: TStringArray;
  i: Integer;
  ser: TChartSeries;
  Y, Y0: Double;
begin
  if TreeView.Selected = nil then
    exit;
  if (TreeView.Selected.Parent = nil) and TreeView.Selected.HasChildren then
    exit;

  fs := FormatSettings;
  fs.ShortDateFormat := 'mm/dd/yyy';
  fs.DateSeparator := '/';

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
        if cbCumulative.Checked then begin
          for i := 4 to High(saY) do
          begin
            Y := StrToInt(saY[i]);
            if Y = 0 then Y := 0.1;
            ser.AddXY(StrToDate(saX[i], fs), Y);
          end;
        end else begin
          Y0 := StrToInt(saY[4]);
          for i := 5 to High(saY) do begin
            Y := StrToInt(saY[i]);
            ser.AddXY(StrToDate(saX[i], fs), Y - Y0);
            Y0 := Y;
          end;
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
  LayoutBars;
  UpdateAffectedSeries;
end;

procedure TMainForm.UpdateAffectedSeries;
var
  i: Integer;
  s: String;
begin
  s := '';
  for i := 0 to Chart.SeriesCount-1 do
    if (Chart.Series[i] is TChartSeries) and Chart.Series[i].Active then
      s := s + ',' + IntToStr(Chart.Series[i].Index);
  Delete(s, 1, 1);
  CrossHairTool.AffectedSeries := s;
  MeasurementTool.AffectedSeries := s;
end;

initialization
  BaseDate := EncodeDate(2020, 1, 1);

end.

