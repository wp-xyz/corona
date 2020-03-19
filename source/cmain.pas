unit cMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, Grids, Types, LCLVersion,
  TAGraph, TAIntervalSources, TASeries, TAChartListbox, TALegend,
  TACustomSeries, TATransformations, TATools, TAFuncSeries, TADataTools;

type
  TCaseType = (ctConfirmed, ctDeaths, ctRecovered);
  TDataType = (dtCumulative, dtNewCases, dtNewCaseRatio);

  { TMainForm }

  TMainForm = class(TForm)
    btnUpdate: TButton;
    btnClear: TButton;
    btnSaveToCSV: TButton;
    Chart: TChart;
    ChartToolset: TChartToolset;
    WheelZoomTool: TZoomMouseWheelTool;
    Grid: TDrawGrid;
    lblTableHdr: TLabel;
    PageControl: TPageControl;
    PanDragTool: TPanDragTool;
    pgChart: TTabSheet;
    pgTable: TTabSheet;
    rgDataType: TRadioGroup;
    SaveDialog: TSaveDialog;
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
    procedure btnSaveToCSVClick(Sender: TObject);
    procedure cbCumulativeChange(Sender: TObject);
    procedure cbLogarithmicChange(Sender: TObject);
    procedure cgCasesItemClick(Sender: TObject; Index: integer);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListboxCheckboxClick(ASender: TObject; AIndex: Integer);
    procedure CrossHairToolDraw(ASender: TDataPointDrawTool);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure MeasurementToolGetDistanceText(ASender: TDataPointDistanceTool;
      var AText: String);
    procedure MeasurementToolMeasure(ASender: TDataPointDistanceTool);
    procedure rgDataTypeClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);

  private
    FMeasurementSeries: TFuncSeries;
    FFitCoeffs: array[0..1] of Double;
    function CalcFit(ASeries: TBasicChartSeries; xmin, xmax: Double): Boolean;
    procedure CalcFitHandler(const AX: Double; out AY: Double);
    procedure Clear;
    procedure CreateMeasurementSeries;
    function GetCellText(ACol, ARow: Integer): String;
    function GetDataString(ANode: TTreeNode; ACaseType: TCaseType; var AHeader, ACounts: String): Boolean;
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState: String);
    function GetSeries(ANode: TTreeNode; ACaseType: TCaseType; ADataType: TDataType): TChartSeries;
    procedure LayoutBars;
    procedure LoadLocations;
    procedure Logarithmic(AEnable: Boolean);
    procedure UpdateAffectedSeries;
    procedure UpdateGrid;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  FpHttpClient, OpenSSL, Math,
  TATypes, TAMath, TACustomSource, TAFitLib,
  cAbout;

const
  CASETYPE_NAMES: array [TCaseType] of string = ('confirmed', 'deaths', 'recovered');

  BASE_URL = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/';
  FILENAME_CONFIRMED = 'time_series_19-covid-Confirmed.csv';
  FILENAME_DEATHS = 'time_series_19-covid-Deaths.csv';
  FILENAME_RECOVERED = 'time_series_19-covid-Recovered.csv';

  // DATA_DIR must end with path delimiter!
  {$IFDEF DARWIN}
  DATA_DIR = '../../../data/';
  {$ELSE}
  DATA_DIR = 'data/';
  {$ENDIF}

  COLORS: array[0..8] of TColor = (clRed, clBlue, clFuchsia, clLime, clTeal,
    clSkyBlue, clPurple, clBlack, clMedGray);

  BARWIDTH_PERCENT = 80;

var
  BaseDate: TDate;
  DataDir: String;

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
    Statusbar.Panels[0].Text := 'Download from:';
    Statusbar.Panels[1].Text := BASE_URL + FILENAME_CONFIRMED + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_CONFIRMED, stream);
    stream.Position := 0;
    stream.SaveToFile(DataDir + FILENAME_CONFIRMED);

    stream.Position := 0;
    Statusbar.Panels[1].Text := BASE_URL + FILENAME_DEATHS + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_DEATHS, stream);
    stream.Position := 0;
    stream.SaveToFile(DataDir + FILENAME_DEATHS);

    stream.Position := 0;
    Statusbar.Panels[1].Text := BASE_URL + FILENAME_RECOVERED + '...';
    Statusbar.Refresh;
    DownloadFile(BASE_URL + FILENAME_RECOVERED, stream);
    stream.Position := 0;
    stream.SaveToFile(DataDir + FILENAME_RECOVERED);

    LoadLocations;
  finally
    Statusbar.Panels[0].Text := 'Locations loaded.';
    Statusbar.Panels[1].Text := '';
    stream.Free;
  end;
end;

procedure TMainForm.btnSaveToCSVClick(Sender: TObject);
var
  r, c: Integer;
  F: TextFile;
  col: TGridColumn;
  ser: TChartSeries;
begin
  SaveDialog.Filename := '';
  if SaveDialog.Execute then
  begin
    AssignFile(F, SaveDialog.FileName);
    Rewrite(F);

    Write(F, 'Date');
    for c := 1 to Grid.ColCount-1 do
    begin
      col := Grid.Columns[c-1];
      ser := TChartSeries(col.Tag);
      Write(F, #9, ser.Title);
    end;
    WriteLn(F);

    for r := 1 to Grid.RowCount-1 do
    begin
      Write(F, GetCellText(0, r));
      for c := 1 to Grid.ColCount-1 do
        Write(F, #9, GetCellText(c, r));
      WriteLn(F);
    end;
    CloseFile(F);
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
  cbLogarithmic.Enabled := TDataType(rgDataType.ItemIndex) = dtCumulative;
  case TDataType(rgDataType.ItemIndex) of
    dtCumulative:
      begin
        Chart.LeftAxis.Title.Caption := 'Cases';
        lblTableHdr.Caption := 'Cumulative Cases';
        cbLogarithmic.Enabled := true;
        Logarithmic(cbLogarithmic.Checked);
        MeasurementTool.Enabled := true;
      end;
    dtNewCases:
      begin
        Chart.LeftAxis.Title.Caption := 'New Cases';
        lblTableHdr.Caption := 'New Cases';
        cbLogarithmic.Enabled := false;
        Logarithmic(false);
        MeasurementTool.Enabled := false;
      end;
    dtNewCaseRatio:
      begin
        Chart.LeftAxis.Title.Caption := 'Day-to-day ratio of new cases';
        lblTableHdr.Caption := 'Ratio of new cases';
        cbLogarithmic.Enabled := false;
        Logarithmic(false);
        MeasurementTool.Enabled := false;
      end;
  end;
end;

procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
var
  ct: TCaseType;
begin
  if ASeries is TFuncSeries then begin
    ASkip := true;
    exit;
  end;

  if (not ASeries.Active) and (TLineSeries(ASeries).Count = 0) then
    for ct in TCaseType do
      if (pos(CASETYPE_NAMES[ct], ASeries.Title) > 0) then
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
var
  ser: TChartSeries;
  x, y: Double;
  sy: String;
  dt: TDataType;
begin
  if ASender.Series = nil then
    Statusbar.Panels[0].Text := ''
  else
  if ASender.Series is TChartSeries then
  begin
    dt := TDataType(rgDataType.ItemIndex);
    ser := TChartSeries(ASender.Series);
    x := ser.GetXValue(ASender.PointIndex);
    y := ser.GetYValue(ASender.PointIndex);
    case dt of
      dtCumulative:
        if y = 1 then sy := '1 case' else sy := Format('%.0n cases', [y]);
      dtNewCases:
        if y = 1 then sy := '1 new case' else sy := Format('%.0n new cases', [y]);
      dtNewCaseRatio:
        sy := Format('cases(%s) / cases(%s) = %.3f', [DateToStr(x), DateToStr(x-1), y])
    end;
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
  UpdateGrid;
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
  DataDir := Application.Location + DATA_DIR;  // DATA_DIR ends with a path delimiter
  WheelZoomTool.ZoomFactor := 1.05;
  WheelZoomTool.ZoomRatio := 1.0 / WheelZoomTool.ZoomFactor;
  {$IF LCL_FullVersion >= 2010000}
  ZoomDragTool.LimitToExtent := [zdDown, zdLeft, zdRight, zdUp];
  PanDragTool.LimitToExtent := [pdDown, pdLeft, pdRight, pdUp];
  {$ENDIF}
  cgCases.Checked[0] := true;
  CreateMeasurementSeries;
  LoadLocations;
end;

function TMainForm.GetCellText(ACol, ARow: Integer): String;
var
  s: String;
  col: TGridColumn;
  ser: TChartSeries;
  r: Integer;
begin
  if (ACol = 0) and (ARow = 0) then
    Result := 'Date'
  else
  begin
    Result := '';
    if Grid.Columns.Count = 0 then
      exit;
    if (ACol = 0) then
    begin
      col := Grid.Columns[0];
      ser := TChartSeries(col.Tag);
      r := ser.Count - ARow;
      Result := DateToStr(ser.XValue[r]);
    end else
    if ARow > 0 then
    begin
      col := Grid.Columns[ACol - 1];
      ser := TChartSeries(col.Tag);
      r := ser.Count - ARow;
      if not IsNan(ser.YValue[r]) then
      begin
        if TDataType(rgDataType.ItemIndex) = dtNewCaseRatio then
          Result := format('%.3f', [ser.YValue[r]])
        else
          Result := Format('%.0n', [ser.YValue[r]]);
      end;
    end;
  end;
end;

function TMainForm.GetDataString(ANode: TTreeNode; ACaseType: TCaseType;
  var AHeader, ACounts: String): Boolean;
var
  L: TStrings;
  country, state: String;
  fn: String;
  i: integer;
  sa: TStringArray;
begin
  Result := false;

  case ACaseType of
    ctConfirmed : fn := DataDir + FILENAME_CONFIRMED;
    ctDeaths    : fn := DataDir + FILENAME_DEATHS;
    ctRecovered : fn := DataDir + FILENAME_RECOVERED;
  end;
  if not FileExists(fn) then
  begin
    MessageDlg(Format('Data file "%s" not found. Please press "Update files".', [fn]), mtError, [mbOK], 0);
    exit;
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
        Result := true;
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

function TMainForm.GetSeries(ANode: TTreeNode; ACaseType: TCaseType;
  ADataType: TDataType): TChartSeries;
var
  i: Integer;
  serTitle: String;
  ser: TChartSeries;
  ct: TCaseType;
begin
  serTitle := Format('%s (%s)', [GetLocation(ANode), CASETYPE_NAMES[ACaseType]]);

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
  for ct in TCaseType do begin
    case ADataType of
      dtCumulative,
      dtNewCaseRatio:
        begin
          ser := TLineSeries.Create(Chart);
          with TLineSeries(ser) do
          begin
            ShowPoints := true;
            LinePen.Color := COLORS[((Chart.SeriesCount - 1) div 3) mod Length(COLORS)];
            Pointer.Pen.Color := LinePen.Color;
            case ct of
              ctConfirmed:
                begin
                  Pointer.Style := psCircle;
                  Pointer.Brush.Color := LinePen.Color;
                end;
              ctDeaths:
                begin
                  Pointer.Style := psCross;
                end;
              ctRecovered:
                begin
                  Pointer.Style := psRectangle;
                  Pointer.Brush.Color := clWhite;
                end;
            end;
          end;
        end;
      dtNewCases:
        begin
          ser := TBarSeries.Create(Chart);
          with TBarSeries(ser) do
          begin
            BarBrush.Color := COLORS[((Chart.SeriesCount-1) div 3) mod Length(COLORS)];
            BarBrush.Style := bsSolid;
            BarPen.Color := BarBrush.Color;
            case ct of
              ctConfirmed : BarBrush.Style := bsSolid;
              ctDeaths    : BarBrush.Color := clWhite;
              ctRecovered : BarBrush.Style := bsDiagCross;
            end;
          end;
        end;
    end;  // case
    ser.Title := serTitle + ' (' + CASETYPE_NAMES[ct] + ')';
    ser.AxisIndexX := 1;
    ser.AxisIndexY := 0;
    if ct = ACaseType then
      Result := ser
    else
      ser.Active := false;
    Chart.AddSeries(ser);
  end;  // for ct

  UpdateAffectedSeries;
end;

procedure TMainForm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: String;
  R: TRect;
begin
  s := GetCellText(aCol, aRow);
  if s = '' then exit;
  R := ARect;
  InflateRect(R, -varCellpadding, -varCellpadding);
  Grid.Canvas.TextRect(R, R.Left, R.Top, s);
end;

procedure TMainForm.GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  ts := Grid.Canvas.TextStyle;
  if ARow = 0 then
  begin
    ts.WordBreak := true;
    ts.SingleLine := false;
    ts.Alignment := taCenter;
    ts.Layout := tlTop;
  end else
  if ACol = 0 then
    ts.Alignment := taCenter
  else
    ts.Alignment := taRightJustify;
  Grid.Canvas.TextStyle := ts;
end;

procedure TMainForm.LayoutBars;
var
  i, j, n: Integer;
  ser: TBarSeries;
begin
  if rgDataType.ItemIndex <> ord(dtNewCases) then
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
  if not DirectoryExists(DataDir) then
  begin
    CreateDir(DataDir);
    MessageDlg('Data directory created. Please click "Update files".', mtInformation, [mbOK], 0);
    exit;
  end;

  fn := DataDir + FILENAME_CONFIRMED;
  if not FileExists(fn) then
  begin
    MessageDlg('Data files not found. Please click "Update files".', mtError, [mbOK], 0);
    exit;
  end;

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
      Marks.Format := '%0:.0n';
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
      case TDataType(rgDataType.ItemIndex) of
        dtCumulative   : Marks.Format := '%0:.0n';
        dtNewCases     : Marks.Format := '%0:.9g';
        dtNewCaseRatio : Marks.Format := '%0:.9g';
      end;
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
  ser: TChartSeries;
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
      FMeasurementSeries.Active := false;
      (*
      FMeasurementSeries.DomainExclusions.Clear;
      FMeasurementSeries.DomainExclusions.AddRange(-Infinity, min);
      FMeasurementSeries.DomainExclusions.AddRange(max, Infinity);
      *)
      ser := TChartSeries(ASender.PointStart.Series);
      s := Format('Doubles every %.1f days --> %.0n cases in 1 week --> %.0n cases in 2 weeks', [
        -ln(0.5) / FFitCoeffs[1],
        exp(FFitCoeffs[1] * 7) * ser.YValue[ser.Count-1],
        exp(FFitCoeffs[1] * 14) * ser.YValue[ser.Count-1]
      ]);
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

procedure TMainForm.rgDataTypeClick(Sender: TObject);
begin
  Clear;
  cbLogarithmic.Enabled := TDataType(rgDataType.ItemIndex) = dtCumulative;
  case TDataType(rgDataType.ItemIndex) of
    dtCumulative:
      begin
        Chart.LeftAxis.Title.Caption := 'Cases';
        lblTableHdr.Caption := 'Cumulative Cases';
        cbLogarithmic.Enabled := true;
        Logarithmic(cbLogarithmic.Checked);
        MeasurementTool.Enabled := true;
      end;
    dtNewCases:
      begin
        Chart.LeftAxis.Title.Caption := 'New Cases';
        lblTableHdr.Caption := 'New Cases';
        cbLogarithmic.Enabled := false;
        Logarithmic(false);
        MeasurementTool.Enabled := false;
      end;
    dtNewCaseRatio:
      begin
        Chart.LeftAxis.Title.Caption := 'Day-to-day ratio of new cases';
        lblTableHdr.Caption := 'Ratio of new cases';
        cbLogarithmic.Enabled := false;
        Logarithmic(false);
        MeasurementTool.Enabled := false;
      end;
  end;
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
var
  fs: TFormatSettings;
  counts: array[TCaseType] of string = ('', '', '');
  dates: array[TCaseType] of string = ('', '', '');
  dt: TDataType;
  ct: TCaseType;
  saX, saY: TStringArray;
  i: Integer;
  ser: TChartSeries;
  Y, Y0, dY, dY0: Double;
begin
  if TreeView.Selected = nil then
    exit;
  if (TreeView.Selected.Parent = nil) and TreeView.Selected.HasChildren then
    exit;

  fs := FormatSettings;
  fs.ShortDateFormat := 'mm/dd/yyy';
  fs.DateSeparator := '/';

  dt := TDataType(rgDataType.ItemIndex);

  for ct := Low(TCaseType) to High(TCaseType) do
  begin
    if cgCases.Checked[ord(ct)] then
    begin
      if not GetDataString(TreeView.Selected, ct, dates[ct], counts[ct]) then
        exit;

      saX := dates[ct].Split(',', '"');
      saY := counts[ct].Split(',','"');
      ser := GetSeries(TreeView.Selected, ct, dt);
      ser.ListSource.BeginUpdate;
      try
        ser.Clear;
        case dt of
          dtCumulative:
            begin
              for i := 4 to High(saY) do
              begin
                Y := StrToInt(saY[i]);
                if Y = 0 then Y := 0.1;
                ser.AddXY(StrToDate(saX[i], fs), Y);
              end;
            end;
          dtNewCases:
            begin
              Y0 := StrToInt(saY[4]);
              for i := 5 to High(saY) do begin
                Y := StrToInt(saY[i]);
                ser.AddXY(StrToDate(saX[i], fs), Y - Y0);
                Y0 := Y;
              end;
            end;
          dtNewCaseRatio:
            begin
              Y0 := StrToInt(saY[4]);
              Y := StrToInt(saY[5]);
              dY0 := Y - Y0;
              for i := 6 to High(saY) do
              begin
                Y := StrToInt(saY[i]);
                dy := Y - Y0;
                if dy0 <> 0 then
                  ser.AddXY(StrToDate(saX[i], fs), dy / dy0)
                else
                  ser.AddXY(StrToDate(saX[i], fs), NaN);
                dy0 := dy;
                Y0 := Y;
              end;
            end;
        end;
      finally
        ser.EndUpdate;
      end;
    end else
    begin
      dates[ct] := '';
      counts[ct] := '';
    end;
  end;
  LayoutBars;
  UpdateAffectedSeries;
  UpdateGrid;
  Statusbar.Panels[0].Text := GetLocation(TreeView.Selected) + ' loaded.';
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

procedure TMainForm.UpdateGrid;
var
  n: Integer;
  i: Integer;
  ser: TChartSeries;
begin
  Grid.BeginUpdate;
  try
    Grid.Columns.Clear;
    n := 0;
    for i:=0 to Chart.SeriesCount-1 do
    begin
      if (Chart.Series[i] is TChartSeries) and Chart.Series[i].Active then
      begin
        ser := TChartSeries(Chart.Series[i]);
        inc(n);
        with Grid.Columns.Add do
        begin
          Title.Caption := StringReplace(ser.Title, ' ', LineEnding, []);
          Tag := PtrInt(Chart.Series[i]);
        end;
      end;
    end;
    if n = 0 then
    begin
      Grid.RowCount := 2;
      exit;
    end;
    Grid.RowCount := ser.Count + Grid.FixedRows;
    Grid.RowHeights[0] := 2 * Grid.DefaultRowHeight;
  finally
    Grid.EndUpdate;
  end;
end;

initialization
  BaseDate := EncodeDate(2020, 1, 1);

end.

