unit cMain;

{$mode objfpc}{$H+}

interface

uses
  LCLType,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, Grids, Types, LCLVersion, Menus, ActnList, StdActns,
  fpopenssl, ssockets, sslsockets,    // needed for SSL to work in general
  TAGraph, TAIntervalSources, TASeries, TAChartListbox, TALegend,
  TACustomSeries, TATransformations, TATools, TAFuncSeries, TADataTools;

type
  TCaseType = (ctConfirmed, ctDeaths, ctRecovered);
  TDataType = (dtCumulative, dtNewCases, dtNewCaseRatio);

  { TMainForm }

  TMainForm = class(TForm)
    acDataUpdate: TAction;
    acAbout: TAction;
    acDataClear: TAction;
    acChartLogarithmic: TAction;
    acConfigHint: TAction;
    acTableSave: TAction;
    acConfigAuto: TAction;
    ActionList: TActionList;
    btnClear: TButton;
    btnSaveToCSV: TButton;
    btnUpdate: TBitBtn;
    Chart: TChart;
    ChartToolset: TChartToolset;
    acFileExit: TFileExit;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    mnuTable: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuData: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuConfigHints: TMenuItem;
    mnuConfig: TMenuItem;
    mnuChartLogarithmic: TMenuItem;
    mnuChart: TMenuItem;
    mnuFileQuit: TMenuItem;
    mnuFile: TMenuItem;
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
    ImageList: TImageList;
    LeftAxisTransformations: TChartAxisTransformations;
    LogAxisTransform: TLogarithmAxisTransform;
    ChartListbox: TChartListbox;
    cbLogarithmic: TCheckBox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    cgCases: TCheckGroup;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    LeftSplitter: TSplitter;
    RightSplitter: TSplitter;
    StatusBar: TStatusBar;
    TreeView: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acChartLogarithmicExecute(Sender: TObject);
    procedure acConfigAutoExecute(Sender: TObject);
    procedure acConfigHintExecute(Sender: TObject);
    procedure acDataClearExecute(Sender: TObject);
    procedure acDataUpdateExecute(Sender: TObject);
    procedure acTableSaveExecute(Sender: TObject);
    procedure cgCasesItemClick(Sender: TObject; Index: integer);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListboxCheckboxClick(ASender: TObject; AIndex: Integer);
    procedure CrossHairToolDraw(ASender: TDataPointDrawTool);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure MeasurementToolGetDistanceText(ASender: TDataPointDistanceTool;
      var AText: String);
    procedure MeasurementToolMeasure(ASender: TDataPointDistanceTool);
    procedure PageControlChange(Sender: TObject);
    procedure rgDataTypeClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);

  private
    FMeasurementSeries: TFuncSeries;
    FFitCoeffs: array[0..1] of Double;
    FStatusText1, FStatusText2: String;
    function CalcFit(ASeries: TBasicChartSeries; xmin, xmax: Double): Boolean;
    procedure CalcFitHandler(const AX: Double; out AY: Double);
    procedure Clear;
    procedure CreateMeasurementSeries;
    function DownloadFile(const Url: string; AStream: TStream): Boolean;
    function GetCellText(ACol, ARow: Integer): String;
    function GetDataString(ANode: TTreeNode; ACaseType: TCaseType; var AHeader, ACounts: String): Boolean;
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState: String);
    function GetSeries(ANode: TTreeNode; ACaseType: TCaseType; ADataType: TDataType): TChartSeries;
    procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
    procedure InitShortCuts;
    procedure LayoutBars;
    procedure LoadLocations;
    procedure Logarithmic(AEnable: Boolean);
    procedure UpdateAffectedSeries;
    procedure UpdateActionStates;
    procedure UpdateGrid;
    procedure UpdateStatusBar(ASeparator: String = ' ');

    procedure LoadIni;
    procedure SaveIni;

  public
    procedure BeforeRun;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math, IniFiles,
  OpenSSL,
  {$IF FPC_FullVersion >= 30200}        // needed for SSL to work in general
  OpenSSLSockets, FpHttpClient,
  {$ELSE}
  opkman_httpclient,
  {$ENDIF}
  // TAChart units
  TATypes, TAMath, TACustomSource, TAFitLib,
  // project-specific units
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

  // Chart series colors
  COLORS: array[0..9] of TColor = (
    clRed, clBlue, clFuchsia, clLime, clSkyBlue,
    clTeal, clPurple, clBlack, clMedGray, clMoneyGreen);

  BARWIDTH_PERCENT = 80;

var
  // The BaseDate must be subtracted from the date values for fitting on the
  // log scale to prevent a floating point overflow.
  BaseDate: TDate;

  // DataDir is the directory in which the downloaded csv files are found.
  DataDir: String;

function BeginsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[1] = '"');
end;

function EndsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[Length(s)] = '"');
end;


{ TMainForm }

procedure TMainForm.acAboutExecute(Sender: TObject);
begin
  with TAboutForm.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.acChartLogarithmicExecute(Sender: TObject);
begin
  Logarithmic(acChartLogarithmic.Checked);
end;

procedure TMainForm.acConfigAutoExecute(Sender: TObject);
begin
  // Checked state is evaluated when writing ini.
end;

procedure TMainForm.acConfigHintExecute(Sender: TObject);
begin
  ShowHint := acConfigHint.Checked;
end;

procedure TMainForm.acDataClearExecute(Sender: TObject);
begin
  Clear;
end;

procedure TMainForm.acDataUpdateExecute(Sender: TObject);
const
  DOWNLOAD_ERR = 'Download error.';
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    FStatusText1 := 'Download from:';
    FStatusText2 := BASE_URL + FILENAME_CONFIRMED + '...';
    UpdateStatusbar;
    if not DownloadFile(BASE_URL + FILENAME_CONFIRMED, stream) then
    begin
      FStatusText1 := DOWNLOAD_ERR;
      UpdateStatusBar;
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOK], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(DataDir + FILENAME_CONFIRMED);

    stream.Position := 0;
    FStatusText2 := BASE_URL + FILENAME_DEATHS + '...';
    UpdateStatusbar;
    if not DownloadFile(BASE_URL + FILENAME_DEATHS, stream) then
    begin
      FStatusText1 := DOWNLOAD_ERR;
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOk], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(DataDir + FILENAME_DEATHS);

    stream.Position := 0;
    FStatusText2 := BASE_URL + FILENAME_RECOVERED + '...';
    UpdateStatusbar;
    if not DownloadFile(BASE_URL + FILENAME_RECOVERED, stream) then
    begin
      FStatusText1 := DOWNLOAD_ERR;
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOk], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(DataDir + FILENAME_RECOVERED);

    LoadLocations;

    FStatusText1 := 'Locations loaded.';
  finally
    FStatusText2 := '';
    UpdateStatusbar;
    stream.Free;
  end;
end;

procedure TMainForm.acTableSaveExecute(Sender: TObject);
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

procedure TMainForm.BeforeRun;
begin
  LeftPanel.Constraints.MinWidth := LeftPanel.Width;
  LeftPanel.AutoSize := false;

  LoadIni;
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

procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
var
  ct: TCaseType;
begin
  if ASeries is TFuncSeries then begin
    ASkip := true;
    exit;
  end;

  if (not ASeries.Active) and (TChartSeries(ASeries).Count = 0) then
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
    FStatusText1 := ''
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
    FStatusText1 := Format('%s: %s, %s',  [ser.Title, DatetoStr(x), sy]);;
    ASender.Handled;
  end;
  UpdateStatusBar;
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
  UpdateActionStates;
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
  FMeasurementSeries.Pen.Color := clGreen;
  Chart.AddSeries(FMeasurementSeries);
end;

function TMainForm.DownloadFile(const Url: string; AStream: TStream): Boolean;
var
  http: TFpHttpClient;
begin
  Result := true;
  InitSSLInterface;
  http := TFpHttpClient.Create(nil);
  try
    http.OnGetSocketHandler := @GetSocketHandler;
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    SaveIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DataDir := Application.Location + DATA_DIR;  // DATA_DIR ends with a path delimiter

  //LeftPanel.Constraints.MinWidth := LeftPanel.Width;
  //LeftPanel.AutoSize := false;

  WheelZoomTool.ZoomFactor := 1.05;
  WheelZoomTool.ZoomRatio := 1.0 / WheelZoomTool.ZoomFactor;

  {$IF LCL_FullVersion >= 2010000}
  ZoomDragTool.LimitToExtent := [zdDown, zdLeft, zdRight, zdUp];
  PanDragTool.LimitToExtent := [pdDown, pdLeft, pdRight, pdUp];
  {$ENDIF}

  cgCases.Checked[0] := true;
  Grid.RowHeights[0] := 2 * Grid.DefaultRowHeight;

  CreateMeasurementSeries;
  InitShortCuts;

//  LoadIni;

  LoadLocations;
end;

function TMainForm.GetCellText(ACol, ARow: Integer): String;
var
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
      if sa[0] <> '' then sa[0] := AnsiDequotedStr(sa[0], '"');
      if sa[1] <> '' then sa[1] := AnsiDequotedStr(sa[1], '"');
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

procedure TMainForm.GetSocketHandler(Sender: TObject; const UseSSL: Boolean; out
  AHandler: TSocketHandler);
begin
  AHandler := TSSLSocketHandler.Create;
  TSSLSocketHandler(AHandler).SSLType := stTLSv1_2;
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

procedure TMainForm.InitShortCuts;
begin
 {$IFDEF LINUX}
  acFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
 {$ENDIF}
 {$IFDEF WINDOWS}
  acFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
 {$ENDIF}
  { TODO : Implement the equivalent for MacOS }
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
        if sa[0] <> '' then sa[0] := AnsiDequotedStr(sa[0], '"');
        if sa[1] <> '' then sa[1] := AnsiDequotedStr(sa[1], '"');
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
    UpdateActionStates;
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
  FStatusText2 := '';
  UpdateStatusbar;
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
  FStatusText2 := s;
  UpdateStatusbar('. ');
end;

procedure TMainForm.MeasurementToolMeasure(ASender: TDataPointDistanceTool);
begin
  FMeasurementSeries.Active := false;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  // FIXME: Chart menu is always hidden once Table menu was shown.
  {
  mnuChart.Visible := PageControl.ActivePageIndex = pgChart;
  mnuTable.Visible := PageControl.ActivePage = pgTable;
  }
end;

procedure TMainForm.rgDataTypeClick(Sender: TObject);
begin
  Clear;
  acChartLogarithmic.Enabled := TDataType(rgDataType.ItemIndex) = dtCumulative;
  case TDataType(rgDataType.ItemIndex) of
    dtCumulative:
      begin
        Chart.LeftAxis.Title.Caption := 'Cases';
        lblTableHdr.Caption := 'Cumulative Cases';
        acChartLogarithmic.Enabled := true;
        Logarithmic(acChartLogarithmic.Checked);
        MeasurementTool.Enabled := true;
      end;
    dtNewCases:
      begin
        Chart.LeftAxis.Title.Caption := 'New Cases';
        lblTableHdr.Caption := 'New Cases';
        acChartLogarithmic.Enabled := false;
        Logarithmic(false);
        MeasurementTool.Enabled := false;
      end;
    dtNewCaseRatio:
      begin
        Chart.LeftAxis.Title.Caption := 'Day-to-day ratio of new cases';
        lblTableHdr.Caption := 'Ratio of new cases';
        acChartLogarithmic.Enabled := false;
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
  FStatustext1 := GetLocation(TreeView.Selected) + ' loaded.';
  UpdateStatusBar;
  UpdateActionStates;
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

procedure TMainForm.UpdateActionStates;
begin
  acTableSave.Enabled := Chart.SeriesCount > 1;  // 1 series reserved for measurement
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
  finally
    Grid.EndUpdate;
  end;
end;

procedure TMainForm.UpdateStatusbar(ASeparator: String = ' ');
begin
  if (FStatusText1 <> '') and (FStatusText2 <> '') then
    Statusbar.SimpleText := FStatusText1 + ASeparator + FStatusText2
  else if (FStatusText1 <> '') then
    Statusbar.SimpleText := FStatusText1
  else if (FStatusText2 <> '') then
    Statusbar.SimpleText := FStatusText2
  else
    Statusbar.SimpleText := '';
  Statusbar.Refresh;
end;

function CreateIni: TCustomIniFile;
begin
  Result := TMemIniFile.Create(GetAppConfigFile(false));
end;

procedure TMainForm.LoadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  ws: TWindowState;
begin
  ini := CreateIni;
  try
    acConfigAuto.Checked := ini.ReadBool('MainForm', 'Automatic', acConfigAuto.Checked);
    if not acConfigAuto.Checked then
      exit;

    ws := TWindowState(ini.ReadInteger('MainForm', 'WindowState', ord(WindowState)));
    if ws = wsMaximized then
      WindowState := ws
    else
    begin
      WindowState := wsNormal;
      L := ini.ReadInteger('MainForm', 'Left', Left);
      T := ini.ReadInteger('MainForm', 'Top', Top);
      W := ini.ReadInteger('MainForm', 'Width', Width);
      H := ini.ReadInteger('MainForm', 'Height', Height);
      R := Screen.WorkAreaRect;
      if W > R.Width then W := R.Width;
      if L + W > R.Right then L := R.Right - W;
      if L < R.Left then L := R.Left;
      if T + H > R.Bottom then T := R.Bottom - H;
      if T < R.Top then T := R.Top;
      SetBounds(L, T, W, H);
    end;

    LeftPanel.Width := ini.ReadInteger('MainForm', 'LeftPanel', LeftPanel.Width);
    RightPanel.Width := ini.ReadInteger('MainForm', 'RightPanel', RightPanel.Width);
    PageControl.ActivePageIndex := ini.ReadInteger('MainForm', 'PageControl', PageControl.ActivePageIndex);
    PageControlChange(nil);

    cgCases.Checked[0] := ini.Readbool('MainForm', 'ConfirmedCases', cgCases.Checked[0]);
    cgCases.Checked[1] := ini.ReadBool('MainForm', 'DeathCases', cgCases.Checked[1]);
    cgCases.Checked[2] := ini.ReadBool('MainForm', 'RecoveredCases', cgCases.Checked[2]);
    rgDataType.ItemIndex := ini.ReadInteger('MainForm', 'DataType', rgDataType.ItemIndex);

    acConfigHint.Checked := ini.ReadBool('MainForm', 'ShowHints', acConfigHint.Checked);
    acConfigHintExecute(nil);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.SaveIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    ini.WriteBool('MainForm', 'Automatic', acConfigAuto.Checked);
    if not acConfigAuto.Checked then
      exit;

    ini.WriteInteger('MainForm', 'WindowState', ord(WindowState));
    if WindowState = wsNormal then
    begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;

    ini.WriteInteger('MainForm', 'LeftPanel', LeftPanel.Width);
    ini.WriteInteger('MainForm', 'RightPanel', RightPanel.Width);
    ini.WriteInteger('MainForm', 'PageControl', PageControl.ActivePageIndex);

    ini.WriteBool('MainForm', 'ConfirmedCases', cgCases.Checked[0]);
    ini.WriteBool('MainForm', 'DeathCases', cgCases.Checked[1]);
    ini.WriteBool('MainForm', 'RecoveredCases', cgCases.Checked[2]);
    ini.WriteInteger('MainForm', 'DataType', rgDataType.ItemIndex);

    ini.WriteBool('MainForm', 'ShowHints', acConfigHint.Checked);

  finally
    ini.Free;
  end;
end;

initialization
  BaseDate := EncodeDate(2020, 1, 1);

end.

