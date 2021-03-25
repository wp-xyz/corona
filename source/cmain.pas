unit cMain;

{$mode objfpc}{$H+}
// {$define RKI}  -- this define is in the project options
{$define USE_BARSERIES}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Grids, Types, LCLVersion, Menus, ActnList,
  StdActns, ColorBox,
  TAGraph, TAIntervalSources, TASeries, TAChartListbox, TALegend,
  TACustomSeries, TATransformations, TATools, TAFuncSeries, TADataTools,
  TAChartUtils, TADrawUtils,
  cGlobal, cDataSource, cDatamodule, cPalette, cSeries, cMapFrame, cTimeSeriesFrame;

type
  TCountArray = array of Integer;
  TDateArray = array of TDate;
  TDataPointArray = array of TDoublePoint;

  TVisibleCharts = (vcMap, vcTimeSeries, vcBoth);

  { TMainForm }

  TMainForm = class(TForm)
    acDataUpdate: TAction;
    acAbout: TAction;
    acDataClear: TAction;
    acConfigHint: TAction;
    acConfigAutoLoad: TAction;
    acDataCommonStart: TAction;
    acDataMovingAverage: TAction;
    acInfectiousPeriod: TAction;
    acSmoothingRange: TAction;
    acChartMap: TAction;
    acChartTimeSeries: TAction;
    acConfigAutoSave: TAction;
    acChartBoth: TAction;
    ActionList: TActionList;
    Chart: TChart;
    BottomAxisTransformations: TChartAxisTransformations;
    BottomAxisLogTransform: TLogarithmAxisTransform;
    DateIndicatorLine: TConstantLine;
    InfoLabel: TLabel;
    InfoPanel: TPanel;
    MenuItem1: TMenuItem;
    MenuItem15: TMenuItem;
    DisplayPanel: TPanel;
    NameLabel: TLabel;
    MapToolset: TChartToolset;
    MapDateLabel: TLabel;
    MapToolsetDataPointClickTool: TDataPointClickTool;
    MapToolsetDataPointHintTool: TDataPointHintTool;
    MapToolsetPanDragTool: TPanDragTool;
    MapToolsetZoomDragTool: TZoomDragTool;
    MenuItem12: TMenuItem;
    MenuItem9: TMenuItem;
    PaletteListbox: TColorListBox;
    MapChart: TChart;
    ChartToolset: TChartToolset;
    acFileExit: TFileExit;
    lblTableHint: TLabel;
    MainMenu: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuCommonStart: TMenuItem;
    mnuMovingAverage: TMenuItem;
    MenuItem4: TMenuItem;
    mnuDataUpdate: TMenuItem;
    mnuData: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuConfigHints: TMenuItem;
    mnuConfig: TMenuItem;
    mnuChart: TMenuItem;
    mnuFileQuit: TMenuItem;
    mnuFile: TMenuItem;
    ChartSplitter: TSplitter;
    MapChartPanel: TPanel;
    MapDateScrollbar: TScrollBar;
    MapSplitter: TSplitter;
    PaletteListboxPanel: TPanel;
    TimeSeriesChartPanel: TPanel;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton15: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tbAbout: TToolButton;
    WheelZoomTool: TZoomMouseWheelTool;
    Grid: TDrawGrid;
    lblTableHdr: TLabel;
    PageControl: TPageControl;
    PanDragTool: TPanDragTool;
    pgChart: TTabSheet;
    pgTable: TTabSheet;
    SaveDialog: TSaveDialog;
    ZoomDragTool: TZoomDragTool;
    MeasurementTool: TDataPointDistanceTool;
    UserdefinedTool: TUserDefinedTool;
    CrossHairTool: TDataPointCrosshairTool;
    LeftAxisTransformations: TChartAxisTransformations;
    LeftAxisLogTransform: TLogarithmAxisTransform;
    ChartListbox: TChartListbox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    LeftPanel: TPanel;
    TreeSplitter: TSplitter;
    RightSplitter: TSplitter;
    TreeView: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acChartMapExecute(Sender: TObject);
    procedure acConfigAutoLoadExecute(Sender: TObject);
    procedure acConfigAutoSaveExecute(Sender: TObject);
    procedure acConfigHintExecute(Sender: TObject);
    procedure acDataClearExecute(Sender: TObject);
    procedure acDataCommonStartExecute(Sender: TObject);
    procedure acDataMovingAverageExecute(Sender: TObject);
    procedure acDataUpdateExecute(Sender: TObject);
    procedure acInfectiousPeriodExecute(Sender: TObject);
    procedure acSmoothingRangeExecute(Sender: TObject);

    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure cmbDataTypeDropDown(Sender: TObject);
    procedure CrossHairToolDraw(ASender: TDataPointDrawTool);

    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure MeasurementToolGetDistanceText(ASender: TDataPointDistanceTool;
      var AText: String);
    procedure MeasurementToolMeasure(ASender: TDataPointDistanceTool);

    procedure PaletteListboxGetColors(Sender: TCustomColorListBox;
      Items: TStrings);

    procedure ToolBarResize(Sender: TObject);

//    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewSelectionChanged(Sender: TObject);

  private
    FDisplayMode: TDisplayMode;
    FMapFrame: TMapFrame;
    FTimeSeriesFrame: TTimeSeriesFrame;
    FDisplaySplitter: TSplitter;    // hor splitter between map and time-series.
    FDisplaySplitterPos: Integer;

    FPalette: TPalette;
    FMeasurementSeries: TFuncSeries;
    FFitCoeffs: array[0..1] of Double;
    FStatusText1, FStatusText2: String;
    FLastDate: TDate;
    FMapLock: Integer;
    FOldMapResource: String;

    function CalcFit(ASeries: TBasicChartSeries; xmin, xmax: Double): Boolean;
    procedure CalcFitCurveHandler(const AX: Double; out AY: Double);
    procedure Clear(UnselectTree: Boolean = true);
    procedure ClearAllSeries;
    procedure CreateMeasurementSeries;
    procedure DownloadMsgHandler(Sender: TObject; const AMsg1, AMsg2: String; APercentage: Integer);
    procedure EnableMovingAverage(ASeries: TChartSeries; AEnabled, AStrict: Boolean);
    function GetCellText(ACol, ARow: Integer): String;
    function GetDataItem(ANode: TTreeNode): TcDataItem;
    function GetDataType: TDataType;
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState, ACity: String; out APopulation: Int64);
    function GetMapDataType: TMapDataType;
    procedure GetMapResourceParams(var ANode: TTreeNode; out AResName: String;
      out APlotChildNodes: Boolean);
    function GetSeries(ANode: TTreeNode; ACaseType: TCaseType;
      ADataType: TDataType): TBasicPointSeries;
    procedure InitShortCuts;
    function IsTimeSeries: Boolean;
    procedure LoadLocations;
    procedure PopulatePaletteListbox(AMapDataType: TMapDataType);
    procedure ReadCommandlineParams;
    procedure SelectNode(ANode: TTreeNode);
    procedure ShowVersionInfo;
    procedure UpdateActionStates;
    procedure UpdateData;
    procedure UpdateDateIndicatorLine(ADate: TDate);
    procedure UpdateInfectiousPeriod;
    procedure UpdateStatusBar(ASeparator: String = ' ');
    procedure UpdateTimeSeriesActions(Sender: TObject);

    procedure SetDisplayMode(AMode: TDisplayMode);
    procedure ShowInfoHandler(Sender: TObject; const ATitle, AInfos: String);
    procedure TimeSeriesFrameResize(Sender: TObject);

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
  LCLIntf, Math, IniFiles, DateUtils, LCLPlatformDef,
  // TAChart units
  TATypes, TAMath, TACustomSource, TAFitLib,
  // project-specific units
  {$IF LCL_FullVersion < 3020000}
  cFixes,
  {$IFEND}
  cJohnsHopkinsUniversity, {$IFDEF RKI}cRobertKochInstitut,{$ENDIF}
  cPolygonSeries, cGeoReaderKML, cUtils, cAbout;

const
  // Chart series colors
  COLORS: array[0..9] of TColor = (
    clRed, clBlue, clFuchsia, clLime, clSkyBlue,
    clTeal, clPurple, clBlack, clMedGray, clMoneyGreen);

  START_COUNT = 100;

var
  // The BaseDate must be subtracted from the date values for fitting on the
  // log scale to prevent a floating point overflow.
  // This is not used, however, when dates have a common start.
  // DateOffset is the correct offset to be used in any case
  BaseDate: TDate;
  DateOffset: TDate;


function GetDataDir: String;
begin
  if PortableInstallation then
   {$IFDEF DARWIN}
    Result := Application.Location + '../../../data/'
   {$ELSE}
    Result := Application.Location + 'data/'
   {$ENDIF}
  else
    {$IFDEF Windows}
    Result := GetUserDir + 'CoronaData\';
    {$ELSE}
    Result := GetUserDir + '.coronadata/';
    {$ENDIF}
end;

function CreateIni: TCustomIniFile;
var
  fn: String;
begin
  if PortableInstallation then
    fn := ChangeFileExt(Application.ExeName, '.cfg')
  else
    fn := GetAppConfigFile(false);
  Result := TMemIniFile.Create(fn);
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

procedure TMainForm.acChartMapExecute(Sender: TObject);
var
  dm: TDisplayMode;
begin
  if acChartMap.Checked then
    SetDisplayMode(dmMap)
  else if acChartTimeSeries.Checked then
    SetDisplayMode(dmTimeSeries)
  else if acChartBoth.Checked then
    SetDisplayMode(dmBoth)
  else
    raise Exception.Create('Unknown display mode');
end;

procedure TMainForm.acConfigAutoLoadExecute(Sender: TObject);
begin
  // Checked state is evaluated when reading ini.
end;

procedure TMainForm.acConfigAutoSaveExecute(Sender: TObject);
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
  if Assigned(FTimeSeriesFrame) then
    FTimeSeriesFrame.Clear;
end;

procedure TMainForm.acDataCommonStartExecute(Sender: TObject);
var
  logX, logY: Boolean;
  i: Integer;
begin
  TimeSeriesSettings.CommonStart := acDataCommonStart.Checked;
  if Assigned(FTimeSeriesFrame) then
    FTimeSeriesFrame.SetCommonStart(TimeSeriesSettings.CommonStart);
                {
  if acDataCommonStart.Checked then
  begin
    DateOffset := 0;
  end else
  begin
    DateOffset := BaseDate;
  end;

  logX := (GetDataType = dtCumVsNewCases) and acChartLogarithmic.Checked;
  logY := acChartLogarithmic.Checked;
  UpdateAxes(logX, logY);

  for i := 0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TcLineSeries then
      ShowTimeSeries(TcLineSeries(Chart.Series[i]).Node)
    else if Chart.Series[i] is TcBarSeries then
      ShowTimeSeries(TcBarSeries(Chart.Series[i]).Node);
      }
end;

procedure TMainForm.acDataMovingAverageExecute(Sender: TObject);
var
  i: Integer;
  isMovingAverage: Boolean;
begin
  if Assigned(FTimeSeriesFrame) then
    FTimeSeriesFrame.EnableMovingAverage(acDataMovingAverage.Checked, false);

  // to be removed
  isMovingAverage := acDataMovingAverage.Checked and
    ( GetDataType in [dtCumulative, dtNormalizedCumulative, dtNormalizedNewCases, dtNewCases] );

  for i:=0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TChartSeries then
      EnableMovingAverage(TChartSeries(Chart.Series[i]), isMovingAverage, false);

  {
  MovingAverageInfo.Caption := Format('(%d days)', [SmoothingRange]);
  MovingAverageInfo.Enabled := acDataMovingAverage.Enabled;
  UpdateGrid;
  }
end;

procedure TMainForm.acDataUpdateExecute(Sender: TObject);
begin
  UpdateData;
end;

procedure TMainForm.acInfectiousPeriodExecute(Sender: TObject);
var
  n: Integer;
  s: String;
begin
  s := IntToStr(InfectiousPeriod);
  if InputQuery('Infectious period', 'Days:', s) then
  begin
    if TryStrToInt(s, n) and (n > 0) then
    begin
      InfectiousPeriod := n;
      UpdateInfectiousPeriod;
    end else
      MessageDlg('No valid number.', mtError, [mbOk], 0);
  end;
end;

procedure TMainForm.acSmoothingRangeExecute(Sender: TObject);
var
  n: Integer;
  s: String;
  L: TFPList;
begin
  s := IntToStr(SmoothingRange);
  if InputQuery('Smoothing range', 'Total days (including center day)', s) then
  begin
    if TryStrToInt(s, n) and (n > 0) then
    begin
      if odd(n) then
      begin
        SmoothingRange := n;
        RRange := (n - 1) div 2;

        //MovingAverageInfo.Caption := Format('(%d days)', [SmoothingRange]);

        FTimeSeriesFrame.UpdateData
        {
        // Recalculate the currently loaded data
        L := StoreSeriesNodesInList();
        try
          RestoreSeriesNodesFromList(L);
        finally
          L.Free;
        end;
         }

      end else
        MessageDlg('Only odd number of days allowed (i.e. center day is included).', mtError, [mbOk], 0);
    end else
      MessageDlg('No valid number.', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.BeforeRun;
begin
//  LeftPanel.Constraints.MinWidth := LeftPanel.Width;
//  LeftPanel.AutoSize := false;

  ReadCommandlineParams;
  LoadLocations;
  LoadIni;
  ShowVersionInfo;
end;

// It is assumed that xmin < xmax.
function TMainForm.CalcFit(ASeries: TBasicChartSeries;
  xmin, xmax: Double): Boolean;
const
  EPS = 1E-9;
var
  x: TArbFloatArray = nil;
  y: TArbFloatArray = nil;
  n, i: Integer;
  xval, yval: Double;
  fitParams: TFitParamArray = nil;
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
    x[n] := xval - DateOffset;
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

procedure TMainForm.CalcFitCurveHandler(const AX: Double; out AY: Double);
begin
  if not (IsNaN(FFitCoeffs[0]) or IsNaN(FFitCoeffs[1])) then
    AY := FFitCoeffs[0] * exp(FFitCoeffs[1] * (AX - DateOffset))
  else
    AY := NaN;
end;

procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
var
  ct: TCaseType;
begin
  if ((ASeries = DateIndicatorLine) and not acChartMap.Checked) or
     (ASeries is TFuncSeries)
  then
  begin
    ASkip := true;
    exit;
  end;
{
  if ((ASeries = DateIndicatorLine) and not (acChartMap.Checked and acChartTimeSeries.Checked)) or
     (ASeries is TFuncSeries) then
  begin
    ASkip := true;
    exit;
  end;
}
  if (not ASeries.Active) and (ASeries is TChartSeries) and (TChartSeries(ASeries).Count = 0) then
    for ct in TCaseType do
      if (pos(CASETYPE_NAMES[ct], ASeries.Title) > 0) or
         (pos(R_NUMBER_STR, ASeries.Title) > 0) then
      begin
        ASkip := true;
        exit;
      end;
end;

procedure TMainForm.CrossHairToolDraw(
  ASender: TDataPointDrawTool);
const
  DECS: array[boolean] of Integer = (0, 1);
var
  ser: TChartSeries;
  x, y: Double;
  sx, sy: String;
  dt: TDataType;
  d: TDate;
  idx: Integer;
begin
  if ASender = nil then
    exit;

  FStatusText1 := '';
  if ASender.Series is TChartSeries then
  begin
    dt := GetDataType();
    ser := TChartSeries(ASender.Series);
    idx := ASender.PointIndex;
    if idx > -1 then
    begin
      x := ser.GetXValue(idx);
      y := ser.GetYValue(idx);
      case dt of
        dtCumulative, dtNormalizedCumulative:
          begin
            sx := FormatDateTime('dddddd', x);
            if y < 0.5 then sy := '0 cases'
              else if y < 1.5 then sy := '1 case'
              else sy := Format('%.*n cases', [DECS[(y < 100) and (dt = dtNormalizedCumulative)], y]);
            if dt = dtNormalizedCumulative then
              sy := sy + ' per 100 000 inhabitants and per week';
            FStatusText1 := Format('%s, %s', [sx, sy]);
          end;
        dtNewCases, dtNormalizedNewCases:
          begin
            sx := FormatDateTime('dddddd', x);
            if y = 1.0 then
              sy := '1 new case'
            else
              sy := Format('%.*n new cases', [DECS[(y < 100) and (dt = dtNormalizedNewCases)], y]);
            if dt = dtNormalizedNewCases then
              sy := sy + ' per 100 000 inhabitants and per week';
            FStatusText1 := Format('%s, %s', [sx, sy]);
          end;
        dtCumulativeCasesDoublingTime, dtNewCasesDoublingTime:
          begin
            sx := FormatDateTime('dddddd', x);
            sy := Format('Doubling time %.1f', [y]);
            FStatusText1 := Format('%s, %s', [sx, sy]);
          end;
        dtCumVsNewCases:
          begin
            sx := ser.Source.Item[ASender.PointIndex]^.Text;
            d := ScanDateTime('mm"/"dd"/"yy', ser.Source.Item[ASender.PointIndex]^.Text, cFormatSettings);
            if x = 1 then sx := '1 case' else sx := Format('%.0n cases', [x]);
            if y = 1 then sy := '1 new case' else sy := Format('%.0n new cases', [y]);
            FStatusText1 := Format('%s - %s, %s', [DateToStr(d), sx, sy]);
          end;
        dtRValue:
          begin
            sx := FormatDateTime('dddddd', x);
            sy := Format('Reproduction number: %.1f', [y]);
            FStatusText1 := Format('%s, %s', [sx, sy]);
          end;
      end;
      FStatusText1 := ser.Title + ': ' + FStatusText1;
    end;
    ASender.Handled;
  end;
  UpdateStatusBar;
end;

procedure TMainForm.Clear(UnselectTree: Boolean = true);
begin
  if UnselectTree then
    TreeView.Selected := nil;

  ClearAllSeries;
  CreateMeasurementSeries;

//  UpdateGrid;
  UpdateActionStates;
end;

procedure TMainForm.ClearAllSeries;
begin
  // Be careful here: DateIndicatorLine is destroyed in Chart.ClearSeries but it
  // is still accessed by the ChartListBox. Better to prevent TChartListbox from
  // updating itself during the clearing steps.
  ChartListbox.Chart := nil;
  try
    // Another safety measure: make sure that DateIndicatorLine is nil while
    // Chart.ClearSeries is executing
    FreeAndNil(DateIndicatorLine);
    Chart.ClearSeries;
  finally
    ChartListBox.Chart := Chart;
  end;
end;
                       (*
procedure TMainForm.cmbDataTypeChange(Sender: TObject);
var
  L: TFPList;
  dt: TDataType;
begin
  // Store currently available series in a list
  L := StoreSeriesNodesInList();
  try
    Clear;
    dt := GetDataType();
    case dt of
      dtCumulative, dtNormalizedCumulative,
      dtNewCases, dtNormalizedNewCases:
        begin
          case dt of
            dtCumulative:
              lblTableHdr.Caption := 'Cumulative cases';
            dtNormalizedCumulative:
              lblTableHdr.Caption := Format('Cumulative cases per population %.0n', [1.0 * PopulationRef]);
            dtNewCases:
              lblTableHdr.Caption := 'New cases per day';
            dtNormalizedNewCases:
              lblTableHdr.Caption := Format('New cases per week and population %.0n', [1.0 * PopulationRef])
            else
              raise Exception.Create('Data type not handled by cmbDataTypeChange');
          end;
          lblTableHint.Caption := '';
          Chart.LeftAxis.Title.Caption := lblTableHdr.Caption;
          Chart.BottomAxis.Title.Caption := 'Date';
          UpdateAxes(false, acChartLogarithmic.Checked);
          MeasurementTool.Enabled := true;
          acDataMovingAverage.Enabled := true;
          //clbCases.Enabled := true;
        end;
      dtCumulativeCasesDoublingTime,
      dtNewCasesDoublingTime:
        begin
          Chart.LeftAxis.Title.Caption := 'Doubling time (days)';
          Chart.BottomAxis.Title.Caption := 'Date';
          lblTableHdr.Caption := 'Doubling time (days, calculated by lookup from previous days)';
          lblTableHint.Caption := ''; //Note: Strictly speaking, this value requires exponential growth which is valid only during the initial phase of the outbreak.';
          UpdateAxes(false, false);
          MeasurementTool.Enabled := false;
          acDataMovingAverage.Enabled := false;
          //clbCases.Enabled := true;
        end;
      dtCumVsNewCases:
        begin
          lblTableHdr.Caption := 'New cases vs. cumulative cases';
          lblTableHint.Caption := '';
          Chart.LeftAxis.Title.Caption := 'New cases';
          Chart.BottomAxis.title.Caption := 'Cumulative cases';
          acChartLogarithmic.Checked := true;
          UpdateAxes(true, true);
          MeasurementTool.Enabled := false;
          acDataMovingAverage.Enabled := false;
          //clbCases.Enabled := true;
        end;
      dtRValue:
        begin
          Chart.LeftAxis.Title.Caption := 'Reproduction number';
          Chart.BottomAxis.Title.Caption := 'Date';
          lblTableHdr.Caption := 'Reproduction number';
          lblTableHint.Caption := 'Calculated as ratio of new case count at a date and ' + IntToStr(InfectiousPeriod) + ' days earlier.';
          UpdateAxes(false, false);
          MeasurementTool.Enabled := false;
          acDataMovingAverage.Enabled := false;
          //clbCases.Enabled := false;
        end;
      else
        raise Exception.Create('Data type unsupported.');
    end;

    acDataMovingAverageExecute(nil);
    RestoreSeriesNodesFromlist(L);
    UpdateActionStates;
    WordwrapChart(Chart);

  finally
    L.Free;
  end;
end;
           *)
procedure TMainForm.cmbDataTypeDropDown(Sender: TObject);
var
  i, w: Integer;
begin
  {
  w := 0;
  for i := 0 to cmbDataType.Items.Count-1 do
    w := Max(w, cmbDataType.Canvas.TextWidth(cmbDataType.Items[i]));
  cmbDataType.ItemWidth := w + Scale96ToForm(10);
  }
end;


procedure TMainForm.CreateMeasurementSeries;
begin
  FMeasurementSeries := TFuncSeries.Create(Chart);
  FMeasurementSeries.Active := false;
  FMeasurementSeries.OnCalculate := @CalcFitCurveHandler;
  FMeasurementSeries.Legend.Visible := false;
  FMeasurementSeries.AxisIndexX := 1;
  FMeasurementseries.AxisIndexY := 0;
  FMeasurementSeries.Pen.Width := 3;
  FMeasurementSeries.Pen.Color := clGreen;
  FMeasurementSeries.ZPosition := 99;
  Chart.AddSeries(FMeasurementSeries);

  DateIndicatorLine := TConstantLine.Create(Chart);
  DateIndicatorLine.Active := false;
  DateIndicatorLine.LineStyle := lsVertical;
  DateIndicatorLine.Pen.Width := 2;
  DateIndicatorLine.Pen.Color := clFuchsia;
  DateIndicatorLine.Title := 'Date indicator';
  Chart.AddSeries(DateIndicatorLine);
end;

procedure TMainForm.DownloadMsgHandler(Sender: TObject; const AMsg1, AMsg2: String;
  APercentage: Integer);
begin
  {
  Progressbar.Position := APercentage;
  Progressbar.Update;

  FStatusText1 := AMsg1;
  FStatusText2 := AMsg2;
  UpdateStatusbar;

  // Make sure that status msg is painted in Linux
  Application.ProcessMessages;
  }
end;

procedure TMainForm.EnableMovingAverage(ASeries: TChartSeries;
  AEnabled, AStrict: Boolean);
begin
  acDataMovingAverage.Checked := AEnabled;
  if ASeries is TcLineSeries then
    TcLineSeries(ASeries).MovingAverage := AEnabled
  else
  if ASeries is TcBarSeries then
    TcBarSeries(ASeries).MovingAverage := AEnabled
  else
  if AStrict then
    raise Exception.Create('Only modified series types allowed.');
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  p4, p6: Integer;
  clr: TColor;
  i, w, wCheckbox: Integer;
begin
  {
  p4 := Scale96ToForm(4);
  p6 := Scale96ToForm(6);
  wCheckbox := GetSystemMetrics(SM_CXMENUCHECK);
  w := 0;
  for i := 0 to clbCases.Items.Count-1 do
    w := max(w, clbCases.Canvas.TextWidth(clbCases.Items[i]));
  CasesPanel.Constraints.MinWidth := w + wCheckbox + 2*p6;
  CasesPanel.Constraints.MinHeight := clbCases.Top + clbCases.Items.Count * clbCases.ItemHeight + 2*p4;
  TimeSeriesGroup.Constraints.MinHeight := TimeseriesGroup.Height - TimeSeriesGroup.ClientHeight +
    CasesPanel.Constraints.MinHeight + CasesPanel.BorderSpacing.Bottom + p6;
  }
  clr := ColorToRGB(Chart.BackColor);
  if Red(clr) + Green(clr) + Blue(clr) > 3*128 then
    Chart.LeftAxis.Grid.Color := BrighterColor(clr, -0.15)
  else
    Chart.LeftAxis.Grid.Color := BrighterColor(clr, +0.15);
  Chart.BottomAxis.Grid.Color := Chart.LeftAxis.Grid.Color;

//  WordwrapChart(Chart);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    SaveIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMapFrame := TMapFrame.Create(self);
  FMapFrame.DataTree := TreeView;
  FMapFrame.Align := alClient;
  FMapFrame.OnShowInfo := @ShowInfoHandler;
  FMapFrame.Hide;
  FMapFrame.Parent := DisplayPanel;

  FTimeSeriesFrame := TTimeSeriesFrame.Create(self);
  FTimeSeriesFrame.Height := DisplayPanel.Height div 2;
  FTimeSeriesFrame.Align := alBottom;
  FTimeSeriesFrame.DataTree := TreeView;
  FTimeSeriesFrame.OnResize := @TimeSeriesFrameResize;
  FTimeSeriesFrame.OnShowInfo := @ShowInfoHandler;
  FTimeSeriesFrame.OnUpdateActions := @UpdateTimeSeriesActions;
  FTimeSeriesFrame.Hide;
  FTimeSeriesFrame.Parent := DisplayPanel;

  FDisplaySplitter := TSplitter.Create(self);
  FDisplaySplitter.Align := alBottom;
  FDisplaySplitter.Parent := DisplayPanel;
  FDisplaySplitter.ResizeStyle := rsPattern;
  FDisplaySplitterPos := FTimeSeriesFrame.Height;

  SetDisplayMode(dmBoth);

  // Narrower input box
  cInputQueryEditSizePercents := 0;

  DataDir := GetDataDir; // ends with a path delimiter
  DateOffset := BaseDate;

  FDisplaySplitterPos := 200;
  WheelZoomTool.ZoomFactor := 1.05;
  WheelZoomTool.ZoomRatio := 1.0 / WheelZoomTool.ZoomFactor;

  PageControl.ActivePageIndex := 0;
  CreateMeasurementSeries;
  InitShortCuts;

  FreeAndNil(DateIndicatorLine);  // needed at designtime, will be recreated

  PopulatePaletteListbox(GetMapDataType);
  ShowInfoHandler(nil, '', '');
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  s: String;
  w: Integer;
  p3: Integer;
  p24: Integer;
begin
  p3 := Scale96ToFont(3);
  p24 := Scale96ToFont(24);

  tbAbout.Align := alRight;
  tbAbout.Left := Toolbar.Width - tbAbout.Width;

  {
  w := 0;
  for s in clbCases.Items do
    w := Max(w, clbCases.Canvas.TextWidth(s));
  clbCases.Width := w + GetSystemMetrics(SM_CXVSCROLL) + p24;
  if clbCases.ItemHeight > 0 then
    clbCases.Height := clbCases.Items.Count * clbCases.ItemHeight + 2*p3;

  LeftPanel.Constraints.MinWidth := cbMovingAverage.Width + clbCases.Width + p24;
  }
end;

function TMainForm.GetCellText(ACol, ARow: Integer): String;
var
  col: TGridColumn;
  ser: TChartSeries;
  n: Integer;
  r: Integer;
  dt: TDate;
  lastDateOfSeries: TDate;
begin
  if (ACol = 0) and (ARow = 0) then
    Result := 'Date'
  else
  begin
    Result := '';
    if Grid.Columns.Count = 0 then
      exit;
    dt := FLastDate - (ARow - Grid.FixedRows);
    if (ACol = 0) then
    begin
      Result := DateToStr(dt);
      {
      col := Grid.Columns[0];
      ser := TChartSeries(col.Tag);
      n := ser.Count;
      r := n - ARow;
      if (r > -1) and (r < n) then
      begin
        if IsTimeSeries() then
          dt := ser.XValue[r]
        else
          dt := ScanDateTime('mm"/"dd"/"yyyy', ser.Source.Item[r]^.Text);
        Result := DateToStr(dt);
      end;
      }
    end else
    if ARow > 0 then
    begin
      col := Grid.Columns[ACol - 1];
      ser := TChartSeries(col.Tag);
      lastDateOfSeries := ser.XValue[ser.LastValueIndex];
      r := ARow - Grid.FixedRows - round(FLastDate - lastDateOfSeries);
      n := ser.Count;
//      r := n - ARow;
      if (r > -1) and (r < n) then
        if not IsNan(ser.YValue[r]) then
          case GetDataType() of
            dtCumulativeCasesDoublingTime,
            dtNewCasesDoublingTime:
              Result := Format('%.1f', [ser.YValue[r]]);
            dtCumVsNewCases:
              if odd(ACol) then
                Result := Format('%.0n', [ser.YValue[r]])
              else
                Result := Format('%.0n', [ser.XValue[r]]);
            dtRValue:
              Result := Format('%.2f', [ser.YValue[r]]);
            else
              Result := Format('%.0n', [ser.YValue[r]]);
          end;
    end;
  end;
end;

function TMainForm.GetDataItem(ANode: TTreeNode): TcDataItem;
begin
  Result := TcDataItem(ANode.Data);
end;

function TMainForm.GetDataType: TDataType;
begin
  Result := TimeSeriesSettings.DataType;
//  Result := TDataType(cmbDataType.ItemIndex);
end;


procedure TMainForm.GetLocation(ANode: TTreeNode;
  out ACountry, AState, ACity: String; out APopulation: Int64);
var
  item: TcDataItem;
begin
  ACountry := '';
  AState := '';
  ACity := '';

  case ANode.Level of
    0,
    1,
    2: ACountry := ANode.Text;
    3: begin
         AState := ANode.Text;
         ACountry := ANode.Parent.Text;
       end;
    4: begin
         ACity := ANode.Text;
         AState := ANode.Parent.Text;
         ACountry := ANode.Parent.Parent.Text;
       end;
  end;

  if TObject(ANode.Data) is TcDataItem then
  begin
    item := TcDataItem(ANode.Data);
    if item <> nil then
      APopulation := item.Population;
  end;
end;

function TMainForm.GetLocation(ANode: TTreeNode): String;
var
  country, state, city: String;
  population: Int64;
begin
  GetLocation(ANode, country, state, city, population);
  Result := country;
  if state <> '' then begin
    Result := Result + '/' + state;
    if city <> '' then
      Result := Result + ' / ' + city;
  end;
end;

function TMainForm.GetMapDataType: TMapDataType;
begin
  Result := MapSettings.DataType;
//  Result := TMapDataType(cmbMapDataType.ItemIndex);
end;

function TMainForm.GetSeries(ANode: TTreeNode; ACaseType: TCaseType;
  ADataType: TDataType): TBasicPointSeries;
var
  i: Integer;
  serTitle: String;
  ser: TBasicPointSeries;
  ct: TCaseType;
  clr: TColor;
begin
  serTitle := Format('%s (%s)', [GetLocation(ANode), CASETYPE_NAMES[ACaseType]]);

  for i:=0 to Chart.SeriesCount-1 do
    if (Chart.Series[i] is TBasicPointSeries) then
    begin
      Result := TBasicPointSeries(Chart.Series[i]);
      if (Result <> nil) and (pos(serTitle, Result.Title) = 1) then
      begin
        Result.Active := true;
        exit;
      end;
    end;

  clr := COLORS[((Chart.SeriesCount - 1) div (ord(High(TCaseType))+1)) mod Length(COLORS)];

  // The node does not yet have associated series. Create 4 series for
  // the cases "confirmed", "deaths", "recovered" and "sick".
  // Return the one for ADatatype and hide the others.
  serTitle := GetLocation(ANode);
  for ct in TCaseType do begin
    {$IFNDEF USE_BARSERIES}
    case ADataType of
      dtCumulative,
      dtNormalizedCumulative,
      dtDoublingTime,
      dtCumVsNewCases,
      dtRValue:
        begin
    {$ENDIF}
          ser := TcLineSeries.Create(Chart);
          with TcLineSeries(ser) do
          begin
            ShowPoints := true;
            LinePen.Color := clr;
            Pointer.Pen.Color := clr;
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
              ctSick:
                begin
                  Pointer.Style := psTriangle;
                  Pointer.Brush.Color := LinePen.Color;
                end;
            end;
            AccumulationRange := SmoothingRange;
            MovingAverage := acDataMovingAverage.Checked;
            Node := ANode;
          end;
    {$IFNDEF USE_BARSERIES}
        end;
      dtNewCases,
      dtNormalizedNewCases:
        begin
          ser := TcBarSeries.Create(Chart);
          with TcBarSeries(ser) do
          begin
            BarBrush.Color := clr;
            BarBrush.Style := bsSolid;
            BarPen.Color := BarBrush.Color;
            case ct of
              ctConfirmed : BarBrush.Style := bsSolid;
              ctDeaths    : BarBrush.Color := clWhite;
              ctRecovered : BarBrush.Style := bsDiagCross;
              ctSick      : BarBrush.Style := bsHorizontal;
            end;
            AccumulationRange := SmoothingRange;
            MovingAverage := acDataMovingAverage.Checked;
            Node := ANode;
          end;
        end;
    end;  // case
    {$ENDIF}

    if ADataType = dtRValue then
      ser.Title := serTitle + ' (' + R_NUMBER_STR + ')'
    else
      ser.Title := serTitle + ' (' + CASETYPE_NAMES[ct] + ')';
    ser.AxisIndexX := 1;
    ser.AxisIndexY := 0;
    if (ct = ACaseType) or ((ct = ctConfirmed) and (ADataType = dtRvalue)) then
      Result := ser
    else
      ser.Active := false;
    Chart.AddSeries(ser);
  end;  // for ct

//  UpdateAffectedSeries;
end;

{ The specified node has been clicked.
  If the node's parent data contain a specification of the map to be displayed the
  name of its resource is returned. If there is no map specification then
  the parents are checked upwards until a node with a map resource name is found.
  The clicked node's data contain also information at which tree level the
  data to be mapped are found relative to the map node. }
procedure TMainForm.GetMapResourceParams(var ANode: TTreeNode; out AResName: String;
  out APlotChildNodes: Boolean);
var
  dataItem: TcDataItem;
  ANodeDataItem: TcDataItem;
  datanode: TTreeNode;
  mapNode: TTreeNode;
  i: Integer;
begin
  AResName := '';
  APlotChildNodes := false;
  if ANode = nil then
    exit;

  ANodeDataItem := TcDataItem(ANode.Data);
  mapNode := ANode.Parent;
  if mapNode = nil then
    mapNode := ANode.GetFirstSibling;
  repeat
    dataItem := TcDataItem(mapNode.Data);
    if ANodeDataItem.UseOtherMapResource and (dataItem.OtherMapResource <> '') then
    begin
      AResName := dataItem.OtherMapResource;
      break;
    end else
    if not ANodeDataItem.UseOtherMapResource and (dataItem.MapResource <> '') then
    begin
      AResName := dataItem.MapResource;
      break;
    end else
      mapNode := mapNode.Parent;
  until (mapnode = nil);

  if mapNode <> nil then
  begin
    dataNode := mapNode;
    if ANodeDataItem.UseOtherMapResource then
    begin
      if ANodeDataItem.OtherMapDataLevelDist = 0 then
        dataNode := dataNode.GetFirstSibling
      else
        for i := 1 to ANodeDataItem.OtherMapDataLevelDist do
        begin
          if dataNode = nil then
            raise Exception.Create('Incorret map information');
          dataNode := datanode.GetFirstChild;
        end;
      APlotChildNodes := ANodeDataItem.OtherMapDataAtChildLevel;
    end else
    begin
      if ANodeDataItem.MapDataLevelDist = 0 then
        dataNode := dataNode.GetFirstSibling
      else
        for i := 1 to ANodeDataItem.MapDataLevelDist do
        begin
          if dataNode = nil then
            raise Exception.Create('Incorrect map information');
          dataNode := datanode.GetFirstChild;
        end;
      APlotChildNodes := ANodeDataItem.MapDataAtChildLevel;
    end;
    ANode := dataNode;
  end;

  if AResName = '' then
  begin
    AResName := WorldMapResName;
    ANode := TreeView.Items.GetFirstNode.GetFirstChild;
    APlotChildNodes := true;
  end;
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

function TMainForm.IsTimeSeries: Boolean;
begin
  Result := GetDataType() <> dtCumVsNewCases;
end;

// Populates the treeview with the locations.
procedure TMainForm.LoadLocations;
var
  ok: Boolean;
begin
  if not DirectoryExists(DataDir) then
  begin
    CreateDir(DataDir);
    MessageDlg('Data directory created. Please click "Update files".', mtInformation, [mbOK], 0);
    exit;
  end;

  Screen.Cursor := crHourglass;
  TreeView.Items.BeginUpdate;
  try
    TreeView.Items.Clear;

    with TJohnsHopkinsDatasource.Create(DataDir) do
    try
      ok := LoadLocations(TreeView) and LoadData(TreeView, nil);
      if not ok then
      begin
        MessageDlg('Local data files not found. Please click "Update files".', mtError, [mbOk], 0);
        exit;
      end;
    finally
      Free;
    end;

    {$IFDEF RKI}
    with TRobertKochDatasource.Create(DataDir) do
    try
      LoadLocations(TreeView);
    finally
      Free;
    end;
    {$ENDIF}

  finally
    TreeView.Items.EndUpdate;
    UpdateActionStates;
    Screen.Cursor := crDefault;
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
      ser := TChartSeries(ASender.PointStart.Series);
      s := Format('Daily growth: %.0f %% / Doubles every %.1f days --> %.0n cases in 1 week --> %.0n cases in 2 weeks / Reproduction number R0: %.1f', [
        (exp(FFitCoeffs[1]) - 1)*100,
        -ln(0.5) / FFitCoeffs[1],
        exp(FFitCoeffs[1] * 7) * ser.YValue[ser.Count-1],
        exp(FFitCoeffs[1] * 14) * ser.YValue[ser.Count-1],
        FFitCoeffs[1] * InfectiousPeriod + 1
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

procedure TMainForm.PaletteListboxGetColors(Sender: TCustomColorListBox;
  Items: TStrings);
var
  i: Integer;
  item, nextItem: TPaletteItem;
  m: Double;
  msk, mask1, mask2: String;
begin
  m := FPalette.Multiplier;
  case GetMapDataType of
    mdtNormalizedNewConfirmed: msk := '%.0f';
    mdtNormalizedNewDeaths: msk := '%.2g';
    mdtRValue: msk := '%.2f';
  end;
  mask1 := '>' + msk;
  mask2 := msk + '-' + msk;

  Items.Clear;
  Items.AddObject('(no data)', TObject(PtrInt(FPalette.BaseColor)));
  for i := 0 to High(FPalette.Items)-1 do
  begin
    item := FPalette.Items[i];
    nextItem := FPalette.Items[i+1];
    Items.AddObject(Format(mask2, [item.Value*m, nextitem.Value*m]), TObject(PtrInt(item.Color)));
  end;
  Items.AddObject(Format(mask1, [nextItem.Value*m]), TObject(PtrInt(nextItem.Color)));
end;


(*
procedure TMainForm.PopulateCumulativeSeries(const ADates, AValues: TStringArray;
  ASeries: TChartSeries);
var
  predata_phase: Boolean;
  start_date: TDate;
  i: Integer;
  X, Y: Double;
begin
  predata_phase := acDataCommonStart.Checked;
  ASeries.Clear;
  for i := 4 to High(AValues) do
  begin
    X := StrToDate(ADates[i], cFormatSettings);
    Y := StrToFloat(AValues[i], cFormatSettings);
    if predata_phase then
    begin
      if Y < START_COUNT then
        Continue;
      predata_phase := false;
      start_date := X;
    end;
    if Y = 0 then Y := 0.1;
    if acDataCommonStart.Checked then
      X := X - start_date;
    ASeries.AddXY(X, Y);
  end;
end;

procedure TMainForm.PopulateNewCasesSeries(const ADates, AValues: TStringArray;
  ASeries: TChartSeries);
var
  predata_phase: Boolean;
  start_date: TDate;
  X, Y, Y0: Double;
  i: Integer;
begin
  predata_phase := acDataCommonStart.Checked;
  ASeries.Clear;

  Y0 := StrToFloat(AValues[4], cFormatSettings);
  for i := 5 to High(AValues) do begin
    X := StrToDate(ADates[i], cFormatSettings);
    Y := StrToFloat(AValues[i], cFormatSettings);
    if predata_phase then
    begin
      if Y < START_COUNT then begin
        Y0 := Y;
        Continue;
      end;
      predata_phase := false;
      start_date := X;
    end;
    if acDataCommonStart.Checked then
      X := X - start_date;
    ASeries.AddXY(X, Y - Y0);
    Y0 := Y;
  end;
end;
*)

procedure TMainForm.PopulatePaletteListbox(AMapDataType: TMapDataType);
begin
  case AMapDataType of
    mdtNormalizedNewConfirmed: FPalette.Init(clWhite, INCIDENCE_PALETTE_ITEMS, 1.0);
    mdtNormalizedNewDeaths: FPalette.Init(clWhite, INCIDENCE_PALETTE_ITEMS, 0.01);
    mdtRValue: FPalette.Init(clWhite, RVALUE_PALETTE_ITEMS, 1.0);
  end;

  PaletteListbox.Style := PaletteListbox.Style - [cbCustomColors];
  PaletteListbox.Style := PaletteListbox.Style + [cbCustomColors];
  PaletteListBox.Selected := clNone;
end;

procedure TMainForm.ReadCommandlineParams;
var
  i: Integer;
  s: String;
begin
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    if s[1] in ['-', '/'] then
    begin
      s := lowercase(Copy(s, 2, MaxInt));
      if s = 'portable' then
      begin
        PortableInstallation := true;
        DataDir := GetDataDir;
      end;
    end;
  end;
end;

procedure TMainForm.SelectNode(ANode: TTreeNode);
begin
  if (ANode = nil) or (ANode.Text = '') then
    exit;

  if FDisplayMode in [dmMap, dmBoth] then
    FMapFrame.ShowMap(ANode);

  if FDisplayMode in [dmTimeSeries, dmBoth] then
    FTimeSeriesFrame.ShowTimeSeries(ANode);
end;

(*
procedure TMainForm.ShowData(ANode: TTreeNode);
var
  counts: array[TCaseType] of string = ('', '', '', '');
  dates: array[TCaseType] of string = ('', '', '', '');
  dt: TDataType;
  caseType, ct: TCaseType;
  saX, saY: TStringArray;
  i, j: Integer;
  ser: TBasicPointSeries;
  Y, Y0, Yhalf, dY, dY0: Double;
  country, state, city: String;
  population: Integer;
  nDesc: Integer;
  dataSrcClass: TcDataSourceClass;
  src: TCustomChartSource;
  dataArr: TDataPointArray;
  RValueDone: Boolean;
  loc: PLocationParams;
begin
  if ANode = nil then
    exit;

  Screen.Cursor := crHourglass;
  try
    if not acChartOverlay.Checked then
      Clear(false);

    dataSrcClass := TJohnsHopkinsDataSource;
    GetLocation(ANode, country, state, city, population);

    {$IFDEF RKI}
    if ((ANode.Level = 0) and (ANode.Text = RKI_CAPTION)) or
       ((ANode.Level = 1) and (ANode.Parent.Text = RKI_CAPTION)) or
       ((ANode.Level = 2) and (ANode.Parent.Parent.Text = RKI_CAPTION))
    then begin
      dataSrcClass := TRobertKochDataSource;
      loc := PLocationParams(ANode.Data);
      if loc = nil then
        raise Exception.Create('Location cannot be nil.');

      if (ANode.Level = 1) then
      begin
        country := IntToStr(loc^.ID);
        state := '';
      end else
      if ANode.Level = 2 then
      begin
        state := FormatFloat('00000', loc^.ID);
        loc := PLocationParams(ANode.Parent.Data);
        country := IntToStr(loc^.ID);
      end;
    end;
    {$ENDIF}

    dt := GetDataType();
    RValueDone := false;

    for caseType in TCaseType do
    begin
      if (dt = dtRValue) then
      begin
        if RValueDone then
          Continue
        else
          ct := ctConfirmed
      end else
      if clbCases.Checked[ord(caseType)] then
        ct := caseType
      else
        Continue;

      with dataSrcClass.Create(DataDir) do
      try
        if not GetDataString(country, state, city, ct, dates[ct], counts[ct]) then
          Continue;
      finally
        Free;
      end;

      saX := dates[ct].Split(',', '"');
      saY := counts[ct].Split(',','"');

      ser := GetSeries(ANode, ct, dt);
      src := ser.Source;
      case dt of
        dtCumulative, dtNormalizedCumulative:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              PopulateCumulativeSeries(saX, saY, ser);
              if dt = dtNormalizedCumulative then
                NormalizeToPopulation(ser.ListSource, population, false);
            finally
              ser.EndUpdate;
              ser.Source := src;
            end;
          end;

        dtNewCases, dtNormalizedNewCases:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              PopulateNewCasesSeries(saX, saY, ser);
              if dt = dtNormalizedNewCases then
                NormalizeToPopulation(ser.ListSource, population, true);
            finally
              ser.EndUpdate;
              ser.Source := src;
            end;
          end;

        dtCumulativeCasesDoublingTime,
        dtNewCasesDoublingTime:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              if dt = dtCumulativeCasesDoublingTime then
                PopulateCumulativeSeries(saX, saY, ser)
              else
                PopulateNewCasesSeries(saX, saY, ser);
              EnableMovingAverage(ser, true, true);
              SeriesToArray(ser, dataArr);
              EnableMovingAverage(ser, false, true);

              for i := High(dataArr) downto 0 do
              begin
                Y := dataArr[i].Y;
                Y0 := Y;
                if Y < 100 then
                  Y := NaN
                else
                begin
                  Yhalf := Y0 * 0.5;
                  Y := NaN;
                  nDesc := 0;  // Data points found on descending part
                  for j := i-1 downto 0 do
                  begin
                    if (dataArr[j].Y <= Y0) then      // use ascending part only
                    begin
                      if (dataArr[j].Y <= Yhalf) then  // half value found
                      begin
                        Y := dataArr[i].X - dataArr[j].X;
                        break;
                      end
                      else
                        nDesc := 0;
                    end else
                      // Count values on descending part.
                      inc(nDesc);
                    // If there is a certaining number of points on descending part --> ignore point
                    if nDesc > 10 then
                    begin
                      Y := NaN;
                      break;
                    end;
                  end;
                end;
                ser.YValue[i] := Y;
              end;
            finally
              ser.EndUpdate;
            end;
          end;

        dtCumVsNewCases:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              // Get cumulative cases, smooth curve, store values in temp array
              PopulateCumulativeSeries(saX, saY, ser);
              EnableMovingAverage(ser, true, true);
              SeriesToArray(ser, dataArr);

              // Get new cases, do not smooth yet
              ser.Source := nil;
              PopulateNewCasesSeries(saX, saY, ser);

              // Replace x by cumulative data from temp array
              j := High(dataArr);
              for i := ser.Count-1 downto 0 do
              begin
                ser.ListSource.Item[i]^.X := dataArr[j].Y;
                ser.ListSource.item[i]^.Text := DateToStr(dataArr[j].X, cFormatSettings);
                dec(j);
              end;
            finally
              ser.EndUpdate;
              // Smooth new case data
              EnableMovingAverage(ser, true, true);
            end;
          end;

        dtRValue:
          begin
            ser.Source := nil;
            ser.BeginUpdate;
            try
              // Get new cases, smooth, store in temp array
              PopulateNewCasesSeries(saX, saY, ser);
              EnableMovingAverage(ser, true, true);
              SeriesToArray(ser, dataArr);
              ser.Source := nil;  // Make source writable again

              // Calculate R as ratio NewCases/NewCases(5 days earlier)
              ser.ListSource.YCount := 2;
              ser.ListSource.YErrorBarData.Kind := ebkChartSource;
              ser.ListSource.YErrorBarData.IndexPlus := 1;
              ser.ListSource.YErrorBarData.IndexMinus := -1;
              if (ser is TLineseries) then
                with TLineSeries(ser) do
                begin
                  YErrorBars.Visible := true;
                  YErrorBars.Pen.Color := LinePen.Color;
                end;
              for i:=ser.Count-1 downto 0 do
              begin
                ser.YValue[i] := NaN;
                if i >= InfectiousPeriod then
                begin
                  Y0 := dataArr[i - InfectiousPeriod].Y;
                  Y := dataArr[i].Y;
                  if (Y0 > 0) and (Y > 0) then
                  begin
                    dY0 := sqrt(Y0);
                    dY := sqrt(Y);
                    dY := dY0/Y0 + dY/Y;
                    if dY < 0.5 then begin
                      ser.YValues[i, 0] := Y / Y0;                     // R value
                      ser.YValues[i, 1] := ser.YValues[i, 0] * dY;     // error of R
                    end;
                  end;
                end;
              end;
              RValueDone := true;
            finally
              ser.EndUpdate;
            end;
          end;
      end;
    end;

    //LayoutBars;
    UpdateAffectedSeries;
    UpdateGrid;
    if population > 0 then
      FStatusText1 := Format('%s loaded (population %.0n)', [GetLocation(ANode), 1.0*population])
    else
      FStatusText1 := Format('%s loaded.', [GetLocation(ANode)]);
    UpdateStatusBar;
    UpdateActionStates;
  finally
    Screen.Cursor := crDefault;
  end;
end;
*)

{
procedure TMainForm.ShowTimeSeries(ANode: TTreeNode);
var
  caseType: TCaseType;
  dt: TDataType;
  pct: TPrimaryCaseType;
  i: Integer;
  ser: TBasicPointSeries;
  country, state, city: String;
  population: Int64;
  dataSrc: TcDataSource;
  values, valuesNew: TValueArray;
  data: TcDataItem;
  d: TDateTime;
  R, dR: Double;
begin
  if ANode = nil then
    exit;

  Screen.Cursor := crHourglass;
  try
    if not acChartOverlay.Checked then
      Clear(false);

    {$IFDEF RKI}
    // The RKI datamodule loads the data individually for each node
    // The JHU datamodule, on the other hand, already has all data ready.
    if TRobertKochDataSource.IsRKINode(ANode) then
    begin
      dataSrc := TRobertKochDatasource.Create(DataDir);
      try
        dataSrc.LoadData(TreeView, ANode);
      finally
        dataSrc.Free;
      end;
    end;
    {$ENDIF}

    dt := GetDataType();
    data := GetDataItem(ANode);
    d := data.FirstDate;

    for caseType in TCaseType do
    begin
      if not clbCases.Checked[ord(caseType)] then
        Continue;

      ser := GetSeries(ANode, caseType, dt);
      case dt of
        dtCumVsNewCases:
          begin
            values := data.GetSmoothedDataArray(caseType, dtCumulative, SmoothingRange);
            valuesNew := data.GetSmoothedDataArray(caseType, dtNewCases, SmoothingRange);
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              ser.Clear;
              for i := 0 to High(values) do
                if (values[i] > 0) and (valuesNew[i] > 0) then  // avoid issues with log axes
                  ser.AddXY(values[i], valuesNew[i], DateToStr(d+i, cFormatSettings));
            finally
              ser.EndUpdate;
            end;
          end;

        dtRValue:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 2;
            ser.ListSource.YErrorBarData.Kind := ebkChartSource;
            ser.ListSource.YErrorBarData.IndexPlus := 1;
            ser.ListSource.YErrorBarData.IndexMinus := -1;
            if (ser is TLineseries) then
              with TLineSeries(ser) do
              begin
                YErrorBars.Visible := true;
                YErrorBars.Pen.Color := LinePen.Color;
              end;
            ser.BeginUpdate;
            try
              ser.Clear;
              if caseType = ctSick then
                pct := pctConfirmed
              else
                pct := TPrimaryCaseType(caseType);
              for i := 0 to data.Count[pct]-1 do
              begin
                data.CalcRValue(i, R, dR);
                if (not IsNaN(R)) and (dR/R < 0.5) then
                  ser.AddXY(d + i, R, [dR])
                else
                  ser.AddXY(d + i, NaN);
              end;
            finally
              ser.EndUpdate;
            end;
          end;

        else
          values := data.GetDataArray(caseType, dt);
          ser.Source := nil;
          ser.ListSource.YCount := 1;
          ser.BeginUpdate;
          try
            ser.Clear;
            for i := 0 to High(values) do
              ser.AddXY(d + i, values[i]);
          finally
            ser.EndUpdate;
            if not (dt in [dtCumulativeCasesDoublingTime, dtNewCasesDoublingTime]) then
              EnableMovingAverage(ser, acDataMovingAverage.Checked, true);
          end;
      end;
        (*
        dtNewCases, dtNormalizedNewCases:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              PopulateNewCasesSeries(saX, saY, ser);
              if dt = dtNormalizedNewCases then
                NormalizeToPopulation(ser.ListSource, population, true);
            finally
              ser.EndUpdate;
              ser.Source := src;
            end;
          end;

        dtCumulativeCasesDoublingTime,
        dtNewCasesDoublingTime:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              if dt = dtCumulativeCasesDoublingTime then
                PopulateCumulativeSeries(saX, saY, ser)
              else
                PopulateNewCasesSeries(saX, saY, ser);
              EnableMovingAverage(ser, true, true);
              SeriesToArray(ser, dataArr);
              EnableMovingAverage(ser, false, true);

              for i := High(dataArr) downto 0 do
              begin
                Y := dataArr[i].Y;
                Y0 := Y;
                if Y < 100 then
                  Y := NaN
                else
                begin
                  Yhalf := Y0 * 0.5;
                  Y := NaN;
                  nDesc := 0;  // Data points found on descending part
                  for j := i-1 downto 0 do
                  begin
                    if (dataArr[j].Y <= Y0) then      // use ascending part only
                    begin
                      if (dataArr[j].Y <= Yhalf) then  // half value found
                      begin
                        Y := dataArr[i].X - dataArr[j].X;
                        break;
                      end
                      else
                        nDesc := 0;
                    end else
                      // Count values on descending part.
                      inc(nDesc);
                    // If there is a certaining number of points on descending part --> ignore point
                    if nDesc > 10 then
                    begin
                      Y := NaN;
                      break;
                    end;
                  end;
                end;
                ser.YValue[i] := Y;
              end;
            finally
              ser.EndUpdate;
            end;
          end;

        dtCumVsNewCases:
          begin
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
              // Get cumulative cases, smooth curve, store values in temp array
              PopulateCumulativeSeries(saX, saY, ser);
              EnableMovingAverage(ser, true, true);
              SeriesToArray(ser, dataArr);

              // Get new cases, do not smooth yet
              ser.Source := nil;
              PopulateNewCasesSeries(saX, saY, ser);

              // Replace x by cumulative data from temp array
              j := High(dataArr);
              for i := ser.Count-1 downto 0 do
              begin
                ser.ListSource.Item[i]^.X := dataArr[j].Y;
                ser.ListSource.item[i]^.Text := DateToStr(dataArr[j].X, cFormatSettings);
                dec(j);
              end;
            finally
              ser.EndUpdate;
              // Smooth new case data
              EnableMovingAverage(ser, true, true);
            end;
          end;

        dtRValue:
          begin
            ser.Source := nil;
            ser.BeginUpdate;
            try
              // Get new cases, smooth, store in temp array
              PopulateNewCasesSeries(saX, saY, ser);
              EnableMovingAverage(ser, true, true);
              SeriesToArray(ser, dataArr);
              ser.Source := nil;  // Make source writable again

              // Calculate R as ratio NewCases/NewCases(5 days earlier)
              ser.ListSource.YCount := 2;
              ser.ListSource.YErrorBarData.Kind := ebkChartSource;
              ser.ListSource.YErrorBarData.IndexPlus := 1;
              ser.ListSource.YErrorBarData.IndexMinus := -1;
              if (ser is TLineseries) then
                with TLineSeries(ser) do
                begin
                  YErrorBars.Visible := true;
                  YErrorBars.Pen.Color := LinePen.Color;
                end;
              for i:=ser.Count-1 downto 0 do
              begin
                ser.YValue[i] := NaN;
                if i >= InfectiousPeriod then
                begin
                  Y0 := dataArr[i - InfectiousPeriod].Y;
                  Y := dataArr[i].Y;
                  if (Y0 > 0) and (Y > 0) then
                  begin
                    dY0 := sqrt(Y0);
                    dY := sqrt(Y);
                    dY := dY0/Y0 + dY/Y;
                    if dY < 0.5 then begin
                      ser.YValues[i, 0] := Y / Y0;                     // R value
                      ser.YValues[i, 1] := ser.YValues[i, 0] * dY;     // error of R
                    end;
                  end;
                end;
              end;
              RValueDone := true;
            finally
              ser.EndUpdate;
            end;
          end;
      end;
    *)
    end;

    //LayoutBars;
    UpdateAffectedSeries;
    UpdateGrid;
    if population > 0 then
      FStatusText1 := Format('%s loaded (population %.0n)', [GetLocation(ANode), 1.0*population])
    else
      FStatusText1 := Format('%s loaded.', [GetLocation(ANode)]);
    UpdateStatusBar;
    UpdateActionStates;
    if acChartMap.Checked and acChartTimeSeries.Checked then
      DateIndicatorLine.Position := d + MapDateScrollbar.Position;

    LessChartSymbols;
  finally
    Screen.Cursor := crDefault;
  end;
end;
 }

procedure TMainForm.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  // This node contains the name of the resource with the map polygons.
  // --> nothing to free
  if (Node.Text <> '') and (Node.Text[1] = '(') then
    exit;

  if TObject(Node.Data) is TcDataItem then
    TcDataItem(Node.Data).Free;
end;

procedure TMainForm.TreeViewSelectionChanged(Sender: TObject);
begin
  SelectNode(TreeView.Selected);
end;

procedure TMainForm.UpdateActionStates;
begin
  FMapFrame.UpdateCmdStates;
  FTimeSeriesFrame.UpdateCmdStates;
end;

procedure TMainForm.UpdateData;
begin
  with TJohnsHopkinsDataSource.Create(DataDir) do
  try
    DownloadToCache;
  finally
    Free;
  end;

  {$IFDEF RKI}
  with TRobertKochDatasource.Create(DataDir) do
  try
    ClearCache;
  finally
    Free;
  end;
  {$ENDIF}

  LoadLocations;
                                   {
  FStatusText1 := 'Locations loaded.';
  FStatusText2 := '';
  UpdateStatusbar;
  }
end;

procedure TMainForm.UpdateDateIndicatorLine(ADate: TDate);
begin
  if Assigned(DateIndicatorLine) then
    DateIndicatorLine.Position := ADate;
end;

procedure TMainForm.UpdateInfectiousPeriod;
begin
  FMapFrame.UpdateInfectiousPeriod;
  FTimeSeriesFrame.UpdateInfectiousPeriod;
end;

procedure TMainForm.UpdateStatusbar(ASeparator: String = ' ');
begin
  (*
  if (FStatusText1 <> '') and (FStatusText2 <> '') then
    Statusbar.SimpleText := FStatusText1 + ASeparator + FStatusText2
  else if (FStatusText1 <> '') then
    Statusbar.SimpleText := FStatusText1
  else if (FStatusText2 <> '') then
    Statusbar.SimpleText := FStatusText2
  else
    Statusbar.SimpleText := '';
  Statusbar.Update;
  *)
end;

procedure TMainForm.UpdateTimeSeriesActions(Sender: TObject);
begin
  acDataMovingAverage.Checked := TimeSeriesSettings.MovingAverage;
end;

procedure TMainForm.LoadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  ws: TWindowState;
  n: Integer;
  isLog: Boolean;
begin
  ini := CreateIni;
  try
    acConfigAutoLoad.Checked := ini.ReadBool('MainForm', 'AutoLoad', acConfigAutoLoad.Checked);
    if not acConfigAutoLoad.Checked then
    begin
      {
      cmbMapDataType.ItemIndex := ord(mdtNormalizedNewConfirmed);
      cmbMapDataTypeChange(nil);
      cmbDataType.ItemIndex := ord(dtNormalizedNewCases);
      cmbDataTypeChange(nil);
      ShowCharts(vcBoth);
      }
      // <--------- FIX ME
      exit;
    end;

    // Avoid unnecessary repainting the maps
   // inc(FMapLock);

    acConfigAutoSave.Checked := ini.ReadBool('MainForm', 'AutoSave', acConfigAutoSave.Checked);

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
    acConfigHint.Checked := ini.ReadBool('MainForm', 'ShowHints', acConfigHint.Checked);
    acConfigHintExecute(nil);

    LeftPanel.Width := ini.ReadInteger('MainForm', 'LeftPanel', LeftPanel.Width);
    FDisplaySplitterPos := ini.ReadInteger('MainForm', 'DisplaySplitterPos', FDisplaySplitterPos);
    FTimeSeriesFrame.Height := FDisplaySplitterPos;

    SetDisplayMode(TDisplayMode(ini.ReadInteger('MainForm', 'DisplayMode', ord(FDisplayMode))));

    FMapFrame.LoadFromIni(ini);
    FTimeSeriesFrame.LoadFromIni(ini);
(*
    LeftPanel.Width := ini.ReadInteger('MainForm', 'LeftPanel', LeftPanel.Width);
    ChartListbox.Width := ini.ReadInteger('MainForm', 'RightPanel', ChartListBox.Width);
    PaletteListboxPanel.Width := ini.ReadInteger('MainForm', 'PaletteListbox', PaletteListboxPanel.Width);
    CasesPanel.Width := ini.ReadInteger('MainForm', 'CasesPanel', CasesPanel.Width);
    TimeSeriesGroup.Height := ini.ReadInteger('MainForm', 'TimeSeriesGroup', TimeSeriesGroup.Height);
    PageControl.ActivePageIndex := ini.ReadInteger('MainForm', 'PageControl', PageControl.ActivePageIndex);

    clbCases.Checked[0] := ini.Readbool('MainForm', 'ConfirmedCases', clbCases.Checked[0]);
    clbCases.Checked[1] := ini.ReadBool('MainForm', 'DeathCases', clbCases.Checked[1]);
    clbCases.Checked[2] := ini.ReadBool('MainForm', 'RecoveredCases', clbCases.Checked[2]);
    clbCases.Checked[3] := ini.ReadBool('MainForm', 'SickCases', clbCases.Checked[3]);

    n := 0;
    if ini.ReadBool('MainForm', 'ShowMap', acChartMap.Checked) then n := n or 1;
    if ini.ReadBool('MainForm', 'ShowTimeSeries', acChartTimeSeries.Checked) then n := n or 2;
    if n = 0 then n := 3;
    ShowCharts(TVisibleCharts(n-1));
    acChartMapExecute(nil);
    acDataMovingAverage.Checked := ini.ReadBool('MainForm', 'MovingAverage', acDataMovingAverage.Checked);
    acChartOverlay.Checked := ini.ReadBool('MainForm', 'Overlay', acChartOverlay.Checked);
    acChartHighlightWeekends.Checked := ini.ReadBool('MainForm', 'HighlightWeekends', acChartHighlightWeekends.Checked);

    n := ini.ReadInteger('MainForm', 'MapDataType', cmbMapDataType.ItemIndex);
    if (n >= 0) and (n < cmbMapDataType.Items.Count) then
    begin
      cmbMapDataType.ItemIndex := n;
      cmbMapDataTypeChange(nil);
    end;

    n := ini.ReadInteger('MainForm', 'DataType', cmbDataType.ItemIndex);
    if (n >= 0) and (n <= ord(High(TDataType))) then
      cmbDataType.ItemIndex := n;

    isLog := ini.ReadBool('MainForm', 'Logarithmic', acChartLogarithmic.Checked);

    case GetDataType() of
      dtCumulative, dtNormalizedCumulative:
        begin
          acChartLogarithmic.Checked  := isLog;
          UpdateAxes(false, isLog);
        end;
      dtCumVsNewCases:
        begin
          acChartLogarithmic.Checked := isLog;
          UpdateAxes(isLog, isLog);
        end;
      else
        UpdateAxes(false, false);
    end;

    InfectiousPeriod := ini.ReadInteger('Params', 'InfectiousPeriod', InfectiousPeriod);
    SmoothingRange := ini.ReadInteger('Params', 'SmoothingRange', SmoothingRange);
    RRange := (SmoothingRange - 1) div 2;

    cmbDataType.Items[ord(dtRValue)] := Format('Reproduction number (%d d)', [InfectiousPeriod]);
    cmbDataTypeChange(nil);
    MovingAverageInfo.Caption := Format('(%d days)', [SmoothingRange]);

    // Release painting of map. Repainting itself is done by the caller.
    dec(FMapLock);
    *)
  finally
    UpdateActionStates;
    ini.Free;
  end;
end;

procedure TMainForm.SaveIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    ini.WriteBool('MainForm', 'AutoSave', acConfigAutoSave.Checked);
    if not acConfigAutoSave.Checked then
      exit;

    ini.WriteBool('MainForm', 'AutoLoad', acConfigAutoLoad.Checked);

    ini.WriteInteger('MainForm', 'WindowState', ord(WindowState));
    if WindowState = wsNormal then
    begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;

    ini.WriteBool('MainForm', 'ShowHints', acConfigHint.Checked);
    ini.WriteInteger('MainForm', 'LeftPanel', LeftPanel.Width);
    ini.WriteInteger('MainForm', 'DisplayMode', ord(FDisplayMode));
    ini.WriteInteger('MainForm', 'DisplaySplitterPos', FDisplaySplitterPos);

    FMapFrame.SaveToIni(ini);
    FTimeSeriesFrame.SaveToIni(ini);
                         (*
    if acChartMap.Checked then
      ini.WriteInteger('MainForm', 'PaletteListbox', PaletteListboxPanel.Width);
    if acChartTimeSeries.Checked then
      ini.WriteInteger('MainForm', 'RightPanel', ChartListBox.Width);
  ini.WriteInteger('MainForm', 'CasesPanel', CasesPanel.Width);
  ini.WriteInteger('MainForm', 'TimeSeriesGroup', TimeSeriesGroup.Height);
  ini.WriteInteger('MainForm', 'PageControl', PageControl.ActivePageIndex);

    ini.WriteBool('MainForm', 'ConfirmedCases', clbCases.Checked[0]);
    ini.WriteBool('MainForm', 'DeathCases', clbCases.Checked[1]);
    ini.WriteBool('MainForm', 'RecoveredCases', clbCases.Checked[2]);
    ini.WriteBool('MainForm', 'SickCases', clbCases.Checked[3]);
    ini.WriteInteger('MainForm', 'DataType', cmbDataType.ItemIndex);
    ini.WriteInteger('MainForm', 'MapDataType', cmbMapDataType.ItemIndex);

    ini.WriteBool('MainForm', 'ShowMap', acChartMap.Checked);
    ini.WriteBool('MainForm', 'ShowTimeSeries', acChartTimeSeries.Checked);
    ini.WriteBool('MainForm', 'MovingAverage', acDataMovingAverage.Checked);
    ini.WriteBool('MainForm', 'Overlay', acChartOverlay.Checked);
    ini.WriteBool('MainForm', 'HighlightWeekends', acChartHighlightWeekends.Checked);
    if GetDataType() in [dtCumulative, dtNormalizedCumulative] then
      ini.WriteBool('MainForm', 'Logarithmic', acChartLogarithmic.Checked);
                          *)

    ini.WriteInteger('Params', 'InfectiousPeriod', InfectiousPeriod);
    ini.WriteInteger('Params', 'SmoothingRange', SmoothingRange);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.SetDisplayMode(AMode: TDisplayMode);
begin
  {
  if FDisplayMode = dmBoth then
    FDisplaySplitterPos := FTimeSeriesFrame.Height;
  }
  FDisplayMode := AMode;

  case FDisplayMode of
    dmMap:
      begin
        acChartMap.Checked := true;
        FTimeSeriesFrame.Hide;
        FDisplaySplitter.Hide;
        FMapFrame.Align := alClient;
        FMapFrame.Show;
        FMapFrame.OnDateSelect := nil;
      end;
    dmTimeSeries:
      begin
        acChartTimeSeries.Checked := true;
        FMapFrame.Hide;
        FDisplaySplitter.Hide;
        FTimeSeriesFrame.Show;
        FTimeSeriesFrame.Align := alClient;
        FTimeSeriesFrame.ShowDateIndicatorLine(false);
      end;
    dmBoth:
      begin
        acChartBoth.Checked := true;
        FMapFrame.Parent := DisplayPanel;
        FMapFrame.Show;
        FTimeSeriesFrame.Show;
        FTimeSeriesFrame.ShowDateIndicatorLine(true);
        FDisplaySplitter.Show;
        FTimeSeriesFrame.Align := alBottom;
        FTimeSeriesFrame.Height := FDisplaySplitterPos;
        FDisplaySplitter.Align := alBottom;
        FDisplaySplitter.Top := 0;
        FMapFrame.Align := alClient;
        FMapFrame.OnDateSelect := @FTimeSeriesFrame.UpdateDateIndicatorLine;
      end;
  end;
end;

procedure TMainForm.ShowInfoHandler(Sender: TObject; const ATitle, AInfos: String);
begin
  // The labels are word-wrapped. We add LineEndings to keep the height constant.
  if ATitle = '' then
    NameLabel.Caption := ' ' + LineEnding + ' '
  else
    NameLabel.Caption := ATitle;

  if AInfos = '' then
    InfoLabel.Caption := LineEnding + LineEnding + LineEnding + LineEnding + LineEnding
  else
    InfoLabel.Caption := AInfos;
end;

procedure TMainForm.ShowVersionInfo;
begin
  Caption := Format('%s (%s)', [APP_TITLE, GetVersionStr]);
end;

procedure TMainForm.TimeSeriesFrameResize(Sender: TObject);
begin
  if FDisplayMode = dmBoth then
    FDisplaySplitterPos := FTimeSeriesFrame.Height;
end;

procedure TMainForm.ToolBarResize(Sender: TObject);
begin
  tbAbout.Left := Toolbar.Width - tbAbout.Width;
end;

initialization
  BaseDate := EncodeDate(2020, 1, 1);

end.

