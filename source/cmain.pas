unit cMain;

{$mode objfpc}{$H+}
{.$define RKI}
{$define USE_BARSERIES}

// Es gibt noch ein Define DEBUG_LOCATIONPARAMS in den Projekt-Optionen.


interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Grids, Types, LCLVersion, Menus, ActnList,
  StdActns, CheckLst,
  TAGraph, TAIntervalSources, TASeries, TAChartListbox, TALegend, TASources,
  TACustomSeries, TATransformations, TATools, TAFuncSeries, TADataTools,
  TAChartUtils,TADrawUtils,
  cGlobal, cDataSource;

type

  TCountArray = array of Integer;
  TDateArray = array of TDate;
  TDataPointArray = array of TDoublePoint;

  { TMainForm }

  TMainForm = class(TForm)
    acDataUpdate: TAction;
    acAbout: TAction;
    acDataClear: TAction;
    acChartLogarithmic: TAction;
    acConfigHint: TAction;
    acTableSave: TAction;
    acConfigAuto: TAction;
    acChartLinear: TAction;
    acChartCopyToClipboard: TAction;
    acChartOverlay: TAction;
    acDataCommonStart: TAction;
    acDataMovingAverage: TAction;
    acInfectiousPeriod: TAction;
    acSmoothingRange: TAction;
    acChartHighlightWeekends: TAction;
    ActionList: TActionList;
    Chart: TChart;
    BottomAxisTransformations: TChartAxisTransformations;
    BottomAxisLogTransform: TLogarithmAxisTransform;
    ChartToolset: TChartToolset;
    acFileExit: TFileExit;
    cbMovingAverage: TCheckBox;
    clbCases: TCheckListBox;
    cmbDataType: TComboBox;
    lblCases: TLabel;
    lblDataType: TLabel;
    lblTableHint: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    mnuChartHighlightWeekends: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuCommonStart: TMenuItem;
    mnuMovingAverage: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    mnuTable: TMenuItem;
    mnuDataUpdate: TMenuItem;
    mnuDataClear: TMenuItem;
    mnuData: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuConfigHints: TMenuItem;
    mnuConfig: TMenuItem;
    mnuChartLogarithmic: TMenuItem;
    mnuChart: TMenuItem;
    mnuFileQuit: TMenuItem;
    mnuFile: TMenuItem;
    Panel1: TPanel;
    ProgressBar: TProgressBar;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tbAbout: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
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
    ImageList: TImageList;
    LeftAxisTransformations: TChartAxisTransformations;
    LeftAxisLogTransform: TLogarithmAxisTransform;
    ChartListbox: TChartListbox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    LeftSplitter: TSplitter;
    RightSplitter: TSplitter;
    StatusBar: TStatusBar;
    TreeView: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acChartCopyToClipboardExecute(Sender: TObject);
    procedure acChartHighlightWeekendsExecute(Sender: TObject);
    procedure acChartLinearExecute(Sender: TObject);
    procedure acChartLogarithmicExecute(Sender: TObject);
    procedure acChartOverlayExecute(Sender: TObject);
    procedure acConfigAutoExecute(Sender: TObject);
    procedure acConfigHintExecute(Sender: TObject);
    procedure acDataClearExecute(Sender: TObject);
    procedure acDataCommonStartExecute(Sender: TObject);
    procedure acDataMovingAverageExecute(Sender: TObject);
    procedure acDataUpdateExecute(Sender: TObject);
    procedure acInfectiousPeriodExecute(Sender: TObject);
    procedure acNormalizeToPopulationExecute(Sender: TObject);
    procedure acSmoothingRangeExecute(Sender: TObject);
    procedure acTableSaveExecute(Sender: TObject);
    procedure ChartBeforeCustomDrawBackWall(ASender: TChart;
      ADrawer: IChartDrawer; const ARect: TRect; var ADoDefaultDrawing: Boolean);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListboxCheckboxClick(ASender: TObject; AIndex: Integer);
    procedure clbCasesClickCheck(Sender: TObject);
    procedure cmbDataTypeChange(Sender: TObject);
    procedure CrossHairToolDraw(ASender: TDataPointDrawTool);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure MeasurementToolGetDistanceText(ASender: TDataPointDistanceTool;
      var AText: String);
    procedure MeasurementToolMeasure(ASender: TDataPointDistanceTool);
    procedure PageControlChange(Sender: TObject);
    procedure ToolBarResize(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);

  private
    FMeasurementSeries: TFuncSeries;
    FFitCoeffs: array[0..1] of Double;
    FStatusText1, FStatusText2: String;

    function CalcFit(ASeries: TBasicChartSeries; xmin, xmax: Double): Boolean;
    procedure CalcFitCurveHandler(const AX: Double; out AY: Double);
    procedure Clear(UnselectTree: Boolean = true);
    procedure CreateMeasurementSeries;
    procedure DownloadMsgHandler(Sender: TObject; const AMsg1, AMsg2: String; APercentage: Integer);
    procedure EnableMovingAverage(ASeries: TChartSeries; AEnabled, AStrict: Boolean);
    function GetCellText(ACol, ARow: Integer): String;
    function GetDataItem(ANode: TTreeNode): TcDataItem;
    function GetDataType: TDataType;
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState, ACity: String; out APopulation: Integer);
    function GetSeries(ANode: TTreeNode; ACaseType: TCaseType; ADataType: TDataType): TBasicPointSeries;
    procedure InitShortCuts;
    function IsTimeSeries: Boolean;
//    procedure LayoutBars;
    procedure LoadLocations;
//    procedure NormalizeToPopulation(ASource: TListChartSource; APopulation: Integer; PerWeek: Boolean);
//    procedure PopulateCumulativeSeries(const ADates, AValues: TStringArray; ASeries: TChartSeries);
//    procedure PopulateNewCasesSeries(const ADates, AValues: TStringArray; ASeries: TChartSeries);
    procedure RestoreSeriesNodesFromList(AList: TFPList);
    procedure SeriesToArray(ASeries: TChartSeries; out AData: TDataPointArray);
    procedure ShowData(ANode: TTreeNode);
    procedure StatusMsgHandler(Sender: TObject; const AMsg1, AMsg2: String);
    function StoreSeriesNodesInList: TFPList;
    procedure UpdateActionStates;
    procedure UpdateAffectedSeries;
    procedure UpdateAxes(LogarithmicX, LogarithmicY: Boolean);
    procedure UpdateData;
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
  LCLIntf, Math, IniFiles, DateUtils, InterfaceBase, LCLPlatformDef,
  // TAChart units
  TATypes, TAMath, TACustomSource, TAFitLib,
  // project-specific units
  cJohnsHopkinsUniversity, {$IFDEF RKI}cRobertKochInstitut,{$ENDIF}
  cSeries, cUtils, cAbout;

const
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

  START_COUNT = 100;

var
  // The BaseDate must be subtracted from the date values for fitting on the
  // log scale to prevent a floating point overflow.
  // This is not used, however, when dates have a common start.
  // DateOffset is the correct offset to be used in any case
  BaseDate: TDate;
  DateOffset: TDate;

  // DataDir is the directory in which the downloaded csv files are found.
  DataDir: String;


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

procedure TMainForm.acChartCopyToClipboardExecute(Sender: TObject);
begin
  Chart.Color := clWhite;
  Chart.CopyToClipboardBitmap;
  Chart.Color := cldefault;
end;

procedure TMainForm.acChartHighlightWeekendsExecute(Sender: TObject);
begin
  Chart.Invalidate;
end;

procedure TMainForm.acChartLinearExecute(Sender: TObject);
begin
  UpdateAxes(false, false);
end;

procedure TMainForm.acChartLogarithmicExecute(Sender: TObject);
begin
  UpdateAxes(not IsTimeSeries(), true);
end;

procedure TMainForm.acChartOverlayExecute(Sender: TObject);
begin
  // Checked state is evaluated when adding curves.
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

procedure TMainForm.acDataCommonStartExecute(Sender: TObject);
var
  logX, logY: Boolean;
  i: Integer;
begin
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
      ShowData(TcLineSeries(Chart.Series[i]).Node)
    else if Chart.Series[i] is TcBarSeries then
      ShowData(TcBarSeries(Chart.Series[i]).Node);
end;

procedure TMainForm.acDataMovingAverageExecute(Sender: TObject);
var
  i: Integer;
  isMovingAverage: Boolean;
begin
  isMovingAverage := acDataMovingAverage.Checked and
    ( GetDataType in [dtCumulative, dtNormalizedCumulative, dtNormalizedNewCases, dtNewCases] );

  for i:=0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TChartSeries then
      EnableMovingAverage(TChartSeries(Chart.Series[i]), isMovingAverage, false);

  cbMovingAverage.Caption := Format('Moving average (%d days)', [SmoothingRange]);
  UpdateGrid;
end;

procedure TMainForm.acDataUpdateExecute(Sender: TObject);
begin
  Progressbar.Show;
  UpdateData;
  Progressbar.Hide;
end;

procedure TMainForm.acInfectiousPeriodExecute(Sender: TObject);
var
  n: Integer;
  s: String;
  L: TFPList;
begin
  s := IntToStr(InfectiousPeriod);
  if InputQuery('Infectious period', 'Days:', s) then
  begin
    if TryStrToInt(s, n) and (n > 0) then
    begin
      InfectiousPeriod := n;
      cmbDataType.Items[ord(dtRValue)] := Format('Reproduction number (%d d)', [InfectiousPeriod]);

      // Recalculate the currently loaded data
      L := StoreSeriesNodesInList();
      try
        RestoreSeriesNodesFromList(L);
      finally
        L.Free;
      end;
    end else
      MessageDlg('No valid number.', mtError, [mbOk], 0);
  end;
end;

procedure TMainForm.acNormalizeToPopulationExecute(Sender: TObject);
var
  L: TFPList;
begin
  L := StoreSeriesNodesInList();
  try
    RestoreSeriesNodesFromList(L);
  finally
    L.Free;
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

        cbMovingAverage.Caption := Format('Moving average (%d days)', [SmoothingRange]);

        // Recalculate the currently loaded data
        L := StoreSeriesNodesInList();
        try
          RestoreSeriesNodesFromList(L);
        finally
          L.Free;
        end;

      end else
        MessageDlg('Only odd number of days allowed (i.e. center day is included).', mtError, [mbOk], 0);
    end else
      MessageDlg('No valid number.', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.acTableSaveExecute(Sender: TObject);
const
  ASCENDING = false;
var
  r, c: Integer;
  r_start, r_end, r_delta: Integer;
  F: TextFile;
  col: TGridColumn;
  ser: TChartSeries;
  dt: TDateTime;
  n: Integer;
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

    if ASCENDING then
    begin
      r_start := 1;
      r_end := Grid.RowCount;
      r_delta := +1;
    end else
    begin
      r_start := Grid.RowCount-1;
      r_end := 0;
      r_delta := -1;
    end;

    r := r_start;
    while r <> r_end do begin
//    for r := 1 to Grid.RowCount-1 do
//    begin
      dt := StrToDateTime(GetCellText(0, r));
      Write(F, FormatDateTime(SAVE_DATE_FORMAT, dt));
      for c := 1 to Grid.ColCount-1 do
      begin
        n := StrToInt(StripThousandSeparator(GetCellText(c, r), FormatSettings.ThousandSeparator));
        Write(F, #9, n);
      end;
      WriteLn(F);
      inc(r, r_delta);
    end;
    CloseFile(F);
  end;
end;

procedure TMainForm.ChartBeforeCustomDrawBackWall(ASender: TChart;
  ADrawer: IChartDrawer; const ARect: TRect; var ADoDefaultDrawing: Boolean);
const
  SATURDAY = 7;
var
  ext: TDoubleRect;
  x: Double;
begin
  if (not acChartHighlightWeekends.Checked) or (not IsTimeSeries) then
    exit;

  ext := ASender.LogicalExtent;
  if (ext.a.x = -1) and (ext.b.x = +1) then
    exit;

  ADrawer.BrushColor := ASender.BackColor;
  ADrawer.Rectangle(ARect);

  x := trunc(ext.a.x);
  while DayOfWeek(x) <> SATURDAY do
    x += 1;

  ADrawer.BrushColor := ASender.BottomAxis.Grid.Color;
  while (x <= ext.b.x) do
  begin
    ADrawer.FillRect(ASender.XGraphToImage(x), ARect.Top+1, ASender.XGraphToImage(x+1), ARect.Bottom-1);
    x += 7;
  end;

  ADoDefaultDrawing := false;
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
  if ASeries is TFuncSeries then begin
    ASkip := true;
    exit;
  end;

  if (not ASeries.Active) and (TChartSeries(ASeries).Count = 0) then
    for ct in TCaseType do
      if (pos(CASETYPE_NAMES[ct], ASeries.Title) > 0) or
         (pos(R_NUMBER_STR, ASeries.Title) > 0) then
      begin
        ASkip := true;
        exit;
      end;
end;

procedure TMainForm.ChartListboxCheckboxClick(ASender: TObject; AIndex: Integer);
begin
//  LayoutBars;
  UpdateAffectedSeries;
end;

procedure TMainForm.clbCasesClickCheck(Sender: TObject);
var
  L: TFPList;
begin
  L := StoreSeriesNodesInList();
  try
    TreeViewClick(nil);
    RestoreSeriesNodesFromlist(L);
  finally
    L.Free;
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
begin
  if ASender.Series = nil then
    FStatusText1 := ''
  else
  if ASender.Series is TChartSeries then
  begin
    dt := GetDataType();
    ser := TChartSeries(ASender.Series);
    x := ser.GetXValue(ASender.PointIndex);
    y := ser.GetYValue(ASender.PointIndex);
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
    ASender.Handled;
  end;
  UpdateStatusBar;
end;

procedure TMainForm.Clear(UnselectTree: Boolean = true);
begin
  if UnselectTree then
    TreeView.Selected := nil;
  Chart.ClearSeries;
  CreateMeasurementSeries;
  UpdateGrid;
  UpdateActionStates;
end;

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
              lblTableHdr.Caption := Format('Cumulative cases per %.0n persons', [1.0 * PopulationRef]);
            dtNewCases:
              lblTableHdr.Caption := 'New cases per day';
            dtNormalizedNewCases:
              lblTableHdr.Caption := Format('New cases per %.0n persons and week', [1.0 * PopulationRef])
            else
              raise Exception.Create('Data type not handled by cmbDataTypeChange');
          end;
          lblTableHint.Caption := '';
          Chart.LeftAxis.Title.Caption := lblTableHdr.Caption;
          Chart.BottomAxis.Title.Caption := 'Date';
          UpdateAxes(false, acChartLogarithmic.Checked);
          MeasurementTool.Enabled := true;
          acDataMovingAverage.Enabled := true;
          clbCases.Enabled := true;
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
          clbCases.Enabled := true;
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
          clbCases.Enabled := true;
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
          clbCases.Enabled := false;
        end;
      else
        raise Exception.Create('Data type unsupported.');
    end;

    acDataMovingAverageExecute(nil);
    RestoreSeriesNodesFromlist(L);
    UpdateActionStates;

  finally
    L.Free;
  end;
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
  Chart.AddSeries(FMeasurementSeries);
end;

procedure TMainForm.DownloadMsgHandler(Sender: TObject; const AMsg1, AMsg2: String;
  APercentage: Integer);
begin
  Progressbar.Position := APercentage;
  Progressbar.Update;

  FStatusText1 := AMsg1;
  FStatusText2 := AMsg2;
  UpdateStatusbar;

  // Make sure that status msg is painted in Linux
  Application.ProcessMessages;
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
  p3: Integer;
begin
  // Workaround for gtk2 issue: Listbox.ItemHeight is 0 only after FormShow.
  if GetDefaultLCLWidgetType = lpGtk2 then
  begin
    p3 := Scale96ToForm(3);
    clbCases.Height := clbCases.Items.Count * clbCases.ItemHeight + 2*p3;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    SaveIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Narrower input box
  cInputQueryEditSizePercents := 0;

  DataDir := Application.Location + DATA_DIR;  // DATA_DIR ends with a path delimiter
  DateOffset := BaseDate;

  WheelZoomTool.ZoomFactor := 1.05;
  WheelZoomTool.ZoomRatio := 1.0 / WheelZoomTool.ZoomFactor;

  {$IF LCL_FullVersion >= 2010000}
  ZoomDragTool.LimitToExtent := [zdDown, zdLeft, zdRight, zdUp];
  PanDragTool.LimitToExtent := [pdDown, pdLeft, pdRight, pdUp];
  {$ENDIF}

  clbCases.Checked[0] := true;
  Grid.RowHeights[0] := 3 * Grid.Canvas.TextHeight('Tg') + 2* varCellPadding;

  PageControl.ActivePageIndex := 0;
  Progressbar.Parent := Statusbar;
  Progressbar.Align := alRight;

  CreateMeasurementSeries;
  InitShortCuts;

  LoadLocations;
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

  w := 0;
  for s in clbCases.Items do
    w := Max(w, clbCases.Canvas.TextWidth(s));
  clbCases.Width := w + GetSystemMetrics(SM_CXVSCROLL) + p24;
  if clbCases.ItemHeight > 0 then
    clbCases.Height := clbCases.Items.Count * clbCases.ItemHeight + 2*p3;

  LeftPanel.Constraints.MinWidth := cbMovingAverage.Width + clbCases.Width + p24;
end;

function TMainForm.GetCellText(ACol, ARow: Integer): String;
var
  col: TGridColumn;
  ser: TChartSeries;
  r: Integer;
  dt: TDateTime;
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
      if IsTimeSeries() then
        dt := ser.XValue[r]
      else
        dt := ScanDateTime('mm"/"dd"/"yyyy', ser.Source.Item[r]^.Text);
      Result := DateToStr(dt);
    end else
    if ARow > 0 then
    begin
      col := Grid.Columns[ACol - 1];
      ser := TChartSeries(col.Tag);
      r := ser.Count - ARow;
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
  Result := TDataType(cmbDataType.ItemIndex);
end;

procedure TMainForm.GetLocation(ANode: TTreeNode;
  out ACountry, AState, ACity: String; out APopulation: Integer);
var
  item: TcDataItem;
  loc: PLocationParams;
begin
  ACountry := '';
  AState := '';
  ACity := '';

  case ANode.Level of
    0: ACountry := ANode.Text;
    1: begin
         AState := ANode.Text;
         ACountry := ANode.Parent.Text;
       end;
    2: begin
         ACity := ANode.Text;
         AState := ANode.Parent.Text;
         ACountry := ANode.Parent.Parent.Text;
       end;
  end;
  {
  if ANode.Parent = nil then begin
    ACountry := ANode.Text;
    AState := '';
    ACity := '';
  end else
  begin
    ACountry := ANode.Parent.Text;
    AState := ANode.Text;

  end;
  }
  if TObject(ANode.Data) is TcDataItem then
  begin
    item := TcDataitem(ANode.Data);
    if item <> nil then
      APopulation := item.Population;
  end else
  begin
    // !!!!!!!!! to be removed when TcDataItem is fully established
    loc := PLocationParams(ANode.Data);
    if loc <> nil then
      APopulation := loc^.Population;
  end;
end;

function TMainForm.GetLocation(ANode: TTreeNode): String;
var
  country, state, city: String;
  population: Integer;
begin
  GetLocation(ANode, country, state, city, population);
  Result := country;
  if state <> '' then begin
    Result := Result + '/' + state;
    if city <> '' then
      Result := Result + ' / ' + city;
  end;
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

{
procedure TMainForm.LayoutBars;
var
  i, j, n: Integer;
  ser: TBarSeries;
begin
  if not (GetDataType() in [dtNewCases, dtNormalizedNewCases, dtDoublingTime]) then
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
}

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
      ok := LoadLocations(TreeView) and LoadData(TreeView);
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
    TreeView.AlphaSort;
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
                          (*
procedure TMainForm.NormalizeToPopulation(ASource: TListChartSource;
  APopulation: Integer; PerWeek: Boolean);
var
  i, j: Integer;
  sum: Double;
begin
  if APopulation <= 0 then
    exit;

  if PerWeek then
    for i := ASource.Count-1 downto 0 do begin
      // Calculate 1 week sum
      sum := ASource.Item[i]^.Y;
      for j := 1 to 6 do   // 1 week
        if i - j >= 0 then
          sum := sum + ASource.Item[i-j]^.Y;
      ASource.Item[i]^.Y := sum / APopulation * PopulationRef;
    end
  else
    for i := ASource.Count-1 downto 0 do
      ASource.Item[i]^.Y := ASource.Item[i]^.Y / APopulation * PopulationRef;
end;                        *)

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  // FIXME: Chart menu is always hidden once Table menu was shown.
  {
  mnuChart.Visible := PageControl.ActivePageIndex = pgChart;
  mnuTable.Visible := PageControl.ActivePage = pgTable;
  }
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

procedure TMainForm.RestoreSeriesNodesFromList(AList: TFPList);
var
  i: Integer;
begin
  for i := 0 to AList.Count-1 do
    ShowData(TTreeNode(AList[i]));
end;

procedure TMainForm.SeriesToArray(ASeries: TChartSeries; out AData: TDataPointArray);
var
  i: Integer;
begin
  SetLength(AData, ASeries.Count);
  for i := 0 to ASeries.Count-1 do
    AData[i] := ASeries.Source.Item[i]^.Point;
end;

{
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
}
procedure TMainForm.ShowData(ANode: TTreeNode);
var
  caseType: TCaseType;
  dt: TDataType;
  pct: TPrimaryCaseType;
  i, j: Integer;
  ser: TBasicPointSeries;
  country, state, city: String;
  population: Integer;
  dataSrcClass: TcDataSourceClass;
  src: TCustomChartSource;
  values, valuesNew: TValueArray;
  item: TcDataItem;
  d: TDateTime;
  R, dR: Double;
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
    (*

      !!!!!!!!!! DO NOT DELETE THIS FOR THE MOMEMENT !!!!!!!!!!!!

      Must download RKI data file and create a TcDataItem for the current node

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
    *)
    {$ENDIF}

    dt := GetDataType();
    item := GetDataItem(ANode);
    d := item.FirstDate;

    for caseType in TCaseType do
    begin
      if not clbCases.Checked[ord(caseType)] then
        Continue;

      ser := GetSeries(ANode, caseType, dt);
      src := ser.Source;
      case dt of
        dtCumVsNewCases:
          begin
            values := item.GetSmoothedDataArray(caseType, dtCumulative, SmoothingRange);
            valuesNew := item.GetSmoothedDataArray(caseType, dtNewCases, SmoothingRange);
            ser.Source := nil;
            ser.ListSource.YCount := 1;
            ser.BeginUpdate;
            try
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
              if caseType = ctSick then
                pct := pctConfirmed
              else
                pct := TPrimaryCaseType(caseType);
              for i := 0 to item.Count[pct]-1 do
              begin
                item.CalcRValue(i, R, dR);
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
          values := item.GetDataArray(caseType, dt);
          ser.Source := nil;
          ser.ListSource.YCount := 1;
          ser.BeginUpdate;
          try
            for i := 0 to High(values) do
              ser.AddXY(d + i, values[i]);
          finally
            ser.EndUpdate;
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
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.StatusMsgHandler(Sender: TObject; const AMsg1, AMsg2: String);
begin
  FStatusText1 := AMsg1;
  FStatusText2 := AMsg2;
  UpdateStatusbar;
end;

// Store nodes belonging to currently available series in a list
// Can be restored by calling RestoreSeriesNodesFromList.
function TMainForm.StoreSeriesNodesInList: TFPList;
var
  i: Integer;
begin
  Result := TFPList.Create;

  for i:=0 to ChartListbox.SeriesCount-1 do
    if ChartListbox.Series[i] is TcLineSeries then
      Result.Add(TcLineSeries(ChartListbox.Series[i]).Node)
    else if ChartListbox.Series[i] is TcBarSeries then
      Result.Add(TcBarSeries(ChartListbox.Series[i]).Node);
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
begin
  ShowData(TreeView.Selected);
end;

procedure TMainForm.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
var
  data: TcDataItem;
  loc: PLocationParams;
begin
  if TObject(Node.Data) is TcDataItem then
  begin
    data := TcDataItem(Node.Data);
    data.Free;
  end else
  begin
    // !!!!!!!!!! to be removed once TcDataItem is fully established
    loc := PLocationParams(Node.Data);
    if loc <> nil then
      Dispose(loc);
    Node.Data := nil;
  end;
end;

procedure TMainForm.UpdateActionStates;
begin
  acTableSave.Enabled := Chart.SeriesCount > 1;  // 1 series reserved for measurement
  acChartLogarithmic.Enabled := GetDataType() in [dtCumulative, dtNormalizedCumulative,
    dtNewCases, dtNormalizedNewCases, dtCumVsNewCases];
  acChartLinear.Enabled := acChartLogarithmic.Enabled;
end;

procedure TMainForm.UpdateAffectedSeries;
var
  i: Integer;
  s: String;
begin
  s := '';
  for i := 0 to Chart.SeriesCount-1 do
  begin
    // Avoid fitting to sick cases.
    if i mod (ord(High(TCaseType))+1) = 0 then
      Continue;
    if (Chart.Series[i] is TChartSeries) and Chart.Series[i].Active then
      s := s + ',' + IntToStr(Chart.Series[i].Index);
  end;
  Delete(s, 1, 1);
  CrossHairTool.AffectedSeries := s;
  MeasurementTool.AffectedSeries := s;
end;

procedure TMainForm.UpdateAxes(LogarithmicX, LogarithmicY: Boolean);
begin
  with Chart.BottomAxis do
    if LogarithmicX then
    begin
      Transformations := BottomAxisTransformations;
      Intervals.Options := Intervals.Options + [aipGraphCoords];
      Intervals.MaxLength := 150;
      Intervals.MinLength := 50;
      Intervals.Tolerance := 100;
      Marks.Source := nil;
      Marks.Format := '%0:.0n';
      Marks.Style := smsValue;
      //Title.Caption := 'Date';
    end else
    if IsTimeSeries() then
    begin
      Transformations := nil;
      if acDataCommonStart.Checked then
      begin
        Marks.Source := nil;
        Marks.Style := smsValue;
        Title.Caption := 'Days since ' + IntToStr(START_COUNT) + ' confirmed cases';
      end else
      begin
        Marks.Source := DateTimeIntervalChartSource;
        Marks.Style := smsLabel;
        Title.Caption := 'Date';
        // no need to reset Intervals here because they are handled by DateTimeInterval source on time series.
      end;
    end else
    begin
      Transformations := nil;
      Intervals.Options := Intervals.Options - [aipGraphCoords];
      Intervals.MaxLength := 150;
      Intervals.MinLength := 30;
      Intervals.Tolerance := 0;
      Marks.Source := nil;
      Marks.Style := smsValue;
      Marks.Format := '%0:.0n';
    end;

  with Chart.LeftAxis do
  begin
    if LogarithmicY then
    begin
      Transformations := LeftAxisTransformations;
      Intervals.Options := Intervals.Options + [aipGraphCoords];
      Intervals.MaxLength := 150;
      Intervals.MinLength := 50;
      Intervals.Tolerance := 100;
      Marks.Format := '%0:.0n';
    end else
    begin
      Transformations := nil;
      Intervals.Options := Intervals.Options - [aipGraphCoords];
      Intervals.MaxLength := 50;
      Intervals.MinLength := 10;
      Intervals.Tolerance := 0;
      Marks.Style := smsValue;
      if GetDataType() in [dtCumulative, dtCumVsNewCases] then
        Marks.Format := '%0:.0n'
      else
        Marks.Format := '%0:.9g';
    end;
  end;
  Chart.Extent.UseYMin := LogarithmicY or (GetDataType = dtRValue);
end;

procedure TMainForm.UpdateData;
begin
  with TJohnsHopkinsDataSource.Create(DataDir) do
  try
    OnDownloadMsg := @DownloadMsgHandler;
//    OnStatusMsg := @StatusMsgHandler;
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

  FStatusText1 := 'Locations loaded.';
  FStatusText2 := '';
  UpdateStatusbar;
end;

procedure TMainForm.UpdateGrid;
var
  n: Integer;
  i: Integer;
  ser: TChartSeries;
  s: String;
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
          s := StringReplace(ser.Title, ' ', LineEnding, []);
          if not IsTimeSeries() then
            s := s + LineEnding + 'New';
          Title.Caption := s;
          Tag := PtrInt(Chart.Series[i]);
        end;
        if not IsTimeSeries() then
          with Grid.Columns.Add do
          begin
            Title.Caption := StringReplace(ser.Title, ' ', LineEnding , []) + LineEnding + 'Total';
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
  Statusbar.Update;
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
  n: Integer;
  isLog: Boolean;
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

    clbCases.Checked[0] := ini.Readbool('MainForm', 'ConfirmedCases', clbCases.Checked[0]);
    clbCases.Checked[1] := ini.ReadBool('MainForm', 'DeathCases', clbCases.Checked[1]);
    clbCases.Checked[2] := ini.ReadBool('MainForm', 'RecoveredCases', clbCases.Checked[2]);
    clbCases.Checked[3] := ini.ReadBool('MainForm', 'SickCases', clbCases.Checked[3]);

    acDataMovingAverage.Checked := ini.ReadBool('MainForm', 'MovingAverage', acDataMovingAverage.Checked);
    acChartOverlay.Checked := ini.ReadBool('MainForm', 'Overlay', acChartOverlay.Checked);
    acChartHighlightWeekends.Checked := ini.ReadBool('MainForm', 'HighlightWeekends', acChartHighlightWeekends.Checked);

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

    acConfigHint.Checked := ini.ReadBool('MainForm', 'ShowHints', acConfigHint.Checked);
    acConfigHintExecute(nil);

    InfectiousPeriod := ini.ReadInteger('Params', 'InfectiousPeriod', InfectiousPeriod);
    SmoothingRange := ini.ReadInteger('Params', 'SmoothingRange', SmoothingRange);
    RRange := (SmoothingRange - 1) div 2;

    cmbDataType.Items[ord(dtRValue)] := Format('Reproduction number (%d d)', [InfectiousPeriod]);
    cmbDataTypeChange(nil);
    cbMovingAverage.Caption := Format('Moving average (%d days)', [SmoothingRange]);

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

    ini.WriteBool('MainForm', 'ConfirmedCases', clbCases.Checked[0]);
    ini.WriteBool('MainForm', 'DeathCases', clbCases.Checked[1]);
    ini.WriteBool('MainForm', 'RecoveredCases', clbCases.Checked[2]);
    ini.WriteBool('MainForm', 'SickCases', clbCases.Checked[3]);
    ini.WriteInteger('MainForm', 'DataType', cmbDataType.ItemIndex);

    ini.WriteBool('MainForm', 'MovingAverage', acDataMovingAverage.Checked);
    ini.WriteBool('MainForm', 'Overlay', acChartOverlay.Checked);
    ini.WriteBool('MainForm', 'HighlightWeekends', acChartHighlightWeekends.Checked);
    if GetDataType() in [dtCumulative, dtNormalizedCumulative] then
      ini.WriteBool('MainForm', 'Logarithmic', acChartLogarithmic.Checked);

    ini.WriteBool('MainForm', 'ShowHints', acConfigHint.Checked);

    ini.WriteInteger('Params', 'InfectiousPeriod', InfectiousPeriod);
    ini.WriteInteger('Params', 'SmoothingRange', SmoothingRange);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.ToolBarResize(Sender: TObject);
begin
  tbAbout.Left := Toolbar.Width - tbAbout.Width;
end;

initialization
  BaseDate := EncodeDate(2020, 1, 1);

end.

