unit cTimeSeriesFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Types, IniFiles, LCLVersion,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Menus, ActnList,
  TAGraph, TACustomSeries, TASeries, TAFuncSeries, TAChartListbox, TAIntervalSources,
  TALegend, TATransformations, TATools, TADataTools,
  cGlobal, cBasicFrame, cSeries, TADrawUtils;

type
  TUpdateTimeSeriesActions = procedure (Sender: TObject) of object;

  { TTimeSeriesFrame }

  TTimeSeriesFrame = class(TBasicFrame)
    acHighlightWeekends: TAction;
    acClear: TAction;
    acOverlayMode: TAction;
    acLinear: TAction;
    acLogarithmic: TAction;
    acInfected: TAction;
    acDeaths: TAction;
    acRecovered: TAction;
    acSick: TAction;
    acMovingAverage: TAction;
    BottomAxisLogTransform: TLogarithmAxisTransform;
    BottomAxisTransformations: TChartAxisTransformations;
    ChartListbox: TChartListbox;
    ChartToolset: TChartToolset;
    cmbDataType: TComboBox;
    CrossHairTool: TDataPointCrosshairTool;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    lblTableHdr: TLabel;
    lblTableHint: TLabel;
    LeftAxisLogTransform: TLogarithmAxisTransform;
    LeftAxisTransformations: TChartAxisTransformations;
    MeasurementTool: TDataPointDistanceTool;
    PanDragTool: TPanDragTool;
    DataTypePanel: TPanel;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    tbDivider2: TToolButton;
    tbSick: TToolButton;
    tbHighlightWeekends: TToolButton;
    tbClear: TToolButton;
    tbOverlay: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    tbDivider1: TToolButton;
    ToolButton5: TToolButton;
    tbDivider3: TToolButton;
    tbInfected: TToolButton;
    tbDeaths: TToolButton;
    tbRecovered: TToolButton;
    WheelZoomTool: TZoomMouseWheelTool;
    ZoomDragTool: TZoomDragTool;
    procedure acCasesExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure acHighlightWeekendsExecute(Sender: TObject);
    procedure acLinearExecute(Sender: TObject);
    procedure acLogarithmicExecute(Sender: TObject);
    procedure acMovingAverageExecute(Sender: TObject);
    procedure acOverlayModeExecute(Sender: TObject);

    procedure ChartBeforeCustomDrawBackWall(ASender: TChart; ADrawer: IChartDrawer;
      const ARect: TRect; var ADoDefaultDrawing: Boolean);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListboxClick(Sender: TObject);
    procedure ChartResize(Sender: TObject);
    procedure CheckedCasesChange(Sender: TObject);
    procedure cmbDataTypeChange(Sender: TObject);
    procedure CrossHairToolDraw(ASender: TDataPointDrawTool);

    procedure MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure MeasurementToolGetDistanceText(ASender: TDataPointDistanceTool; var AText: String);
    procedure MeasurementToolMeasure(ASender: TDataPointDistanceTool);

  private
    FDateIndicatorLine: TConstantLine;
    FMeasurementSeries: TFuncSeries;
    FFitCoeffs: array[0..1] of Double;
    FCheckedCases: TCaseTypes;
    FMenu: TMenuItem;
    FCurrentDate: TDate; // Date displayed by the DateIndicatorLine
    FOnUpdateActions: TUpdateTimeSeriesActions;

    function CalcFit(ASeries: TBasicChartSeries; xmin, xmax: Double): Boolean;
    procedure CalcFitCurveHandler(const AX: Double; out AY: Double);
    function CasesChecked: TCaseTypes;
    procedure CheckCases(ACaseTypes: TCaseTypes);
    procedure ClearAllSeries;
    procedure CreateMeasurementSeries;
    function DataLoaded: Boolean;
    procedure EnableMovingAverage(ASeries: TChartSeries; AEnable: Boolean);
    procedure FixChartColors;
    function GetDataType: TDataType;
    function GetSeries(ADataNode: TTreeNode; ACaseType: TCaseType;
      ADataType: TDataType): TBasicPointSeries;
    procedure LessChartSymbols;
    procedure LessChartSymbols(ASeries: TcLineSeries);
    procedure MenuDataTypeClickHandler(Sender: TObject);
    procedure RestoreSeriesNodesFromList(AList: TFPList);
    procedure StoreSeriesNodesInList(AList: TFPList);
    procedure UpdateAffectedSeries;

  protected
    procedure CreateHandle; override;
    procedure DoUpdateActions; virtual;
//    function GetCellText(ACol, ARow: Integer): String; override;
    procedure UpdateAxes;
    procedure UpdateGrid;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear(UnselectTree: Boolean = true);
    procedure HighlightWeekends(AEnable: Boolean);
    function IsTimeSeries: Boolean;
    procedure SetCommonStart(AEnable: Boolean);
    procedure SetDataType(ADataType: TDataType);
    procedure SetLogarithmic(AEnable: Boolean);
    procedure SetMovingAverage(AEnable: Boolean);
    procedure SetOverlayMode(AEnable: Boolean);
    procedure ShowDateIndicatorLine(AEnable: Boolean);
    procedure ShowTimeSeries(ADataNode: TTreeNode);
    procedure UpdateAxes(LogarithmicX, LogarithmicY: Boolean);
    procedure UpdateCmdStates; override;
    procedure UpdateData;
    procedure UpdateDateIndicatorLine(Sender: TObject; ADate: TDate);
    procedure UpdateInfectiousPeriod;
    procedure UpdateMenu(AMenu: TMenuItem); override;
    procedure UpdateStatusbar(AText: String);

    procedure LoadFromIni(ini: TCustomIniFile); override;
    procedure SaveToIni(ini: TCustomIniFile); override;

    property OnUpdateActions: TUpdateTimeSeriesActions read FOnUpdateActions write FOnUpdateActions;

  end;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLType, Math, DateUtils,
  TAChartUtils, TATypes, TAMath, TACustomSource, TAFitLib,
  cUtils, cDataSource, cRobertKochInstitut;

const
  START_COUNT = 100;
  MENUBASE_DATATYPE = 200;

var
  // The BaseDate must be subtracted from the date values for fitting on the
  // log scale to prevent a floating point overflow.
  // This is not used, however, when dates have a common start.
  // DateOffset is the correct offset to be used in any case
  BaseDate: TDate;
  DateOffset: TDate;

  // Chart series colors
  COLORS: array[0..9] of TColor = (
    clRed, clBlue, clFuchsia, clLime, clSkyBlue,
    clTeal, clPurple, clBlack, clMedGray, clMoneyGreen);


{ TTimeSeriesFrame }

constructor TTimeSeriesFrame.Create(AOwner: TComponent);
begin
  inherited;

  DateOffset := BaseDate;
  FFixedColAlignment := taCenter;

  FCheckedCases := [ctConfirmed];
  CreateMeasurementSeries;
  FixChartColors;

  WheelZoomTool.ZoomFactor := 1.05;
  WheelZoomTool.ZoomRatio := 1.0 / WheelZoomTool.ZoomFactor;
  {$IF LCL_FullVersion >= 2010000}
  ZoomDragTool.LimitToExtent := [zdDown, zdLeft, zdRight, zdUp];
  PanDragTool.LimitToExtent := [pdDown, pdLeft, pdRight, pdUp];
  {$IFEND}
end;

procedure TTimeSeriesFrame.acCasesExecute(Sender: TObject);
var
  L: TFPList;
begin
  FCheckedCases := CasesChecked();
  L := TFPList.Create;
  try
    StoreSeriesNodesInList(L);
    ShowTimeSeries(DataTree.Selected);
    RestoreSeriesNodesFromlist(L);
  finally
    L.Free;
  end;
end;

procedure TTimeSeriesFrame.acClearExecute(Sender: TObject);
begin
  Clear;
end;

procedure TTimeSeriesFrame.acHighlightWeekendsExecute(Sender: TObject);
begin
  HighlightWeekends(acHighlightWeekends.Checked);
end;

procedure TTimeSeriesFrame.acLinearExecute(Sender: TObject);
begin
  SetLogarithmic(false);
end;

procedure TTimeSeriesFrame.acLogarithmicExecute(Sender: TObject);
begin
  SetLogarithmic(true);
end;

procedure TTimeSeriesFrame.acMovingAverageExecute(Sender: TObject);
begin
  SetMovingAverage(acMovingAverage.Checked);
end;

procedure TTimeSeriesFrame.acOverlayModeExecute(Sender: TObject);
begin
  SetOverlayMode(acOverlayMode.Checked);
end;

procedure TTimeSeriesFrame.cmbDataTypeChange(Sender: TObject);
begin
  SetDataType(TDataType(cmbDataType.ItemIndex));
end;

procedure TTimeSeriesFrame.CrossHairToolDraw(ASender: TDataPointDrawTool);
const
  DECS: array[boolean] of Integer = (0, 1);
var
  ser: TcLineSeries;
  data: TcDataItem;
  x, y, R: Double;
  sx, sy, sR: String;
  dt: TDataType;
  d: TDate;
  serIdx: Integer;
  dataIdx: Integer;
  lTitle: String;
  lInfo: String;
  L: TStrings;
begin
  if ASender = nil then
    exit;

  if ASender.Series is TcLineSeries then
  begin
    ser := TcLineSeries(ASender.Series);
    data := GetDataItem(ser.Node);
    dt := GetDataType();
    serIdx := ASender.PointIndex;
    if serIdx > -1 then
    begin
      x := data.Date[serIdx];
      //x := ser.GetXValue(serIdx);
      y := ser.GetYValue(serIdx);
      lTitle := GetInfoTitle(ser.Node, round(x));
      lInfo := GetInfoText(ser.Node, round(x));
      if dt in [dtCumulativeCasesDoublingTime, dtNewCasesDoublingTime] then
      begin
        L := TStringList.Create;
        L.SkipLastLineBreak := true;
        L.Text := lInfo;
        L.Delete(L.Count-1);  // Delete "R value"
        L.Add(Format('Doubling time: %.1f', [y]));
        lInfo := L.Text;
        L.Free;
      end;
      ShowInfo(lTitle, lInfo);
    end;
    ASender.Handled;
  end else
    ShowInfo('', '');
end;

// It is assumed that xmin < xmax.
function TTimeSeriesFrame.CalcFit(ASeries: TBasicChartSeries;
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
    if GreaterThan(xval, xmax) then
      break;
    if LessThan(xval, xmin) then
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

procedure TTimeSeriesFrame.CalcFitCurveHandler(const AX: Double; out AY: Double);
begin
  if not (IsNaN(FFitCoeffs[0]) or IsNaN(FFitCoeffs[1])) then
    AY := FFitCoeffs[0] * exp(FFitCoeffs[1] * (AX - DateOffset))
  else
    AY := NaN;
end;

function TTimeSeriesFrame.CasesChecked: TCaseTypes;
begin
  Result := [];
  if acInfected.Checked then Include(Result, ctConfirmed);
  if acDeaths.Checked then Include(Result, ctDeaths);
  if acRecovered.Checked then Include(Result, ctRecovered);
  if acSick.Checked then Include(Result, ctSick);
end;

procedure TTimeSeriesFrame.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
var
  ct: TCaseType;
begin
  // Do not add the fit series to the chartlistbox
  if (ASeries is TFuncSeries) then
  begin
    ASkip := true;
    exit;
  end;

  // Do not add the DateIndicatorLine when no map is displayed
  if (ASeries = FDateIndicatorLine) and ((Align = alClient) or not DataLoaded) then
  begin
    ASkip := true;
    exit;
  end;

  // Do not add empty series to the chartlistbox.
  if (not ASeries.Active) and (ASeries is TChartSeries) and (TChartSeries(ASeries).Count = 0) then
    for ct in TCaseType do
      if (pos(CASETYPE_NAMES[ct], ASeries.Title) > 0) or
         (pos(R_NUMBER_STR, ASeries.Title) > 0) then
      begin
        ASkip := true;
        exit;
      end;
end;

procedure TTimeSeriesFrame.ChartBeforeCustomDrawBackWall(ASender: TChart;
  ADrawer: IChartDrawer; const ARect: TRect; var ADoDefaultDrawing: Boolean);
const
  SATURDAY = 7;
var
  ext: TDoubleRect;
  x: Double;
begin
  if (not TimeSeriesSettings.HighlightWeekends) or (not IsTimeSeries) then
    exit;

  ext := ASender.LogicalExtent;
  if (ext.a.x = -1) and (ext.b.x = +1) then
    exit;

  ADrawer.SetPenColor(ASender.Frame.Color);
  ADrawer.BrushColor := ASender.BackColor;
  ADrawer.Rectangle(ARect);

  x := trunc(ext.a.x);
  while DayOfWeek(x) <> SATURDAY do
    x += 1;

  ADrawer.BrushColor := ASender.BottomAxis.Grid.Color;
  while (x <= ext.b.x) do
  begin
    ADrawer.FillRect(ASender.XGraphToImage(x){%H-}, ARect.Top+1, ASender.XGraphToImage(x+1){%H-}, ARect.Bottom-1);
    x += 7;
  end;

  ADoDefaultDrawing := false;
end;

procedure TTimeSeriesFrame.ChartListboxClick(Sender: TObject);
begin
  UpdateAffectedSeries;
end;

procedure TTimeSeriesFrame.ChartResize(Sender: TObject);
begin
  inherited;
  LessChartSymbols;
end;

procedure TTimeSeriesFrame.CheckCases(ACaseTypes: TCaseTypes);
begin
  FCheckedCases := ACaseTypes;
  acInfected.Checked := ctConfirmed in FCheckedCases;
  acDeaths.Checked := ctDeaths in FCheckedCases;
  acRecovered.Checked := ctRecovered in FCheckedCases;
  acSick.Checked := ctSick in FCheckedCases;
end;

procedure TTimeSeriesFrame.CheckedCasesChange(Sender: TObject);
var
  L: TFPList;
begin
  L := TFPList.Create;
  try
    StoreSeriesNodesInList(L);
    ShowTimeSeries(DataTree.Selected);
    RestoreSeriesNodesFromlist(L);
  finally
    L.Free;
  end;
end;

procedure TTimeSeriesFrame.Clear(UnselectTree: Boolean = true);
begin
  if UnselectTree then
    DataTree.Selected := nil;

  ClearAllSeries;
  CreateMeasurementSeries;

  UpdateGrid;
  DoUpdateActions;
end;

procedure TTimeSeriesFrame.ClearAllSeries;
begin
  // Be careful here: DateIndicatorLine is destroyed in Chart.ClearSeries but it
  // is still accessed by the ChartListBox. Better to prevent TChartListbox from
  // updating itself during the clearing steps.
  ChartListbox.Chart := nil;
  try
    // Another safety measure: make sure that DateIndicatorLine is nil while
    // Chart.ClearSeries is executing
    FreeAndNil(FDateIndicatorLine);
    Chart.ClearSeries;
  finally
    ChartListBox.Chart := Chart;
  end;
end;

procedure TTimeSeriesFrame.CreateHandle;
var
  w: Integer;
  s: String;
begin
  inherited;
  w := 0;
  for s in cmbDataType.Items do
    w := Max(w, cmbDataType.Canvas.TextWidth(s));
  cmbDataType.Width := w + GetSystemMetrics(SM_CXVSCROLL);
end;

procedure TTimeSeriesFrame.CreateMeasurementSeries;
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

  FDateIndicatorLine := TConstantLine.Create(Chart);
  FDateIndicatorLine.Active := false;
  FDateIndicatorLine.LineStyle := lsVertical;
  FDateIndicatorLine.Pen.Width := 2;
  FDateIndicatorLine.Pen.Color := clFuchsia;
  FDateIndicatorLine.Title := 'Date indicator';
  Chart.AddSeries(FDateIndicatorLine);
end;

function TTimeSeriesFrame.DataLoaded: Boolean;
var
  i: Integer;
begin
  for i := 0 to Chart.SeriesCount-1 do
    if (Chart.Series[i] is TcLineSeries) and (TcLineSeries(Chart.Series[i]).Count > 0) then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

procedure TTimeSeriesFrame.DoUpdateActions;
begin
  UpdateCmdStates;
  if Assigned(FOnUpdateActions) then FOnUpdateActions(self);
end;

procedure TTimeSeriesFrame.EnableMovingAverage(ASeries: TChartSeries;
  AEnable: Boolean);
begin
  if ASeries is TcLineSeries then
    TcLineSeries(ASeries).MovingAverage := AEnable;
end;

procedure TTimeSeriesFrame.FixChartColors;
var
  clr: TColor;
begin
  clr := ColorToRGB(Chart.BackColor);
  if Red(clr) + Green(clr) + Blue(clr) > 3*128 then
    Chart.LeftAxis.Grid.Color := BrighterColor(clr, -0.15)
  else
    Chart.LeftAxis.Grid.Color := BrighterColor(clr, +0.15);
  Chart.BottomAxis.Grid.Color := Chart.LeftAxis.Grid.Color;
end;

function TTimeSeriesFrame.GetDataType: TDataType;
begin
  Result := TimeSeriesSettings.DataType;
end;

function TTimeSeriesFrame.GetSeries(ADataNode: TTreeNode; ACaseType: TCaseType;
  ADataType: TDataType): TBasicPointSeries;
var
  i: Integer;
  serTitle: String;
  ser: TBasicPointSeries;
  ct: TCaseType;
  clr: TColor;
begin
  serTitle := Format('%s (%s)', [GetLocation(ADataNode), CASETYPE_NAMES[ACaseType]]);

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
  serTitle := GetLocation(ADataNode);
  for ct in TCaseType do begin
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
      Node := ADataNode;
    end;

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

    if (ser is TcLineSeries) then
      LessChartSymbols(TcLineSeries(ser));
  end;  // for ct

  UpdateAffectedSeries;
end;

procedure TTimeSeriesFrame.HighlightWeekends(AEnable: Boolean);
begin
  acHighlightWeekends.Checked := AEnable;
  TimeSeriesSettings.HighlightWeekends := AEnable;
  Chart.Invalidate;
end;

function TTimeSeriesFrame.IsTimeSeries: Boolean;
begin
  Result := GetDataType() <> dtCumVsNewCases;
end;

procedure TTimeSeriesFrame.LessChartSymbols;
var
  i: Integer;
begin
  for i := 0 to Chart.SeriesCount-1 do
  begin
    if (Chart.Series[i] is TcLineSeries) then
      LessChartSymbols(TcLineSeries(Chart.Series[i]));
  end;
end;

procedure TTimeSeriesFrame.LessChartSymbols(ASeries: TcLineSeries);
var
  data: TcDataItem;
  dx, interval: Integer;
begin
  ASeries.SymbolInterval := 0;
  if ASeries.Count = 0 then
    exit;

  data := GetDataItem(ASeries.Node);

  // Cannot use Chart.XGraphToImage at this early stage. So we must calculate
  // the length of an x axis unit (1 day) in image units manually (approximately):
  dx := round(Chart.Width / ASeries.Count);
  if dx <> 0 then
  begin
    interval := round(DataSymbolDistance / dx);
    ASeries.FirstSymbolIndex := Random(interval);
    ASeries.SymbolInterval := interval;
  end;
end;

procedure TTimeSeriesFrame.LoadFromIni(ini: TCustomIniFile);
begin
  PageControl.ActivePageIndex := ini.ReadInteger('TimeSeries', 'PageControl', PageControl.ActivePageIndex);
  ChartListbox.Width := ini.ReadInteger('TimeSeries', 'ListboxWidth', ChartListbox.Width);

  SetDataType(TDataType(ini.ReadInteger('TimeSeries', 'DataType', ord(TimeSeriesSettings.DataType))));
  CheckCases(TCaseTypes(ini.ReadInteger('TimeSeries', 'CaseTypes', integer(CasesChecked))));;
  SetOverlayMode(ini.ReadBool('TimeSeries', 'OverlayMode', TimeSeriesSettings.OverlayMode));
  SetLogarithmic(ini.ReadBool('TimeSeries', 'Logarithmic', TimeSeriesSettings.Logarithmic));
  SetMovingAverage(ini.ReadBool('TimeSeries', 'MovingAverage', TimeSeriesSettings.MovingAverage));
  HighlightWeekends(ini.ReadBool('TimeSeries', 'HighlightWeekends', TimeSeriesSettings.HighlightWeekends));

  // Update tool button hints
  PageControlChange(nil);
end;


procedure TTimeSeriesFrame.MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
begin
  FMeasurementSeries.Active := false;
  UpdateStatusbar('');
end;

procedure TTimeSeriesFrame.MeasurementToolGetDistanceText(
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
      s := Format('Daily growth: %.0f %% / Doubles every %.1f days --> %.0n cases in 1 week --> %.0n cases in 2 weeks / Reproduction number R: %.1f', [
        (exp(FFitCoeffs[1]) - 1)*100,
        -ln(0.5) / FFitCoeffs[1],
        exp(FFitCoeffs[1] * 7) * ser.YValue[ser.Count-1],
        exp(FFitCoeffs[1] * 14) * ser.YValue[ser.Count-1],
        FFitCoeffs[1] * InfectiousPeriod + 1
      ]);
      ok := true;
    end;
  FMeasurementSeries.Active := ok;
  UpdateStatusbar(s);
end;

procedure TTimeSeriesFrame.MeasurementToolMeasure(ASender: TDataPointDistanceTool);
begin
  FMeasurementSeries.Active := false;
end;

procedure TTimeSeriesFrame.MenuDataTypeClickHandler(Sender: TObject);
begin
  SetDataType(TDataType(TMenuItem(Sender).Tag - MENUBASE_DATATYPE));
end;

procedure TTimeSeriesFrame.SaveToIni(ini: TCustomIniFile);
begin
  ini.WriteInteger('TimeSeries', 'DataType', ord(TimeSeriesSettings.DataType));
  ini.WriteInteger('TimeSeries', 'CaseTypes', integer(CasesChecked));
  ini.WriteBool('TimeSeries', 'OverlayMode', TimeSeriesSettings.OverlayMode);
  ini.WriteBool('TimeSeries', 'Logarithmic', TimeSeriesSettings.Logarithmic);
  ini.WriteBool('TimeSeries', 'MovingAverage', TimeSeriesSettings.MovingAverage);
  ini.WriteBool('TimeSeries', 'HighlightWeekends', TimeSeriesSettings.HighlightWeekends);

  ini.WriteInteger('TimeSeries', 'PageControl', PageControl.ActivePageIndex);
  ini.WriteInteger('TimeSeries', 'ListboxWidth', ChartListbox.Width);
end;

procedure TTimeSeriesFrame.SetCommonStart(AEnable: Boolean);
var
  logX, logY: Boolean;
  i: Integer;
begin
  TimeSeriesSettings.CommonStart := AEnable;

  if TimeSeriesSettings.CommonStart then
  begin
    DateOffset := 0;
  end else
  begin
    DateOffset := BaseDate;
  end;

  logX := (GetDataType = dtCumVsNewCases) and TimeSeriesSettings.Logarithmic;
  logY := TimeSeriesSettings.Logarithmic;
  UpdateAxes(logX, logY);

  for i := 0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TcLineSeries then
      ShowTimeSeries(TcLineSeries(Chart.Series[i]).Node);
end;

procedure TTimeSeriesFrame.SetDataType(ADataType: TDataType);
var
  L: TFPList;
  i: Integer;
begin
  TimeSeriesSettings.DataType := ADataType;
  cmbDataType.ItemIndex := ord(ADataType);

  for i := 0 to FMenu.Count-1 do
    FMenu.Items[i].Checked := (FMenu.Items[i].Tag - MENUBASE_DATATYPE = ord(ADataType));

  // Store currently available series in a list
  L := TFPList.Create;
  try
    StoreSeriesNodesInList(L);
    Clear;
    case TimeseriesSettings.DataType of
      dtCumulative, dtNormalizedCumulative,
      dtNewCases, dtNormalizedNewCases:
        begin
          case TimeSeriesSettings.DataType of
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
          UpdateAxes(false, TimeSeriesSettings.Logarithmic);
          MeasurementTool.Enabled := true;
          { <-------- FIX ME
          clbCases.Enabled := true;
          }
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
          { <---------- FIX ME
          clbCases.Enabled := true;
          }
        end;
      dtCumVsNewCases:
        begin
          lblTableHdr.Caption := 'New cases vs. cumulative cases';
          lblTableHint.Caption := '';
          Chart.LeftAxis.Title.Caption := 'New cases';
          Chart.BottomAxis.title.Caption := 'Cumulative cases';
          MeasurementTool.Enabled := false;
          { <--------- FIX ME
          acChartLogarithmic.Checked := true;
          UpdateAxes(true, true);
          clbCases.Enabled := true;
          }
        end;
      dtRValue:
        begin
          Chart.LeftAxis.Title.Caption := 'Reproduction number';
          Chart.BottomAxis.Title.Caption := 'Date';
          lblTableHdr.Caption := 'Reproduction number';
          lblTableHint.Caption := 'Calculated as ratio of new case count at a date and ' + IntToStr(InfectiousPeriod) + ' days earlier.';
          UpdateAxes(false, false);
          MeasurementTool.Enabled := false;
          { <----------- FIX ME
          clbCases.Enabled := false;
          }
        end;
      else
        raise Exception.Create('Data type unsupported.');
    end;

    SetMovingAverage(TimeSeriesSettings.MovingAverage);
    RestoreSeriesNodesFromlist(L);
    DoUpdateActions;
    WordwrapChart(Chart);

  finally
    L.Free;
  end;

end;

procedure TTimeSeriesFrame.SetLogarithmic(AEnable: Boolean);
begin
  acLinear.Checked := not AEnable;
  acLogarithmic.Checked := AEnable;
  TimeSeriesSettings.Logarithmic := AEnable;
  if AEnable then
    UpdateAxes(not IsTimeSeries(), true)
  else
    UpdateAxes(false, false);
end;

procedure TTimeSeriesFrame.SetMovingAverage(AEnable: Boolean);
var
  movingAvg: Boolean;
  i: Integer;
begin
  TimeSeriesSettings.MovingAverage := AEnable;
  acMovingAverage.Checked := AEnable;
  acMovingAverage.Hint := Format('Moving average (%d days)', [SmoothingRange]);

  movingAvg := AEnable and
    ( GetDataType in [dtCumulative, dtNormalizedCumulative, dtNormalizedNewCases, dtNewCases] );

  for i:=0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TChartSeries then
      EnableMovingAverage(TChartSeries(Chart.Series[i]), movingAvg);

  UpdateGrid;
end;

procedure TTimeSeriesFrame.SetOverlayMode(AEnable: Boolean);
begin
  TimeSeriesSettings.OverlayMode := AEnable;
  acOverlayMode.Checked := AEnable;
end;

procedure TTimeSeriesFrame.ShowDateIndicatorLine(AEnable: Boolean);
begin
  FDateIndicatorLine.Position := FCurrentDate;
  FDateIndicatorLine.Active := AEnable;
end;

procedure TTimeSeriesFrame.ShowTimeSeries(ADataNode: TTreeNode);
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
  firstDate: TDateTime;
  R, dR: Double;
  ext: TDoubleRect;
begin
  if ADataNode = nil then
    exit;

  Screen.Cursor := crHourglass;
  try
    if not TimeSeriesSettings.OverlayMode then
      Clear(false);

    {$IFDEF RKI}
    // The RKI datamodule loads the data individually for each node
    // The JHU datamodule, on the other hand, already has all data ready.
    if TRobertKochDataSource.IsRKINode(ADataNode) then
    begin
      dataSrc := TRobertKochDatasource.Create(DataDir);
      try
        dataSrc.LoadData(DataTree, ADataNode);
      finally
        dataSrc.Free;
      end;
    end;
    {$ENDIF}

    dt := GetDataType();
    data := GetDataItem(ADataNode);
    firstDate := data.FirstDate;

    acRecovered.Enabled := data.Count[pctRecovered] > 0;
    acSick.Enabled := acRecovered.Enabled;

    for caseType in TCaseType do
    begin
      if not (caseType in FCheckedCases) then
        Continue;

      // Ignore RKI data which do not contain recovered case values.
      if (caseType in [ctRecovered, ctSick]) and (data.Count[pctRecovered] = 0) then
        Continue;

      ser := GetSeries(ADataNode, caseType, dt);
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
                  ser.AddXY(values[i], valuesNew[i], DateToStr(firstDate+i, cFormatSettings));
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
                  ser.AddXY(firstDate + i, R, [dR])
                else
                  ser.AddXY(firstDate + i, NaN);
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
              ser.AddXY(firstDate + i, values[i]);
          finally
            ser.EndUpdate;
            if not (dt in [dtCumulativeCasesDoublingTime, dtNewCasesDoublingTime]) then
              EnableMovingAverage(ser, TimeSeriesSettings.MovingAverage);
          end;
      end;
    end;

    UpdateAffectedSeries;
    UpdateAxes;
    UpdateGrid;
    DoUpdateActions;

    // Update DateIndicatorLine position
    if IsTimeSeries then
    begin
      ext := Chart.GetFullExtent;
      if (FCurrentDate < ext.a.x) or (FCurrentDate > ext.b.x) then
      FCurrentDate := ext.b.x;
      FDateIndicatorLine.Position := FCurrentDate;
    end;

    LessChartSymbols;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TTimeSeriesFrame.RestoreSeriesNodesFromList(AList: TFPList);
var
  i: Integer;
begin
  for i := 0 to AList.Count-1 do
    ShowTimeSeries(TTreeNode(AList[i]));
end;

// Store nodes belonging to currently available series in a list
// Will be restored by calling RestoreSeriesNodesFromList.
procedure TTimeSeriesFrame.StoreSeriesNodesInList(AList: TFPList);
var
  i: Integer;
begin
  for i:=0 to ChartListbox.SeriesCount-1 do
    if ChartListbox.Series[i] is TcLineSeries then
      AList.Add(TcLineSeries(ChartListbox.Series[i]).Node);
end;

procedure TTimeSeriesFrame.UpdateAffectedSeries;
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

procedure TTimeSeriesFrame.UpdateAxes;
begin
  UpdateAxes(TimeSeriesSettings.Logarithmic and not IsTimeSeries, TimeSeriesSettings.Logarithmic);
end;

procedure TTimeSeriesFrame.UpdateAxes(LogarithmicX, LogarithmicY: Boolean);
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
    end else
    if IsTimeSeries() then
    begin
      Transformations := nil;
      if TimeSeriesSettings.CommonStart then
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

procedure TTimeSeriesFrame.UpdateCmdStates;
begin
  if PageControl.ActivePage = pgTable then
    acSaveToFile.Enabled := Grid.RowCount > 2
  else
    acSaveToFile.Enabled := Chart.SeriesCount > 2;  // FitSeries and DateIndicator are always present.
  acCopyToClipboard.Enabled := acSaveToFile.Enabled;

  acMovingAverage.Enabled := (Chart.SeriesCount > 2);

  acHighlightWeekends.Enabled := Chart.SeriesCount > 2;
end;

// Recalculates data after changes in SmoothingPeriod or InfectiousPeriod.
procedure TTimeSeriesFrame.UpdateData;
var
  L: TFPList;
begin
  L := TFPList.Create;
  try
    StoreSeriesNodesInList(L);
    RestoreSeriesNodesFromList(L);
  finally
    L.Free;
  end;
end;

procedure TTimeSeriesFrame.UpdateDateIndicatorLine(Sender: TObject; ADate: TDate);
begin
  if not DataLoaded then
    exit;

//  FDateIndicatorLine.Active := true;
  FDateIndicatorLine.Position := ADate;
  FCurrentDate := ADate;
end;

procedure TTimeSeriesFrame.UpdateGrid;
var
  n: Integer;
  i, j: Integer;
  ser: TcLineSeries;
  s: String;
  firstDate, lastDate, d, serLastDate: TDate;
  r, c: Integer;
  data: TcDataItem;
  dt: TDataType;
begin
  dt := GetDataType();
  Grid.BeginUpdate;
  try
    Grid.Columns.Clear;
    n := 0;
    lastDate := -1;
    firstDate := MaxInt;
    for i:=0 to Chart.SeriesCount-1 do
    begin
      if (Chart.Series[i] is TcLineSeries) and Chart.Series[i].Active then
      begin
        ser := TcLineSeries(Chart.Series[i]);
        if ser.Count = 0 then
          Continue;
        if dt = dtCumVsNewCases then
        begin
          inc(n, 2);
          data := GetDataItem(ser.Node);
          lastDate := Max(lastDate, data.GetLastDate);
          firstDate := Min(firstDate, data.GetFirstDate);
        end else
        begin
          inc(n);
          lastDate := Max(lastDate, ser.XValue[ser.LastValueIndex]);
          firstDate := Min(firstDate, ser.XValue[0]);
        end;
        with Grid.Columns.Add do
        begin
          s := StringReplace(ser.Title, ' ', LineEnding, []);
          if not IsTimeSeries() then
            s := s + LineEnding + 'New';
          Title.Caption := s;
          Tag := PtrInt(Chart.Series[i]);
        end;
        if dt = dtCumVsNewCases then
          with Grid.Columns.Add do
          begin
            Title.Caption := StringReplace(ser.Title, ' ', LineEnding , []) + LineEnding + 'Total';
            Tag := PtrInt(Chart.Series[i]);
          end;
      end;
    end;

    // Set number of rows
    if n = 0 then
    begin
      Grid.RowCount := 2;
      Grid.Cells[0, 1] := '';
      exit;
    end;
    Grid.RowCount := round(lastDate - firstDate + 1) + Grid.FixedRows;

    // Fill date column
    d := lastDate;
    for r := Grid.FixedRows to Grid.RowCount-1 do
    begin
      Grid.Cells[0, r] := DateToStr(d);
      d := d - 1;
    end;

    // Fill series columns
    c := Grid.FixedCols;
    for i := 0 to Chart.SeriesCount-1 do begin
      if (Chart.Series[i] is TcLineSeries) and Chart.Series[i].Active then
      begin
        ser := TcLineSeries(Chart.Series[i]);
        data := GetDataItem(ser.Node);
        serLastDate := data.GetLastDate;
        r := Grid.FixedRows + round(lastDate - serLastDate);
        for j := ser.Count-1 downto 0 do
        begin
          if not IsNan(ser.YValue[j]) then
            case dt of
              dtCumulativeCasesDoublingTime,
              dtNewCasesDoublingTime:
                Grid.Cells[c, r] := Format('%.1f', [ser.YValue[j]]);
              dtCumVsNewCases:
                begin
                  Grid.Cells[c, r] := Format('%.0n', [ser.YValue[j]]);
                  Grid.Cells[c+1, r] := Format('%.0n', [ser.XValue[j]]);
                end;
              dtRValue:
                Grid.Cells[c, r] := Format('%.2f', [ser.YValue[j]]);
              else
                Grid.Cells[c, r] := Format('%.0n', [ser.YValue[j]]);
            end
          else
            Grid.Cells[c, r] := '';
          inc(r);
        end;
        if dt = dtCumVsNewCases then
          inc(c, 2)
        else
          inc(c);
      end;
    end;

  finally
    Grid.EndUpdate;
  end;
end;

procedure TTimeSeriesFrame.UpdateInfectiousPeriod;
begin
  cmbDataType.Items[7] := Format('Reproduction number (R, % days)', [InfectiousPeriod]);
  UpdateData;
end;

procedure TTimeSeriesFrame.UpdateMenu(AMenu: TMenuItem);
var
  i: Integer;
  item: TMenuItem;
begin
  FMenu := AMenu;

  for i := 0 to cmbDataType.Items.Count - 1 do
  begin
    item := TMenuItem.Create(self);
    item.Caption := cmbDataType.Items[i];
    item.AutoCheck := true;
    item.Caption := cmbDataType.Items[i];
    item.GroupIndex := 1;
    item.Tag := i + MENUBASE_DATATYPE;
    item.OnClick := @MenuDataTypeClickHandler;
    AMenu.Add(item);
  end;

  item := TMenuItem.Create(self);
  item.caption := '-';
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acInfected;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acDeaths;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acRecovered;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acSick;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.caption := '-';
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acClear;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acOverlayMode;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acLinear;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acLogarithmic;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acHighlightWeekends;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acMovingAverage;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Caption := '-';
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acSaveToFile;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acCopyToClipboard;
  AMenu.Add(item);
end;

procedure TTimeSeriesFrame.UpdateStatusbar(AText: String);
begin
  Statusbar.SimpleText := AText;
  Statusbar.Update;
end;

initialization
  BaseDate := EncodeDate(2020, 1, 1);

end.

