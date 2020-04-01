unit cMain;

{$mode objfpc}{$H+}
{$define RKI}

interface

uses
  LCLType,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, Grids, Types, LCLVersion, Menus, ActnList, StdActns,
  TAGraph, TAIntervalSources, TASeries, TAChartListbox, TALegend,
  TACustomSeries, TATransformations, TATools, TAFuncSeries, TADataTools,
  cGlobal;

type

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
    ActionList: TActionList;
    Chart: TChart;
    BottomAxisTransformations: TChartAxisTransformations;
    BottomAxisLogTransform: TLogarithmAxisTransform;
    ChartToolset: TChartToolset;
    acFileExit: TFileExit;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
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
    rgDataType: TRadioGroup;
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
    cgCases: TCheckGroup;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    LeftSplitter: TSplitter;
    RightSplitter: TSplitter;
    StatusBar: TStatusBar;
    TreeView: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acChartCopyToClipboardExecute(Sender: TObject);
    procedure acChartLinearExecute(Sender: TObject);
    procedure acChartLogarithmicExecute(Sender: TObject);
    procedure acChartOverlayExecute(Sender: TObject);
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
    procedure rgDataTypeClick(Sender: TObject);
    procedure ToolBarResize(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);

  private
    FMeasurementSeries: TFuncSeries;
    FFitCoeffs: array[0..1] of Double;
    FStatusText1, FStatusText2: String;
    function CalcFit(ASeries: TBasicChartSeries; xmin, xmax: Double): Boolean;
    procedure CalcFitHandler(const AX: Double; out AY: Double);
    procedure Clear(UnselectTree: Boolean = true);
    procedure CreateMeasurementSeries;
    function GetCellText(ACol, ARow: Integer): String;
//    function GetDataString(ANode: TTreeNode; ACaseType: TCaseType; var AHeader, ACounts: String): Boolean;
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState: String);
    function GetSeries(ANode: TTreeNode; ACaseType: TCaseType; ADataType: TDataType): TChartSeries;
    procedure InitShortCuts;
    function IsTimeSeries: Boolean;
    procedure LayoutBars;
    procedure LoadLocations;
    procedure ShowData(ANode: TTreeNode);
    procedure StatusbarHandler(Sender: TObject; const AMsg1, AMsg2: String);
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
  Math, IniFiles, DateUtils,
  // TAChart units
  TATypes, TAMath, TAChartUtils, TACustomSource, TAFitLib,
  // project-specific units
  cJohnsHopkinsUniversity,
  {$IFDEF RKI}cRobertKochInstitut,{$ENDIF}
  cAbout;

const
  CASETYPE_NAMES: array [TCaseType] of string = ('confirmed', 'deaths', 'recovered');

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

procedure TMainForm.acDataUpdateExecute(Sender: TObject);
begin
  UpdateData;
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
  sx, sy: String;
  dt: TDataType;
  d: TDate;
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
        begin
          sx := DateToStr(x);
          if y = 1 then sy := '1 case' else sy := Format('%.0n cases', [y]);
          FStatusText1 := Format('%s, %s', [sx, sy]);
        end;
      dtNewCases:
        begin
          sx := DateToStr(x);
          if y = 1 then sy := '1 new case' else sy := Format('%.0n new cases', [y]);
          FStatusText1 := Format('%s, %s', [sx, sy]);
        end;
      dtDoublingTime:
        begin
          sx := DateToStr(x);
          sy := Format('Doubling time %.1f', [y]);
          FStatusText1 := Format('%s, %s', [sx, sy]);
        end;
      dtCumVsNewCases:
        begin
          sx := ser.Source.Item[ASender.PointIndex]^.Text;
          d := ScanDateTime('mm"/"dd"/"yy', ser.Source.Item[ASender.PointIndex]^.Text);
          if x = 1 then sx := '1 case' else sx := Format('%.0n cases', [x]);
          if y = 1 then sy := '1 new case' else sy := Format('%.0n new cases', [y]);
          FStatusText1 := Format('%s - %s, %s', [DateToStr(d), sx, sy]);
        end;
    end;
    FStatusText1 := ser.Title + ': ' + FStatusText1;
    ASender.Handled;
  end;
  UpdateStatusBar;
end;

procedure TMainForm.cgCasesItemClick(Sender: TObject; Index: integer);
begin
  TreeViewClick(nil);
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
  Grid.RowHeights[0] := 3 * Grid.Canvas.TextHeight('Tg') + 2* varCellPadding;
  PageControl.ActivePageIndex := 0;

  CreateMeasurementSeries;
  InitShortCuts;

//  LoadIni;

  LoadLocations;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  tbAbout.Align := alRight;
  tbAbout.Left := Toolbar.Width - tbAbout.Width;
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
        case TDataType(rgDataType.ItemIndex) of
          dtDoublingTime:
            Result := Format('%.1f', [ser.YValue[r]]);
          dtCumVsNewCases:
            if odd(ACol) then
              Result := Format('%.0n', [ser.YValue[r]])
            else
              Result := Format('%.0n', [ser.XValue[r]]);
          else
            Result := Format('%.0n', [ser.YValue[r]]);
        end;
    end;
  end;
end;
  (*
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
    *)
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
      dtCumVsNewCases:
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
      dtNewCases,
      dtDoublingTime:
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
    ser.Tag := PtrUInt(ANode);
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
  Result := TDataType(rgDataType.ItemIndex) <> dtCumVsNewCases;
end;

procedure TMainForm.LayoutBars;
var
  i, j, n: Integer;
  ser: TBarSeries;
begin
  if not (TDataType(rgDataType.ItemIndex) in [dtNewCases, dtDoublingTime]) then
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
      ok := LoadLocations(TreeView);
      if not ok then
      begin
        MessageDlg('Data files not found. Please click "Update files".', mtError, [mbOk], 0);
        exit;
      end;
    finally
      Free;
    end;

    {$IFDEF RKI}
    // Robert-Koch goes here...
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
var
  L: TFPList;
  i: Integer;
begin
  L := TFPList.Create;
  try
    // Store currently available series in
    for i:=0 to ChartListbox.SeriesCount-1 do
      L.Add(Pointer(ChartListbox.Series[i].Tag));

    Clear;
    case TDataType(rgDataType.ItemIndex) of
      dtCumulative:
        begin
          Chart.LeftAxis.Title.Caption := 'Cases';
          lblTableHdr.Caption := 'Cumulative Cases';
          UpdateAxes(false, acChartLogarithmic.Checked);
          MeasurementTool.Enabled := true;
        end;
      dtNewCases:
        begin
          Chart.LeftAxis.Title.Caption := 'New Cases';
          lblTableHdr.Caption := 'New Cases';
          UpdateAxes(false, false);
          MeasurementTool.Enabled := false;
        end;
      dtDoublingTime:
        begin
          Chart.LeftAxis.Title.Caption := 'Doubling time (days)';
          lblTableHdr.Caption := 'Doubling time (days, calculated from case-ratio of two consecutive days)';
          UpdateAxes(false, false);
          MeasurementTool.Enabled := false;
        end;
      dtCumVsNewCases:
        begin
          lblTableHdr.Caption := 'New cases vs. cumulative cases';
          Chart.LeftAxis.Title.Caption := 'New cases';
          Chart.BottomAxis.title.Caption := 'Cumulative cases';
          UpdateAxes(true, true);
          MeasurementTool.Enabled := false;
        end;
      else
        raise Exception.Create('Data type unsupported.');
    end;

    for i := 0 to L.Count-1 do
      ShowData(TTreeNode(L[i]));

    UpdateActionStates;

  finally
    L.Free;
  end;
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
begin
  ShowData(TreeView.Selected);
end;

procedure TMainForm.ShowData(ANode: TTreeNode);
var
  fs: TFormatSettings;
  counts: array[TCaseType] of string = ('', '', '');
  dates: array[TCaseType] of string = ('', '', '');
  dt: TDataType;
  ct: TCaseType;
  saX, saY: TStringArray;
  i: Integer;
  ser: TChartSeries;
  X, Y, Y0: Double;
  t2: Double;  // Doubling time
  country, state: String;
begin
  if ANode = nil then
    exit;

  if not acChartOverlay.Checked then
    Clear(false);

  fs := FormatSettings;
  fs.ShortDateFormat := 'mm/dd/yyy';
  fs.DateSeparator := '/';

  dt := TDataType(rgDataType.ItemIndex);
  GetLocation(ANode, country, state);

  for ct := Low(TCaseType) to High(TCaseType) do
  begin
    if cgCases.Checked[ord(ct)] then
    begin
      with TJohnsHopkinsDatasource.Create(DataDir) do
      try
        if not GetDataString(country, state, ct, dates[ct], counts[ct]) then
        exit;
      finally
        Free;
      end;

      {$IFDEF RKI}
      // Robert-Koch goes here...: if toplevel node contains RKI...
      {$ENDIF}

      saX := dates[ct].Split(',', '"');
      saY := counts[ct].Split(',','"');
      ser := GetSeries(ANode, ct, dt);
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
          dtDoublingTime:
            begin
              Y0 := StrToInt(saY[4]);
              Y := StrToInt(saY[5]);
              for i := 6 to High(saY) do
              begin
                Y := StrToInt(saY[i]);
                if (Y > Y0) and (Y0 > 0) then
                begin
                  t2 := ln(2.0) / ln(Y / Y0);
                  ser.AddXY(StrToDate(saX[i], fs), t2);
                end else
                  ser.AddXY(StrToDate(saX[i], fs), NaN);
                Y0 := Y;
              end;
            end;
          dtCumVsNewCases:
            begin
              Y0 := StrToInt(saY[4]);
              for i := 5 to High(saY) do begin
                X := StrToInt(saY[i]);
                Y := StrToInt(saY[i]);
                if (Y > Y0) and (Y0 > 0) then
                  ser.AddXY(X, Y - Y0, saX[i]);
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
  FStatustext1 := GetLocation(ANode) + ' loaded.';
  UpdateStatusBar;
  UpdateActionStates;
end;

procedure TMainForm.StatusbarHandler(Sender: TObject; const AMsg1, AMsg2: String);
begin
  FStatusText1 := AMsg1;
  FStatusText2 := AMsg2;
  UpdateStatusbar;
end;

procedure TMainForm.UpdateActionStates;
begin
  acTableSave.Enabled := Chart.SeriesCount > 1;  // 1 series reserved for measurement
  acChartLogarithmic.Enabled := TDataType(rgDataType.ItemIndex) in [dtCumulative, dtCumVsNewCases];
  acChartLinear.Enabled := acChartLogarithmic.Enabled;
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
    end else
    if IsTimeSeries() then
    begin
      Transformations := nil;
      Marks.Source := DateTimeIntervalChartSource;
      Marks.Style := smsLabel;
      // no need to reset Intervals because they are handled by DateTimeInterval source on time series.
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
      if TDataType(rgDataType.ItemIndex) in [dtCumulative, dtCumVsNewCases] then
        Marks.Format := '%0:.0n'
      else
        Marks.Format := '%0:.9g';
    end;
  end;
  Chart.Extent.UseYMin := LogarithmicY;
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
  // Robert-Koch goes here...
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

    cgCases.Checked[0] := ini.Readbool('MainForm', 'ConfirmedCases', cgCases.Checked[0]);
    cgCases.Checked[1] := ini.ReadBool('MainForm', 'DeathCases', cgCases.Checked[1]);
    cgCases.Checked[2] := ini.ReadBool('MainForm', 'RecoveredCases', cgCases.Checked[2]);

    acChartOverlay.Checked := ini.ReadBool('MainForm', 'Overlay', acChartOverlay.Checked);

    n := ini.ReadInteger('MainForm', 'DataType', rgDataType.ItemIndex);
    if (n >= 0) and (n <= ord(High(TDataType))) then
      rgDataType.ItemIndex := n;
    isLog := ini.ReadBool('MainForm', 'Logarithmic', acChartLogarithmic.Checked);
    case TDataType(rgDataType.ItemIndex) of
      dtCumulative:
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

    ini.WriteBool('MainForm', 'ConfirmedCases', cgCases.Checked[0]);
    ini.WriteBool('MainForm', 'DeathCases', cgCases.Checked[1]);
    ini.WriteBool('MainForm', 'RecoveredCases', cgCases.Checked[2]);
    ini.WriteInteger('MainForm', 'DataType', rgDataType.ItemIndex);

    ini.WriteBool('MainForm', 'Overlay', acChartOverlay.Checked);
    if TDataType(rgDataType.ItemIndex) = dtCumulative then
      ini.WriteBool('MainForm', 'Logarithmic', acChartLogarithmic.Checked);

    ini.WriteBool('MainForm', 'ShowHints', acConfigHint.Checked);

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

