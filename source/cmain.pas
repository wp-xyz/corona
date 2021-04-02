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


  { TMainForm }

  TMainForm = class(TForm)
    acDataUpdate: TAction;
    acAbout: TAction;
    acConfigHint: TAction;
    acConfigAutoLoad: TAction;
    acDataCommonStart: TAction;
    acInfectiousPeriod: TAction;
    acSmoothingRange: TAction;
    acDataMap: TAction;
    acDataTimeSeries: TAction;
    acConfigAutoSave: TAction;
    acDataBoth: TAction;
    ActionList: TActionList;
    BottomAxisLogTransform: TLogarithmAxisTransform;
    DateIndicatorLine: TConstantLine;
    InfoLabel: TLabel;
    InfoPanel: TPanel;
    mnuDataBoth: TMenuItem;
    MenuItem12: TMenuItem;
    mnuDataTimeSeries: TMenuItem;
    mnuDataMap: TMenuItem;
    mnuMap: TMenuItem;
    MenuItem15: TMenuItem;
    DisplayPanel: TPanel;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mnuTimeSeries: TMenuItem;
    NameLabel: TLabel;
    MapToolsetDataPointClickTool: TDataPointClickTool;
    MapToolsetDataPointHintTool: TDataPointHintTool;
    MapToolsetPanDragTool: TPanDragTool;
    MapToolsetZoomDragTool: TZoomDragTool;
    acFileExit: TFileExit;
    MainMenu: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuCommonStart: TMenuItem;
    MenuItem4: TMenuItem;
    mnuDataUpdate: TMenuItem;
    mnuData: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuConfigHints: TMenuItem;
    mnuConfig: TMenuItem;
    mnuFileQuit: TMenuItem;
    mnuFile: TMenuItem;
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
    PanDragTool: TPanDragTool;
    SaveDialog: TSaveDialog;
    ZoomDragTool: TZoomDragTool;
    MeasurementTool: TDataPointDistanceTool;
    UserdefinedTool: TUserDefinedTool;
    CrossHairTool: TDataPointCrosshairTool;
    LeftAxisLogTransform: TLogarithmAxisTransform;
    LeftPanel: TPanel;
    TreeSplitter: TSplitter;
    TreeView: TTreeView;
    procedure acAboutExecute(Sender: TObject);
    procedure acDataMapExecute(Sender: TObject);
    procedure acConfigAutoLoadExecute(Sender: TObject);
    procedure acConfigAutoSaveExecute(Sender: TObject);
    procedure acConfigHintExecute(Sender: TObject);
    procedure acDataCommonStartExecute(Sender: TObject);
    procedure acTimeSeriesMovingAverageExecute(Sender: TObject);
    procedure acDataUpdateExecute(Sender: TObject);
    procedure acInfectiousPeriodExecute(Sender: TObject);
    procedure acSmoothingRangeExecute(Sender: TObject);

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

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

    function GetDataItem(ANode: TTreeNode): TcDataItem;
    function GetDataType: TDataType;
    function GetLocation(ANode: TTreeNode): String;
    procedure GetLocation(ANode: TTreeNode; out ACountry, AState, ACity: String; out APopulation: Int64);
    function GetMapDataType: TMapDataType;
    procedure InitShortCuts;
    procedure LoadLocations;
    procedure ReadCommandlineParams;
    procedure SelectNode(ANode: TTreeNode);
    procedure ShowVersionInfo;
    procedure UpdateActionStates;
    procedure UpdateData;
    procedure UpdateInfectiousPeriod;
    procedure UpdateStatusBar(ASeparator: String = ' ');
    procedure UpdateTimeSeriesActions(Sender: TObject);

    procedure SetDisplayMode(AMode: TDisplayMode);
    procedure ShowInfoHandler(Sender: TObject; const ATitle, AInfos: String);

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
  LCLIntf, IniFiles, DateUtils, LCLPlatformDef,
  // project-specific units
  cJohnsHopkinsUniversity, {$IFDEF RKI}cRobertKochInstitut,{$ENDIF}
  cUtils, cAbout;


// Do not move this to cUtils to keep it free from GUI stuff.
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

procedure TMainForm.acDataMapExecute(Sender: TObject);
var
  dm: TDisplayMode;
begin
  if acDataMap.Checked then
    SetDisplayMode(dmMap)
  else if acDataTimeSeries.Checked then
    SetDisplayMode(dmTimeSeries)
  else if acDataBoth.Checked then
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

procedure TMainForm.acDataCommonStartExecute(Sender: TObject);
var
  logX, logY: Boolean;
  i: Integer;
begin
  TimeSeriesSettings.CommonStart := acDataCommonStart.Checked;
  if Assigned(FTimeSeriesFrame) then
    FTimeSeriesFrame.SetCommonStart(TimeSeriesSettings.CommonStart);
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
        FTimeSeriesFrame.UpdateMovingAverage;
        FTimeSeriesFrame.UpdateData;
      end else
        MessageDlg('Only odd number of days allowed (i.e. center day is included).', mtError, [mbOk], 0);
    end else
      MessageDlg('No valid number.', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.acTimeSeriesMovingAverageExecute(Sender: TObject);
begin
  if Assigned(FTimeSeriesFrame) then
    FTimeSeriesFrame.acMovingAverageExecute(nil);
end;

procedure TMainForm.BeforeRun;
begin
  ReadCommandlineParams;
  LoadLocations;
  LoadIni;
  ShowVersionInfo;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    SaveIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create and insert the map frame
  FMapFrame := TMapFrame.Create(self);
  FMapFrame.DataTree := TreeView;
  FMapFrame.Align := alClient;
  FMapFrame.OnShowInfo := @ShowInfoHandler;
  FMapFrame.Hide;
  FMapFrame.Parent := DisplayPanel;
  FMapFrame.UpdateMenu(mnuMap);

  // Create and insert the time-series frame
  FTimeSeriesFrame := TTimeSeriesFrame.Create(self);
  FTimeSeriesFrame.Height := DisplayPanel.Height div 2;
  FTimeSeriesFrame.Align := alBottom;
  FTimeSeriesFrame.DataTree := TreeView;
  FTimeSeriesFrame.OnShowInfo := @ShowInfoHandler;
  FTimeSeriesFrame.OnUpdateActions := @UpdateTimeSeriesActions;
  FTimeSeriesFrame.Hide;
  FTimeSeriesFrame.Parent := DisplayPanel;
  FTimeSeriesFrame.UpdateMenu(mnuTimeSeries);

  // Create and insert the splitter between the map and time-series frames
  FDisplaySplitter := TSplitter.Create(self);
  FDisplaySplitter.Align := alBottom;
  FDisplaySplitter.Parent := DisplayPanel;
  FDisplaySplitter.ResizeStyle := rsPattern;
  FDisplaySplitterPos := FTimeSeriesFrame.Height;

  // Show both map and time-series by default.
  SetDisplayMode(dmBoth);

  // Narrower input box in unit Dialogs.
  cInputQueryEditSizePercents := 0;

  // Define the directory for the downloaded data files
  DataDir := GetDataDir; // ends with a path delimiter

  // Fix the menu short-cuts to be cross-platform
  InitShortCuts;

  // Initialize the info box below the tree
  ShowInfoHandler(nil, '', '');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Move the About toolbutton to the right of the toolbar.
  tbAbout.Align := alRight;
  tbAbout.Left := Toolbar.Width - tbAbout.Width;
end;

function TMainForm.GetDataItem(ANode: TTreeNode): TcDataItem;
begin
  Result := TcDataItem(ANode.Data);
end;

function TMainForm.GetDataType: TDataType;
begin
  Result := TimeSeriesSettings.DataType;
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
end;

procedure TMainForm.InitShortCuts;
begin
 {$IFDEF LINUX}
  acFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
 {$ENDIF}
 {$IFDEF WINDOWS}
  acFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
 {$ENDIF}
  {TODO : Implement the equivalent for MacOS }
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
    FMapFrame.Clear;
    FTimeSeriesFrame.Clear;

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
    if ini.ReadBool('MainForm', 'ShowMap', acDataMap.Checked) then n := n or 1;
    if ini.ReadBool('MainForm', 'ShowTimeSeries', acDataTimeSeries.Checked) then n := n or 2;
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

    ini.WriteBool   ('MainForm', 'ShowHints', acConfigHint.Checked);
    ini.WriteInteger('MainForm', 'LeftPanel', LeftPanel.Width);
    ini.WriteInteger('MainForm', 'DisplayMode', ord(FDisplayMode));
    ini.WriteInteger('MainForm', 'DisplaySplitterPos', FDisplaySplitterPos);

    FMapFrame.SaveToIni(ini);
    FTimeSeriesFrame.SaveToIni(ini);
                         (*
    if acDataMap.Checked then
      ini.WriteInteger('MainForm', 'PaletteListbox', PaletteListboxPanel.Width);
    if acDataTimeSeries.Checked then
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

    ini.WriteBool('MainForm', 'ShowMap', acDataMap.Checked);
    ini.WriteBool('MainForm', 'ShowTimeSeries', acDataTimeSeries.Checked);
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
        acDataMap.Checked := true;
        FTimeSeriesFrame.Hide;
        FDisplaySplitter.Hide;
        FMapFrame.Align := alClient;
        FMapFrame.Show;
        FMapFrame.OnDateSelect := nil;
        mnuMap.Enabled := true;
        mnuTimeSeries.Enabled := false;
      end;
    dmTimeSeries:
      begin
        acDataTimeSeries.Checked := true;
        FMapFrame.Hide;
        FDisplaySplitter.Hide;
        FTimeSeriesFrame.Show;
        FTimeSeriesFrame.Align := alClient;
        FTimeSeriesFrame.ShowDateIndicatorLine(false);
        mnuMap.Enabled := false;
        mnuTimeSeries.Enabled := true;
      end;
    dmBoth:
      begin
        acDataBoth.Checked := true;
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
        mnuMap.Enabled := true;
        mnuTimeSeries.Enabled := true;
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

procedure TMainForm.ToolBarResize(Sender: TObject);
begin
  tbAbout.Left := Toolbar.Width - tbAbout.Width;
end;

procedure TMainForm.UpdateTimeSeriesActions(Sender: TObject);
begin
 // acTimeSeriesMovingAvg.Checked := TimeSeriesSettings.MovingAverage;
end;


end.

