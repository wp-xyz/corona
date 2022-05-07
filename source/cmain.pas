unit cMain;

{$mode objfpc}{$H+}
{$include corona.inc}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Types, LCLVersion, Menus, ActnList, StdActns,
  TAGraph, TASeries, TALegend, TACustomSeries, TATransformations,
  TATools, TAFuncSeries, TADataTools, TAChartUtils,
  cGlobal, cDataSource, cMapFrame, cTimeSeriesFrame;

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
    MenuItem4: TMenuItem;
    mnuDataUpdate: TMenuItem;
    mnuData: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuConfigHints: TMenuItem;
    mnuConfig: TMenuItem;
    mnuFileQuit: TMenuItem;
    mnuFile: TMenuItem;
    StatusBar: TStatusBar;
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
    procedure acDataUpdateExecute(Sender: TObject);
    procedure acConfigAutoLoadExecute(Sender: TObject);
    procedure acConfigAutoSaveExecute(Sender: TObject);
    procedure acConfigHintExecute(Sender: TObject);
    procedure acInfectiousPeriodExecute(Sender: TObject);
    procedure acSmoothingRangeExecute(Sender: TObject);
    procedure acTimeSeriesMovingAverageExecute(Sender: TObject);
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
  oldDisplayMode: TDisplayMode;
begin
  oldDisplayMode := FDisplayMode;
  if acDataMap.Checked then
    SetDisplayMode(dmMap)
  else if acDataTimeSeries.Checked then
    SetDisplayMode(dmTimeSeries)
  else if acDataBoth.Checked then
    SetDisplayMode(dmBoth)
  else
    SetDisplayMode(oldDisplayMode);
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
  Application.ShowHint := acConfigHint.Checked;
//  ShowHint := acConfigHint.Checked;
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
    BackupFiles;
    DownloadToCache;
  finally
    Free;
  end;

  {$IFDEF RKI}
  with TRobertKochDatasource.Create(DataDir) do
  try
    BackupFiles;
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

procedure TMainForm.LoadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  ws: TWindowState;
begin
  ini := CreateIni;
  try
    acConfigAutoLoad.Checked := ini.ReadBool('MainForm', 'AutoLoad', acConfigAutoLoad.Checked);
    if not acConfigAutoLoad.Checked then
      exit;

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

    SetDisplayMode(
      TDisplayMode(ini.ReadInteger('MainForm', 'DisplayMode', ord(FDisplayMode)))
    );
    InitDefaultFormatSettings(
      ini.ReadBool('MainForm', 'LocalizedFormatSettings', LocalizedFormatSettings)
    );

    InfectiousPeriod := ini.ReadInteger('Params', 'InfectiousPeriod', InfectiousPeriod);
    SmoothingRange := ini.ReadInteger('Params', 'SmoothingRange', SmoothingRange);

    FMapFrame.LoadFromIni(ini);
    FTimeSeriesFrame.LoadFromIni(ini);

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

    ini.WriteBool   ('MainForm', 'LocalizedFormatSettings', LocalizedFormatSettings);

    ini.WriteInteger('Params', 'InfectiousPeriod', InfectiousPeriod);
    ini.WriteInteger('Params', 'SmoothingRange', SmoothingRange);

    FMapFrame.SaveToIni(ini);
    FTimeSeriesFrame.SaveToIni(ini);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.SetDisplayMode(AMode: TDisplayMode);
begin
  FDisplayMode := AMode;

  case FDisplayMode of
    dmMap:
      begin
        acDataMap.Checked := true;
        //FTimeSeriesFrame.ShowDateIndicatorLine(false);
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
        //FTimeSeriesFrame.ShowDateIndicatorLine(false);
        mnuMap.Enabled := false;
        mnuTimeSeries.Enabled := true;
      end;
    dmBoth:
      begin
        acDataBoth.Checked := true;
        FMapFrame.Parent := DisplayPanel;
        FMapFrame.Show;
        FTimeSeriesFrame.Show;
        //FTimeSeriesFrame.ShowDateIndicatorLine(true);
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
  FTimeSeriesFrame.DisplayModeChanged(FDisplayMode);
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

