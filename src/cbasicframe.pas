unit cBasicFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics, Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, Grids,
  LCLVersion, Dialogs, Menus, ActnList, IniFiles,
  TAChartUtils, TAGraph,
  cGlobal, cDataSource, Types;

type

  TShowInfoEvent = procedure (Sender: TObject; const ATitle, AInfos: String) of object;

  { TBasicFrame }

  TBasicFrame = class(TFrame)
    acSaveToFile: TAction;
    acCopyToClipboard: TAction;
    ActionList: TActionList;
    Chart: TChart;
    Grid: TStringGrid;
    PageControl: TPageControl;
    pgChart: TTabSheet;
    pgTable: TTabSheet;
    SaveDialog: TSaveDialog;
    ToolBar: TToolBar;
    tbSaveToFile: TToolButton;
    tbCopyToClipboard: TToolButton;
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acSaveToFileExecute(Sender: TObject);
    procedure ChartResize(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      {%H-}aState: TGridDrawState);
    procedure GridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      {%H-}aState: TGridDrawState);
    procedure PageControlChange(Sender: TObject);
    procedure tbCopyToClipboardClick(Sender: TObject);
  private
    FDataTree: TTreeView;
    FOnShowInfo: TShowInfoEvent;

 {$IF LCL_FullVersion < 2010000}
  private
    FResizeTimer: TTimer;
    procedure ResizeTimerHandler(Sender: TObject);
 {$IFEND}

  protected
    FCurrentDisplayMode: TDisplayMode;
    FFixedColAlignment: TAlignment;
    procedure CreateHandle; override;
    function GetCellText({%H-}ACol, {%H-}ARow: Integer): String; virtual;
    function GetDataItem(ADataNode: TTreeNode): TcDataitem;
    function GetDataItemName(ADataNode: TTreeNode): string;
    function GetInfoText(ADataNode: TTreeNode; ADate: TDate): String; virtual;
    function GetInfoTitle(ADataNode: TTreeNode; ADate: TDate): STring; virtual;
    function GetLocation(ADataNode: TTreeNode): String;
    procedure SaveGridToFile(const AFileName: String);
    procedure ShowInfo(const ATitle, AText: String); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    procedure DisplayModeChanged(ANewDisplayMode: TDisplayMode); virtual;
    function FindNodeWithGeoID(AGeoID: TGeoID): TTreeNode;
    procedure LoadFromIni({%H-}ini: TCustomIniFile); virtual;
    procedure SaveToIni({%H-}ini: TCustomIniFile); virtual;
    procedure UpdateCmdStates; virtual;
    procedure UpdateMenu({%H-}AMenu: TMenuItem); virtual;
    procedure WordwrapChart(AChart: TChart);

    property DataTree: TTreeView read FDataTree write FDataTree;
    property OnShowInfo: TShowInfoEvent read FOnShowInfo write FOnShowInfo;

  end;

implementation

{$R *.lfm}

uses
  Math, ClipBrd,
  cUtils;

const
  CSV_SEPARATOR = #9;

constructor TBasicFrame.Create(AOwner: TComponent);
begin
  inherited;
 {$IF LCL_FullVersion >= 2010000}
  Chart.Title.WordWrap := true;
  Chart.LeftAxis.Title.WordWrap := true;
  Chart.BottomAxis.Title.WordWrap := true;
 {$ELSe}
  FResizeTimer := TTimer.Create(self);
  FResizeTimer.Enabled := false;
  FResizeTimer.Interval := 50;
  FResizeTimer.OnTimer := @ResizeTimerHandler;
 {$IFEND}
end;

procedure TBasicFrame.acCopyToClipboardExecute(Sender: TObject);
var
  stream: TStream;
  clr: TColor;
begin
  if PageControl.ActivePage = pgTable then
  begin
    stream := TMemoryStream.Create;
    try
      Grid.SaveToCSVStream(stream, CSV_SEPARATOR, true);
      stream.Position := 0;
      Clipboard.AddFormat(CF_TEXT, stream);
    finally
      stream.Free;
    end;
  end else
  begin
    clr := Chart.Color;
    Chart.Color := clWhite;
    Chart.CopyToClipboardBitmap;
    Chart.Color := clr;
  end;
end;

procedure TBasicFrame.acSaveToFileExecute(Sender: TObject);
begin
  if PageControl.ActivePage = PgTable then
  begin
    SaveDialog.Filter := 'CSV files (*.txt;*.csv;*.dat)|*.txt;*.csv;*.dat)';
    SaveDialog.DefaultExt := '*.txt';
    SaveDialog.FileName := '';
    if SaveDialog.Execute then
      SaveGridToFile(SaveDialog.FileName);
  end;
end;

procedure TBasicFrame.ChartResize(Sender: TObject);
begin
 {$IF LCL_FullVersion < 2010000}
  FResizeTimer.Enabled := true;
 {$IFEND}
end;

procedure TBasicFrame.CreateHandle;
begin
  inherited;
  if Grid.RowCount > 0 then
    Grid.RowHeights[0] := 3 * Grid.Canvas.TextHeight('Tg') + 2* varCellPadding;
end;

procedure TBasicFrame.DisplayModeChanged(ANewDisplayMode: TDisplayMode);
begin
  FCurrentDisplayMode := ANewDisplayMode;
end;

function TBasicFrame.FindNodeWithGeoID(AGeoID: TGeoID): TTreeNode;

  function DoFind(ANode: TTreeNode): TTreeNode;
  var
    data: TcDataItem;
    child: TTreeNode;
  begin
    Result := nil;
    if ANode = nil then
      exit;

    // Check node itself
    data := TcDataItem(ANode.Data);
    if data.GeoID = AGeoID then begin
      Result := ANode;
      exit;
    end;

    // Check node's children
    child := ANode.GetFirstChild;
    if child <> nil then begin
      Result := DoFind(child);
      if Result <> nil then
        exit;
    end;

    // Check node's next sibling. (It will check its own next sibling again...)
    ANode := ANode.GetNextSibling;
    if ANode <> nil then begin
      Result := DoFind(ANode);
      if Result <> nil then exit;
    end;

    Result := nil;
  end;

begin
  Result := DoFind(DataTree.Items.GetFirstNode);
end;

function TBasicFrame.GetCellText(ACol, ARow: Integer): String;
begin
  Result := '';
end;

function TBasicFrame.GetDataItem(ADataNode: TTreeNode): TcDataItem;
begin
  if ADataNode <> nil then
    Result := TcDataItem(ADataNode.Data)
  else
    Result := nil;
end;

function TBasicframe.GetDataItemName(ADataNode: TTreeNode): string;
var
  data: TcDataItem;
  p: Integer;
begin
  data := GetDataItem(ADataNode);
  if data <> nil then
  begin
    Result := data.Name;
    p := pos('|', Result);
    if p > 0 then
      SetLength(Result, p-1);
  end else
    Result := '';
end;

function TBasicFrame.GetInfoText(ADataNode: TTreeNode; ADate: TDate): String;
var
  data: TcDataItem;
  dateIdx: Integer;
  R: Double;
  sR: String;
begin
  Result := '';
  if Assigned(ADataNode) then
  begin
    data := GetDataItem(ADataNode);
    dateIdx := data.GetDateIndex(ADate);
    R := data.CalcRValue(dateIdx);
    if IsNaN(R) then sR := '-' else sR := FormatFloat('0.0', R);
    Result := Format(
      'Population: %.0n' + LineEnding +
      'Total infected: %.0n' + LineEnding +
      'Total deaths: %.0n' + LineEnding +
      'New infected: %.0n' + LineEnding +
      'New deaths: %.0n' + LineEnding +
      'Incidence: %.1f' + LineEnding +
      'R value: %s', [
      data.Population*1.0,
      data.RawData[pctConfirmed][dateIdx]*1.0,
      data.RawData[pctDeaths][dateIdx]*1.0,
      data.CalcNewCases(dateIdx, ctConfirmed)*1.0,
      data.CalcNewCases(dateIdx, ctDeaths)*1.0,
      data.CalcNormalizedNewCases(dateIdx, ctConfirmed),
      sR  // R value
    ]);
  end;
end;

function TBasicFrame.GetInfoTitle(ADataNode: TTreeNode; ADate: TDate): String;
var
  fmt: String;
begin
  Result := '';
  if Assigned(ADataNode) then
  begin
    fmt := 'ddd, ddddd';  // weekday (abbrev) + short date format
    Result := GetLocation(ADataNode) + LineEnding + FormatDateTime(fmt, ADate);
  end;
end;

function TBasicFrame.GetLocation(ADataNode: TTreeNode): String;
begin
  Result :='';
  if ADataNode = nil then
    exit;
  case ADataNode.Level of
    3: Result := Format('%s (%s)', [GetDataItemName(ADataNode), GetDataItemName(ADataNode.Parent)]);
    4: Result := Format('%s (%s), %s', [GetDataItemName(ADataNode), GetDataItemName(ADataNode.Parent), GetDataItemName(ADataNode.Parent.Parent)]);
    else Result := GetDataItemName(ADataNode);
  end;
 end;

procedure TBasicFrame.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
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

procedure TBasicFrame.GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
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
    ts.Alignment := FFixedColAlignment
  else
    ts.Alignment := taRightJustify;
  Grid.Canvas.TextStyle := ts;
end;

procedure TBasicFrame.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = pgTable then
  begin
    tbSaveToFile.Hint := 'Save table to CSV file';
    tbCopyToClipboard.Hint := 'Copy table to clipboard';
  end else
  begin
    tbSaveToFile.Hint := 'Save chart image to file';
    tbCopyToClipboard.Hint := 'Copy chart to clipboard';
  end;
  UpdateCmdStates;
end;

procedure TBasicFrame.tbCopyToClipboardClick(Sender: TObject);
var
  stream: TStream;
  clr: TColor;
begin
  if PageControl.ActivePage = pgTable then
  begin
    stream := TMemoryStream.Create;
    try
      Grid.SaveToCSVStream(stream, CSV_SEPARATOR, true);
      stream.Position := 0;
      Clipboard.AddFormat(CF_TEXT, stream);
    finally
      stream.Free;
    end;
  end else
  begin
    clr := Chart.Color;
    Chart.Color := clWhite;
    Chart.CopyToClipboardBitmap;
    Chart.Color := clr;
  end;
end;

procedure TBasicFrame.LoadFromIni(ini: TCustomIniFile);
begin
  // to be overridden by descendants
end;

{$IF LCL_FullVersion < 2010000}
procedure TBasicFrame.ResizeTimerHandler(Sender: TObject);
begin
  FResizeTimer.Enabled := false;
  WordwrapChart(Chart);
end;
{$IFEND}

procedure TBasicFrame.SaveGridToFile(const AFileName: String);
begin
  Grid.SaveToCSVFile(AfileName, CSV_SEPARATOR, true);
end;

procedure TBasicFrame.SaveToIni(ini: TCustomIniFile);
begin
  // to be overridden by descendants
end;

procedure TBasicFrame.ShowInfo(const ATitle, AText: String);
begin
  if Assigned(FOnShowInfo) then
    FOnShowInfo(Self, ATitle, AText);
end;

procedure TBasicFrame.UpdateCmdStates;
begin
end;

// Insert the actions of the frame as items into the specified menu
procedure TBasicFrame.UpdateMenu(AMenu: TMenuItem);
begin
end;

procedure TBasicFrame.WordwrapChart(AChart: TChart);
{$IF LCL_FullVersion >= 2010000}
// In Laz 2.1+, wordwrapping of titles is provided by TAChart itself.
begin
  AChart.LeftAxis.Title.WordWrap := true;
  AChart.Title.Wordwrap := true;
end;
{$ELSE}
function EqualExtent(X, Y: TDoubleRect): Boolean;
begin
  Result := (X.a.x = Y.a.x) and (X.a.y = Y.a.y) and
            (X.b.x = Y.b.x) and (X.b.y = Y.b.y);
end;

var
  s: String;
  ext: TDoubleRect;
  maxLen: Integer;
begin
  ext := AChart.LogicalExtent;
  if EqualExtent(ext, EmptyExtent) then
    maxLen := AChart.Height * 2 div 3
  else
    maxLen := AChart.ClipRect.Height;
  s := StripLineEndings(AChart.LeftAxis.Title.Caption);
  AChart.LeftAxis.Title.Caption := Wordwrap(s, AChart.LeftAxis.Title.LabelFont, maxLen);

  maxLen := AChart.ClientWidth;
  s := StripLineEndings(AChart.Title.Text.Text);
  AChart.Title.Text.Text := WordWrap(s, AChart.Title.Font, maxLen);
end;
{$IFEND}

end.

