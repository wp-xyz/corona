unit cBasicFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Graphics, Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, Grids,
  LCLVersion, IniFiles,
  TAChartUtils, TAGraph,
  cGlobal, cDataSource, Types;

type

  TShowInfoEvent = procedure (Sender: TObject; const ATitle, AInfos: String) of object;

  { TBasicFrame }

  TBasicFrame = class(TFrame)
    Chart: TChart;
    Grid: TDrawGrid;
    PageControl: TPageControl;
    pgChart: TTabSheet;
    pgTable: TTabSheet;
    ToolBar: TToolBar;
    procedure ChartResize(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    FDataTree: TTreeView;
    FOnShowInfo: TShowInfoEvent;

 {$IF LCL_FullVersion < 2010000}
  private
    FResizeTimer: TTimer;
    procedure ResizeTimerHandler(Sender: TObject);
 {$IFEND}

  protected
    function GetCellText(ACol, ARow: Integer): String; virtual;
    function GetDataItem(ADataNode: TTreeNode): TcDataitem;
    function GetInfoText(ADataNode: TTreeNode; ADate: TDate): String; virtual;
    function GetInfoTitle(ADataNode: TTreeNode; ADate: TDate): STring; virtual;
    function GetLocation(ADataNode: TTreeNode): String;
    procedure ShowInfo(const ATitle, AText: String); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    function FindNodeWithGeoID(AGeoID: TGeoID): TTreeNode;
    procedure LoadFromIni(ini: TCustomIniFile); virtual;
    procedure SaveToIni(ini: TCustomIniFile); virtual;
    procedure WordwrapChart(AChart: TChart);

    property DataTree: TTreeView read FDataTree write FDataTree;
    property OnShowInfo: TShowInfoEvent read FOnShowInfo write FOnShowInfo;

  end;

implementation

{$R *.lfm}

uses
  Math,
  cUtils;

constructor TBasicFrame.Create(AOwner: TComponent);
begin
  inherited;

 {$IF LCL_FullVersion < 2010000}
  FResizeTimer := TTimer.Create(self);
  FResizeTimer.Enabled := false;
  FResizeTimer.Interval := 50;
  FResizeTimer.OnTimer := @ResizeTimerHandler;
 {$IFEND}
end;

procedure TBasicFrame.ChartResize(Sender: TObject);
begin
 {$IF LCL_FullVersion < 2010000}
  FResizeTimer.Enabled := true;
 {$IFEND}
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
  Result := TcDataItem(ADataNode.Data);
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
      'Total infected: %.0n' + LineEnding +
      'Total deaths: %.0n' + LineEnding +
      'New infected: %.0n' + LineEnding +
      'New deaths: %.0n' + LineEnding +
      'Incidence: %.1f' + LineEnding +
      'R value: %s', [
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
    3: Result := Format('%s (%s)', [GetDataItem(ADataNode).Name, GetDataItem(ADataNode.Parent).Name]);
    4: Result := Format('%s (%s), %s', [GetDataItem(ADataNode).Name, GetDataItem(ADataNode.Parent).Name, GetDataItem(ADataNode.Parent.Parent).Name]);
    else Result := GetDataItem(ADataNode).Name;
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
    ts.Alignment := taCenter
  else
    ts.Alignment := taRightJustify;
  Grid.Canvas.TextStyle := ts;
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


procedure TBasicFrame.SaveToIni(ini: TCustomIniFile);
begin
  // to be overridden by descendants
end;

procedure TBasicFrame.ShowInfo(const ATitle, AText: String);
begin
  if Assigned(FOnShowInfo) then
    FOnShowInfo(Self, ATitle, AText);
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

