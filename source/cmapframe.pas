unit cMapFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, IniFiles,
  StdCtrls, ExtCtrls, ComCtrls, LCLVersion, Types, Menus,
  TATools,
  cBasicFrame, cGlobal, cDataSource, cPalette, cGeoMap;

type
  TDateSelectEvent = procedure(Sender: TObject; ADate: TDate) of object;

  { TMapFrame }

  TMapFrame = class(TBasicFrame)
    ChartToolset: TChartToolset;
    lblTableHdr: TLabel;
    MouseWheelZoomtool: TZoomMouseWheelTool;
    DataPointClickTool: TDataPointClickTool;
    InfoTool: TUserDefinedTool;
    PanDragTool: TPanDragTool;
    RightPanel: TPanel;
    ToolButton1: TToolButton;
    ZoomDragTool: TZoomDragTool;
    cmbDataType: TComboBox;
    MapDateLabel: TLabel;
    PaletteListbox: TColorListBox;
    Panel1: TPanel;
    MapDateScrollBar: TScrollBar;
    DatePanel: TPanel;
    Splitter: TSplitter;
    procedure cmbDataTypeChange(Sender: TObject);
    procedure DataPointClickToolPointClick(ATool: TChartTool; {%H-}APoint: TPoint);
    procedure InfoToolAfterMouseMove({%H-}ATool: TChartTool; APoint: TPoint);
    procedure MapDateScrollBarChange(Sender: TObject);
    procedure PaletteListboxGetColors(Sender: TCustomColorListBox; Items: TStrings);
  private
    FGeoMap: TcGeoMap;
    FMapLock: Integer;
    FPalette: TPalette;
    FCurrentDate: TDate;
    FMapMenu: TMenuItem;
    FOldMapResource: String;
    FOnDateSelect: TDateSelectEvent;
    function GetMapDataType: TMapDataType;
    procedure GetMapResourceParams(var ADataNode: TTreeNode; out AResName: String;
      out APlotChildNodes: Boolean);
    procedure MenuDataTypeClickHandler(Sender: TObject);
    procedure SetOnDateSelect(AValue: TDateSelectEvent);
    procedure ShowCoronaMap(ADataNode: TTreeNode; ADateIndex: Integer;
      AMapDataType: TMapDataType; PlotChildNodeData: Boolean);
    procedure ShowCoronaMapLevel(ADataNode: TTreeNode; ADateIndex: Integer;
      AMapDataType: TMapDataType);

  protected
    procedure CreateHandle; override;
    procedure DoDateSelect(ADate: TDate); virtual;
    procedure PopulatePaletteListbox(ADataType: TMapDataType); virtual;
    procedure SelectProjection(AMapRes: string); virtual;
    procedure UpdateGrid;

  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromIni(ini: TCustomIniFile); override;
    procedure SaveToIni(ini: TCustomIniFile); override;
    procedure SetDataType(ADataType: TMapDataType);
    procedure ShowMap(ADataNode: TTreeNode); virtual;
    procedure UpdateCmdStates; override;
    procedure UpdateInfectiousPeriod;
    procedure UpdateMenu(AMenu: TMenuItem); override;
    property OnDateSelect: TDateSelectEvent read FOnDateSelect write SetOnDateSelect;

  end;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, Math,
  TAChartUtils, TAGeometry, TACustomSeries,
  {$IF LCL_FullVersion < 2010000}
  cFixes,
  {$IFEND}
  cGeoReaderKML, cPolygonSeries;

const
  MENUBASE_DATATYPE = 100;

constructor TMapFrame.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentDate := Now;
  MapDateLabel.Caption := '';
  PopulatePaletteListbox(GetMapDataType);
  UpdateGrid;
end;

procedure TMapFrame.cmbDataTypeChange(Sender: TObject);
begin
  SetDataType(TMapDataType(cmbDataType.ItemIndex));
end;

procedure TMapFrame.CreateHandle;
var
  w: Integer;
  s: String;
begin
  inherited;
  w := 0;
  for s in cmbDataType.Items do
    w := Max(w, cmbDataType.Canvas.TextWidth(s));
  cmbDataType.Width := w + GetSystemMetrics(SM_CXVSCROLL) + 8;
end;

procedure TMapFrame.DataPointClickToolPointClick(ATool: TChartTool;
  APoint: TPoint);
var
  ser: TcGeoMapSeries;
  node: TTreeNode;
begin
  if (ATool is TDataPointClickTool) and ((TDataPointClickTool(ATool).Series) is TcGeoMapSeries) then
  begin
    ser := TcGeoMapSeries(TDataPointClickTool(ATool).Series);
    node := FindNodeWithGeoID(ser.GeoID);
    DataTree.Selected := node;
  end;
end;

procedure TMapFrame.DoDateSelect(ADate: TDate);
begin
  FCurrentDate := ADate;
  MapDateLabel.Caption := DateToStr(FCurrentDate);
  if Assigned(FOnDateSelect) then
    FOnDateSelect(Self, FCurrentDate);
end;

procedure TMapFrame.InfoToolAfterMouseMove(ATool: TChartTool; APoint: TPoint);
var
  ser: TcGeoMapSeries;
  params: TNearestPointParams;
  res: TNearestPointResults;
  node: TTreeNode;
  data: TcDataItem;
  d: TDate;
  i: Integer;
  dateIdx: Integer;
begin
  params.FDistFunc := @PointDist;
  params.FOptimizeX := false;
  params.FPoint := APoint;
  params.FRadius := 1;
  params.FTargets := [nptPoint, nptCustom];

  for i := 0 to Chart.SeriesCount-1 do begin
    if Chart.Series[i] is TcGeoMapSeries then begin
      ser := TcGeoMapSeries(Chart.Series[i]);
      if ser.GetNearestPoint(params, res) then
      begin
        node := FindNodeWithGeoID(ser.GeoID);
        if node <> nil then
        begin
          data := GetDataItem(node);
          dateIdx := data.GetDateIndex(MapDateScrollbar.Position + data.FirstDate);
          d := data.Date[dateIdx];
          ShowInfo(GetInfoTitle(node, d), GetInfoText(node, d));
          exit;
        end;
      end;
    end;
  end;
  ShowInfo('', '');
end;

procedure TMapFrame.MapDateScrollBarChange(Sender: TObject);
var
  node: TTreeNode;
  dummy: String;
  plotChildData: Boolean;
begin
  node := DataTree.Selected;
  GetMapResourceParams(node, dummy, plotChildData);
  ShowCoronaMap(node, MapDateScrollbar.Position, GetMapDataType, plotChildData);
  DoDateSelect(GetDataItem(node).FirstDate + MapDateScrollbar.Position);
end;

procedure TMapFrame.PaletteListboxGetColors(Sender: TCustomColorListBox;
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

function TMapFrame.GetMapDataType: TMapDataType;
begin
  Result := TMapDataType(cmbDataType.ItemIndex);
end;

{ The specified node has been clicked.
  If the node's parent data contain a specification of the map to be displayed the
  name of its resource is returned. If there is no map specification then
  the parents are checked upwards until a node with a map resource name is found.
  The clicked node's data contain also information at which tree level the
  data to be mapped are found relative to the map node. }
procedure TMapFrame.GetMapResourceParams(var ADataNode: TTreeNode;
  out AResName: String; out APlotChildNodes: Boolean);
var
  dataItem: TcDataItem;
  ANodeDataItem: TcDataItem;
  datanode: TTreeNode;
  mapNode: TTreeNode;
  i: Integer;
begin
  AResName := '';
  APlotChildNodes := false;

  if ADataNode <> nil then
  begin
    ANodeDataItem := GetDataItem(ADataNode);
    mapNode := ADataNode.Parent;
    if mapNode = nil then
      mapNode := ADataNode.GetFirstSibling;
    repeat
      dataItem := GetDataItem(mapNode);
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
      ADataNode := dataNode;
    end;
  end;

  if AResName = '' then
  begin
    AResName := WorldMapResName;
    ADataNode := DataTree.Items.GetFirstNode.GetFirstChild;
    APlotChildNodes := true;
  end;
end;

procedure TMapFrame.LoadFromIni(ini: TCustomIniFile);
begin
  SetDataType(TMapDataType(ini.ReadInteger('Map', 'DataType', ord(MapSettings.DataType))));

  PageControl.ActivePageIndex := ini.ReadInteger('Map', 'PageControl', PageControl.ActivePageIndex);
  RightPanel.Width := ini.ReadInteger('Map', 'RightPanel', RightPanel.Width);
  Splitter.Left := 0;

  // Update tool button hints
  PageControlChange(nil);
end;

procedure TMapFrame.MenuDataTypeClickHandler(Sender: TObject);
begin
  SetDataType(TMapDataType(TMenuItem(Sender).Tag - MENUBASE_DATATYPE));
end;

procedure TMapFrame.PopulatePaletteListbox(ADataType: TMapDataType);
begin
  case ADataType of
    mdtNormalizedNewConfirmed: FPalette.Init(clWhite, INCIDENCE_PALETTE_ITEMS, 1.0);
    mdtNormalizedNewDeaths: FPalette.Init(clWhite, INCIDENCE_PALETTE_ITEMS, 0.01);
    mdtRValue: FPalette.Init(clWhite, RVALUE_PALETTE_ITEMS, 1.0);
  end;

  PaletteListbox.Style := PaletteListbox.Style - [cbCustomColors];
  PaletteListbox.Style := PaletteListbox.Style + [cbCustomColors];
  PaletteListBox.Selected := clNone;
end;

procedure TMapFrame.SaveToIni(ini: TCustomIniFile);
begin
  ini.WriteInteger('Map', 'DataType', ord(MapSettings.DataType));
  ini.WriteInteger('Map', 'PageControl', PageControl.ActivePageIndex);
  ini.WriteInteger('Map', 'RightPanel', RightPanel.Width);
end;

// Selects the projection for the map having the given name.
// Stategy: "small" maps require orthographic, "large" maps require Mercator
procedure TMapFrame.SelectProjection(AMapRes: string);
begin
  if (AMapRes = WorldMapResName) then
    FGeoMap.Projection := gpMercator
  else
  begin
    FGeoMap.Projection := gpOrthographic;
    if (AMapRes = USStatesMapResName) or (AMapRes = USCountiesMapResName) then
      FGeoMap.GeoIDOffset := 84000000;
  end;
end;

procedure TMapFrame.SetDataType(ADataType: TMapDataType);
var
  i: Integer;
begin
  MapSettings.DataType := ADataType;

  cmbDataType.ItemIndex := ord(ADataType);
  for i := 0 to FMapMenu.Count-1 do
    FMapMenu.Items[i].Checked := (FMapMenu.Items[i].Tag = MENUBASE_DATATYPE + ord(ADataType));

  PopulatePaletteListbox(ADataType);
  ShowMap(DataTree.Selected);
end;

procedure TMapFrame.SetOnDateSelect(AValue: TDateSelectEvent);
begin
  FOnDateSelect := AValue;
  UpdateGrid;
end;

procedure TMapFrame.ShowCoronaMap(ADataNode: TTreeNode;
  ADateIndex: Integer; AMapDataType: TMapDataType; PlotChildNodeData: Boolean);
var
  data: TcDataItem;
  ct: TCaseType;
  title: String;
begin
  if ADataNode = nil then
    exit;

  case AMapDataType of
    mdtNormalizedNewConfirmed:
      begin
        ct := ctConfirmed;
        title := Format('Normalized new %s (per week and %.0n population)',
          [LONG_CASETYPE_NAMES[ct], REF_POPULATION*1.0]);
      end;
    mdtNormalizedNewDeaths:
      begin
        ct := ctDeaths;
        title := Format('Normalized new %s (per week and %.0n population)',
          [LONG_CASETYPE_NAMES[ct], REF_POPULATION*1.0]);
      end;
    mdtRValue:
      begin
        ct := ctConfirmed;
        title := 'Reproduction number R';
      end;
  end;

  Chart.Title.Visible := true;
  Chart.Title.Text.Text := title;

  data := GetDataItem(ADataNode);
  DoDateSelect(data.FirstDate + ADateIndex);

  if PlotChildNodeData then begin
    while ADataNode <> nil do
    begin
      ShowCoronaMapLevel(ADataNode.GetFirstChild, ADateIndex, AMapDataType);
      ADataNode := ADataNode.GetNextSibling;
    end
  end else
    ShowCoronaMapLevel(ADataNode, ADateIndex, AMapDataType);
end;

procedure TMapFrame.ShowCoronaMapLevel(ADataNode: TTreeNode;
  ADateIndex: Integer; AMapDataType: TMapDataType);
var
  data: TcDataItem;
  value, dummy: Double;
  clr: TColor;
  ser: TcGeoMapSeries;
  ct: TCaseType = ctConfirmed;
begin
  case AMapDataType of
    mdtNormalizedNewConfirmed: ct := ctConfirmed;
    mdtNormalizedNewDeaths: ct := ctDeaths;
    mdtRValue: ct := ctConfirmed;
  end;

  while ADataNode <> nil do
  begin
    data := GetDataItem(ADataNode);
    case AMapDataType of
      mdtNormalizedNewConfirmed, mdtNormalizedNewDeaths:
        value := data.CalcNormalizedNewCases(ADateIndex, ct);
      mdtRValue:
        data.CalcRValue(ADateIndex, value, dummy);
    end;
    clr := FPalette.GetColor(value);

    ser := nil;
    if data.GeoID <> -1 then
      ser := FGeoMap.SeriesByID[data.GeoID];
    if ser = nil then
      ser := FGeoMap.SeriesByName[data.Name];
    if ser <> nil then begin
      ser.Brush.Color := clr;
      ser.Node := ADataNode;
    end;

    ADataNode := ADataNode.GetNextSibling;
  end;
end;

procedure TMapFrame.ShowMap(ADataNode: TTreeNode);
var
  mapRes: String = '';
  stream: TStream;
  reader: TcGeoReader;
  data: TcDataItem;
  plotChildData: Boolean;
  n: Integer;
begin
  if FMapLock > 0 then
    exit;

  if FGeoMap = nil then
  begin
    FGeoMap := TcGeoMap.Create(self);
    FGeoMap.Chart := Chart;
    FGeoMap.DefaultPenColor := clGray;
  end else
    FGeoMap.Clear;

  if ADataNode = nil then
    ADataNode := DataTree.Items.GetFirstNode;

  GetMapResourceParams(ADataNode, mapRes, plotChildData);
  if ADataNode = nil then
    exit;

  // Find the best projection for the new map
  SelectProjection(mapRes);

  // Undo any zoom when a new map is loaded
  if mapRes <> FOldMapResource then
  begin
    Chart.ZoomFull;
    FOldMapResource := mapRes;
  end;

  stream := TResourceStream.Create(HINSTANCE, mapRes, LCLType.RT_RCDATA);
  try
    reader := TKMLReader.Create;
    try
      if not reader.Check(stream) then
        raise Exception.Create('Resource ' + mapRes + ' is not a valid KML file.');
      reader.ReadFromStream(stream, FGeoMap);
      FGeoMap.Plot;

      // Prepare date scrollbar
      data := GetDataItem(ADataNode);
      if data <> nil then
      begin
        n := data.Count[pctConfirmed];
        if n > 0 then
        begin
          MapDateScrollbar.Max := n - 1;
          MapDateScrollbar.Min := 0;
          if FCurrentDate - data.FirstDate > MapDateScrollBar.Max then
            MapDateScrollbar.Position := MapDateScrollbar.Max
          else
            MapDateScrollbar.Position := round(FCurrentDate - data.FirstDate);
        end;
      end;

      DoDateSelect(data.GetLastDate);

      // Display the map
      ShowCoronaMap(ADataNode, MapDateScrollbar.Position, GetMapDataType, plotChildData);
    finally
      reader.Free;
    end;

    UpdateGrid;
    UpdateCmdStates;
  finally
    stream.Free;
  end;
end;

procedure TMapFrame.UpdateCmdStates;
begin
  if PageControl.ActivePage = pgTable then
    tbSaveToFile.Enabled := Grid.RowCount > 2
  else
    tbSaveToFile.Enabled := Chart.SeriesCount > 0;
  tbCopyToClipboard.Enabled := tbSaveToFile.Enabled;
end;

procedure TMapFrame.UpdateGrid;
var
  i, idx, n, r: Integer;
  ser: TcGeoMapSeries;
  data: TcDataItem;
begin
  lblTableHdr.Caption := FormatDateTime('dddddd', FCurrentDate);

  n := 0;
  for i := 0 to Chart.SeriesCount-1 do
    if Chart.Series[i] is TcGeoMapSeries then
    begin
      ser := TcGeoMapSeries(Chart.Series[i]);
      data := GetDataItem(ser.Node);
      if data <> nil then
        inc(n);
    end;

  if n = 0 then
  begin
    Grid.RowCount := 2;
    Grid.ColCount := 2;
    Grid.Clear;
    exit;
  end;
  Grid.RowCount := Grid.FixedRows + n + 1;
  Grid.ColCount := Grid.FixedCols + 6;
  Grid.ColWidths[0] := 120;
  Grid.Cells[1, 0] := 'Total infected';
  Grid.Cells[2, 0] := 'Total deaths';
  Grid.Cells[3, 0] := 'New infected';
  Grid.Cells[4, 0] := 'New deaths';
  Grid.Cells[5, 0] := 'Incicence';
  Grid.Cells[6, 0] := 'R value';

  r := Grid.FixedRows;
  for i := 0 to Chart.SeriesCount-1 do
  begin
    if Chart.Series[i] is TcGeoMapSeries then
    begin
      ser := TcGeoMapSeries(Chart.Series[i]);
      data := GetDataItem(ser.Node);
      if (data <> nil) then
      begin
        Grid.Cells[0, r] := ser.Title;
        idx := data.GetDateIndex(FCurrentDate);
        Grid.Cells[1, r] := Format('%.0n', [data.RawData[pctConfirmed][idx]*1.0]);
        Grid.Cells[2, r] := Format('%.0n', [data.RawData[pctDeaths][idx]*1.0]);
        Grid.Cells[3, r] := Format('%.0n', [data.CalcNewCases(idx, ctConfirmed)*1.0]);
        Grid.Cells[4, r] := Format('%.0n', [data.CalcNewCases(idx, ctDeaths)*1.0]);
        Grid.Cells[5, r] := Format('%.1n', [data.CalcNormalizedNewCases(idx, ctConfirmed)]);
        Grid.Cells[6, r] := Format('%.1f', [data.CalcRValue(idx)]);
        inc(r);
      end;
    end;
  end;
end;

procedure TMapFrame.UpdateInfectiousPeriod;
begin
  cmbDataType.Items[2] := Format('Reproduction number (R, % days)', [InfectiousPeriod]);
end;

procedure TMapFrame.UpdateMenu(AMenu: TMenuItem);
var
  i: Integer;
  item: TMenuItem;
begin
  FMapMenu := AMenu;

  for i := 0 to cmbDataType.Items.Count-1 do
  begin
    item := TMenuItem.Create(self);
    item.AutoCheck := true;
    item.Caption := cmbDataType.Items[i];
    item.GroupIndex := 1;
    item.Tag := i + MENUBASE_DATATYPE;
    item.OnClick := @MenuDataTypeClickHandler;
    AMenu.Add(item);
  end;

  item := TMenuItem.Create(self);
  item.Caption := '-';
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acCopyToClipboard;
  AMenu.Add(item);

  item := TMenuItem.Create(self);
  item.Action := acSaveTofile;
  AMenu.Add(item);
end;

end.
