unit cGeoReaderKML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_dom, laz2_xmlread,
  cGlobal, cGeoMap;

type
  TKMLReader = class(TcGeoReader)
  private
    procedure ExtractItem(ANode: TDOMNode; AMap: TcGeoMap);
    procedure ExtractPolygon(ANode: TDOMNode; var AItem: TcGeoItem);
    procedure StringToPolygon(AText: string; var AItem: TcGeoItem);
  public
    class function Check(AStream: TStream): Boolean; override;
    procedure ReadFromStream(AStream: TStream; AMap: TcGeoMap); override;
  end;


implementation

uses
  TAChartUtils, TAGeometry;

function GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if (ANode = nil) or (ANode.Attributes = nil) then
    exit;

  Found := false;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then begin
      Found := true;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;


{ TKMLReader }

class function TKMLReader.Check(AStream: TStream): Boolean;
var
  s: String;
  P: Int64;
begin
  P := AStream.Position;
  SetLength(s, 1000);
  AStream.Read(s[1], 1000);
  Result := pos('<kml ', s) > 0;
  AStream.Position := P;
end;

procedure TKMLReader.ExtractItem(ANode: TDOMNode; AMap: TcGeoMap);
var
  node: TDOMNode;
  childNode, child2Node: TDOMNode;
  item: TcGeoItem;
  nodeName: String;
  itemName: String;
  geoID: TGeoID;
  s: String;
  ser: TcGeoMapSeries;
begin
  itemName := '';
  geoID := -1;
  node := ANode.FirstChild;
  while node <> nil do
  begin
    nodeName := node.NodeName;
    if nodeName = 'name' then
      itemName := node.FirstChild.NodeValue
    else
    if nodeName = 'ExtendedData' then
    begin
      childNode := node.FirstChild;
      while childNode <> nil do
      begin
        nodeName := childNode.NodeName;
        if nodeName = 'SimpleData' then
        begin
          s := GetAttrValue(childNode, 'name');
          if (s = 'UN') or (s = 'GEOID') then
          begin
            s := childNode.Firstchild.NodeValue;
            if s <> '' then
              geoID := StrToInt64(s);
          end;
        end else
        if nodeName = 'SchemaData' then
        begin
          child2Node := childNode.FirstChild;
          while child2Node <> nil do
          begin
            nodeName := child2Node.NodeName;
            if nodeName = 'SimpleData' then
            begin
              s := GetAttrValue(child2Node, 'name');
              if s = 'NAME' then
              begin
                s := child2Node.FirstChild.NodeValue;
                if s <> '' then
                  itemName := s;
              end else
              if s = 'GEOID' then
              begin
                s := child2Node.FirstChild.NodeValue;
                if s <> '' then
                  geoID := StrToInt64(s);
              end;
            end;
            child2Node := child2Node.NextSibling;
          end;
        end;
        childNode := childNode.NextSibling;
      end;
    end else
    if nodeName = 'MultiGeometry' then
    begin
      childNode := node.FirstChild;
      while childNode <> nil do
      begin
        nodeName := childNode.NodeName;
        if nodeName = 'Polygon' then
        begin
          item.Name := itemName;
          item.GeoID := geoID;
          item.Polygon := nil;
          ExtractPolygon(childNode, item);
          AMap.AddGeoPolygon(item.Name, item.GeoID, item.Polygon);
        end else
        if nodeName = 'MultiGeometry' then
        begin
          child2node := childNode.FirstChild;
          while child2node <> nil do
          begin
            nodeName := child2Node.NodeName;
            if nodeName = 'Polygon' then
            begin
              item.Name := itemName;
              item.GeoID := geoID;
              item.Polygon := nil;
              ExtractPolygon(child2node, item);
              AMap.AddGeoPolygon(item.Name, item.GeoID, item.Polygon);
            end;
            child2Node := child2Node.NextSibling;
          end;
        end;
        childNode := childNode.NextSibling;
      end;
    end else
    if nodeName = 'Polygon' then
    begin
      item.Name := itemName;
      item.GeoID := geoID;
      item.Polygon := nil;
      ExtractPolygon(node, item);
      AMap.AddGeoPolygon(item.Name, item.GeoID, item.Polygon);
    end;
    node := node.NextSibling;
  end;
end;

procedure TKMLReader.ExtractPolygon(ANode: TDOMNode; var AItem: TcGeoItem);
var
  boundaryNode: TDOMNode;
  linearRingNode: TDOMNode;
  coordinatesNode: TDOMNode;
  isInnerBoundary: Boolean;
  nodeName, s: String;
begin
  boundaryNode := ANode.FirstChild;
  while boundaryNode <> nil do
  begin
    nodeName := boundaryNode.NodeName;
    if (nodeName = 'outerBoundaryIs') or (nodeName = 'innerBoundaryIs') then
    begin
      isInnerBoundary := (nodeName = 'innerBoundaryIs');
      linearRingNode := boundaryNode.FirstChild;
      while linearRingNode <> nil do
      begin
        nodeName := linearRingNode.NodeName;
        if nodeName = 'LinearRing' then
        begin
          coordinatesNode := linearRingNode.FirstChild;
          while coordinatesNode <> nil do
          begin
            nodeName := coordinatesNode.NodeName;
            if nodeName = 'coordinates' then
            begin
              s := trim(coordinatesNode.FirstChild.NodeValue);
              StringToPolygon(s, AItem);
            end;
            coordinatesNode := coordinatesNode.NextSibling;
          end;
        end;
        linearRingNode := linearRingNode.NextSibling;
      end;
    end;
    boundaryNode := boundaryNode.NextSibling;
  end;
end;

procedure TKMLReader.StringToPolygon(AText: string; var AItem: TcGeoItem);
var
  L: TStringList;
  P: Array of TDoublePoint;
  i, j: Integer;
  sa: TStringArray;
  lon, lat: Double;
  res: Integer;
begin
  L := TStringList.Create;
  try
    L.Text := AText;
    if trim(L[L.Count-1]) = '' then
      L.Delete(L.Count-1);
    if L.Count = 1 then
    begin
      L.Delimiter := ' ';
      L.DelimitedText := AText;
    end;
    SetLength(P, L.Count + 1);    // +1: prepare for optional point to close the polygon

    j := 0;
    for i := 0 to L.Count-1 do
    begin
      if trim(L[i]) = '' then
        continue;
      sa := L[i].Split(',');
      val(trim(sa[0]), lon, res);
      val(trim(sa[1]), lat, res);
      P[j].X := lon;
      P[j].Y := lat;
      inc(j);
    end;

    // Make sure that polygon is closed;
    if not (P[j-1] = P[0]) then
    begin
      P[j] := P[0];
      inc(j);
    end;

    // Trim P to real length
    SetLength(P, j);

    // Start index of this polygon part
    j := Length(AItem.Polygon);
    SetLength(AItem.RingStart, Length(AItem.RingStart) + 1);
    AItem.RingStart[High(AItem.Ringstart)] := j;

    // Append new polygon part to polygon already extracted
    SetLength(AItem.Polygon, Length(AItem.Polygon) + Length(P));
    for i := 0 to High(P) do
    begin
      AItem.Polygon[j].X := P[i].X;
      AItem.Polygon[j].Y := P[i].Y;
      inc(j);
    end;
  finally
    L.Free;
  end;
end;

procedure TKMLReader.ReadFromStream(AStream: TStream; AMap: TcGeoMap);
var
  doc: TXMLDocument;
  docNode: TDOMNode;
  placemarkNode: TDOMNode;
  node: TDOMNode;
  nodeName: String;
begin
  if not Check(AStream) then
    raise Exception.Create('Invalid kml file.');

  doc := nil;
  try
    ReadXMLFile(doc, AStream);
    docNode := doc.DocumentElement.FindNode('Document');
    if docNode = nil then
      exit;

    node := docNode.FirstChild;
    while node <> nil do
    begin
      nodeName := node.NodeName;
      if nodeName = 'Placemark' then
        ExtractItem(node, AMap)
      else
      begin
        placemarkNode := node.FindNode('Placemark');
        while placemarknode <> nil do
        begin
          ExtractItem(placemarkNode, AMap);
          placemarkNode := placemarkNode.NextSibling;
        end;
      end;
      node := node.NextSibling;
    end;

  finally
    AMap.Chart.Invalidate;
    Doc.Free;
  end;
end;

initialization
  RegisterGeoReader('kml files (*.kml)', '.kml', TKMLReader);

end.

