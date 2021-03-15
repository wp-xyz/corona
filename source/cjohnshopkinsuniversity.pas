unit cJohnsHopkinsUniversity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  cGlobal, cDataSource;

type
  TJohnsHopkinsDataSource = class(TcDataSource)
  private
    procedure CalcParentCases(ANode: TTreeNode);
    procedure CalcParentPopulation(ANode: TTreeNode);
    procedure CreateTopNodes(ATreeView: TTreeView;
      out AWorldNode, AAfricaNode, AAsiaNode, AEuropeNode, ANorthAmericaNode,
      ASouthAmericaNode, AOceaniaNode: TTreeNode);
    (*
    function GetDataString_Sick(const ACountry, AState, ACity: String;
      out AHeader, ACounts: String): Boolean;
      *)
    function InternalLoadData(ATreeView: TTreeView; ACaseType: TPrimaryCaseType;
      IsUSAFile: Boolean): Boolean;
  public
    procedure DownloadToCache; override;
    (*
    function GetDataString(const ACountry, AState, ACity: String;
      ACaseType: TCaseType; out AHeader, ACounts: String): Boolean; override;
      *)
    function LoadData(ATreeView: TTreeView; ANode: TTreeNode): Boolean; override;
    function LoadLocations(ATreeView: TTreeView): Boolean; override;
  end;

implementation

uses
  LCLType, Dialogs,
  cUtils, cDownloadManager;

const
  BASE_URL = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/';
  TIMESERIES_URL = BASE_URL + 'csse_covid_19_time_series/';
  FILENAME_CONFIRMED = 'time_series_covid19_confirmed_global.csv';
  FILENAME_DEATHS = 'time_series_covid19_deaths_global.csv';
  FILENAME_RECOVERED = 'time_series_covid19_recovered_global.csv';
  FILENAME_POPULATION = 'UID_ISO_FIPS_LookUp_Table.csv';

  FILENAME_CONFIRMED_US = 'time_series_covid19_confirmed_US.csv';
  FILENAME_DEATHS_US = 'time_series_covid19_deaths_US.csv';

  COUNTRIES_CONTINENTS_RESNAME = 'COUNTRY-AND-CONTINENT-CODES-LIST';

function BeginsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[1] = '"');
end;

function EndsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[Length(s)] = '"');
end;


{ -----------------------------------------------------------------------------}
{  TJohnsHopkinsDatasource                                                     }
{------------------------------------------------------------------------------}

procedure TJohnsHopkinsDatasource.CalcParentCases(ANode: TTreeNode);
var
  child: TTreeNode;
  data, childData: TcDataItem;
  cases: TCaseArray = nil;
  firstDate: TDate = -1;
  i: Integer;
  ct: TPrimaryCaseType;
begin
  if (ANode = nil) and not ANode.HasChildren then
    exit;

  data := TcDataItem(ANode.Data);

  for ct in TPrimaryCaseType do
  begin
    cases := nil;
    firstDate := -1;
    child := ANode.GetFirstChild;
    while (child <> nil) do begin
      childData := TcDataItem(child.Data);
      if cases = nil then
      begin
        SetLength(cases, childData.Count[ct]);
        for i := 0 to High(cases) do cases[i] := 0;
      end;
      if firstDate = -1 then
        firstDate := childData.FirstDate;
      for i := 0 to childData.Count[ct] - 1 do
        cases[i] := cases[i] + childData.RawData[ct][i];
      child := child.GetNextSibling;
    end;
    data.SetCases(firstDate, cases, ct);
  end;
end;

procedure TJohnsHopkinsDataSource.CalcParentPopulation(ANode: TTreeNode);
var
  n: Int64;
  data, childData: TcDataItem;
  child: TTreeNode;
begin
  if (ANode = nil) and not ANode.HasChildren then
    exit;

  data := TcDataItem(ANode.Data);

  n := 0;
  child := ANode.GetFirstChild;
  while (child <> nil) do begin
    childData := TcDataItem(child.Data);
    n := n + childData.population;
    child := child.GetNextSibling;
  end;

  data.Population := n;
end;

procedure TJohnsHopkinsDataSource.CreateTopNodes(ATreeView: TTreeView;
  out AWorldNode, AAfricaNode, AAsiaNode, AEuropeNode, ANorthAmericaNode,
  ASouthAmericaNode, AOceaniaNode: TTreeNode);
var
  data: TcDataItem;
begin
  data := TcDataItem.Create;
  data.Name := 'World';
  data.ParentName := '';
  data.GeoID := 0;
  data.Population := 1;  // to be calculated later
  data.MapResource := WorldMapResName;
  data.MapDataLevelDist := 1;
  data.MapDataAtChildLevel := true;
  AWorldNode := ATreeView.Items.AddChildObject(nil, 'World (JHU)', data);

  // Create continent nodes
  data := TcDataItem.Create;
  data.Name := 'Africa';
  data.ParentName := 'World';
  data.GeoID := -10;
  data.Population := 1;
//  data.MapResource := AfricaMapResName;
  data.MapDataLevelDist := 1;  // 1st level below world map node
  data.MapDataAtChildLevel := true;
  AAfricaNode := ATreeView.Items.AddChildObject(AWorldNode, 'Africa', data);

  data := TcDataItem.Create;
  data.Name := 'Asia';
  data.ParentName := 'World';
  data.GeoID := 142;
  data.Population := 1;
//  data.MapResource := AsiaMapResName;
  data.MapDataLevelDist := 1;  // 1st level below world map node
  data.MapDataAtChildLevel := true;
  AAsiaNode := ATreeView.Items.AddChildObject(AWorldNode, 'Asia', data);

  data := TcDataItem.Create;
  data.Name := 'Europe';
  data.Parentname := 'World';
  data.GeoID := 150;
  data.Population := 1;
//  data.MapResource := EuropeMapResName;
  data.MapDataLevelDist := 1;  // 1st level below world map node
  data.MapDataAtChildLevel := true;
  AEuropeNode := ATreeView.Items.AddChildObject(AWorldNode, 'Europe', data);

  data := TcDataItem.Create;
  data.Name := 'North America';
  data.Parentname := 'World';
  data.GeoID := 19;
  data.Population := 1;
//  data.MapResource := AmericasMapResName;
  data.MapDataLevelDist := 1;  // 1st level below world map node
  data.MapDataAtChildLevel := true;
  ANorthAmericaNode := ATreeView.Items.AddChildObject(AWorldNode, 'North America', data);

  data := TcDataItem.Create;
  data.Name := 'South America';
  data.Parentname := 'World';
  data.GeoID := 19;
  data.Population := 1;
//  data.MapResource := AmericasMapResName;
  data.MapDataLevelDist := 1;  // 1st level below world map node
  data.MapDataAtChildLevel := true;
  ASouthAmericaNode := ATreeView.Items.AddChildObject(AWorldNode, 'South America', data);

  data := TcDataItem.Create;
  data.Name := 'Oceania';
  data.Parentname := 'World';
  data.GeoID := 9;
  data.Population := 1;
  //data.MapResource := OceaniaMapResName;
  data.MapDataLevelDist := 1;  // 1st level below world map node
  data.MapDataAtChildLevel := true;
  AOceaniaNode := ATreeView.Items.AddChildObject(AWorldNode, 'Oceania', data);

  // We skip Antarctica because there are no Covid data for it.
end;

procedure TJohnsHopkinsDatasource.DownloadToCache;
var
  F: TDownloadForm;
begin
  F := TDownloadForm.Create(nil);
  try
    // locations, populations
    F.AddDownload(BASE_URL + FILENAME_POPULATION, FCacheDir + FILENAME_POPULATION);
    // confirmed global
    F.AddDownload(TIMESERIES_URL + FILENAME_CONFIRMED, FCacheDir + FILENAME_CONFIRMED);
    // deaths global
    F.AddDownload(TIMESERIES_URL + FILENAME_DEATHS, FCacheDir + FILENAME_DEATHS);
    // recovered global
    F.AddDownload(TIMESERIES_URL + FILENAME_RECOVERED, FCacheDir + FILENAME_RECOVERED);
    // confirmed U.S.A.
    F.AddDownload(TIMESERIES_URL + FILENAME_CONFIRMED_US, FCacheDir + FILENAME_CONFIRMED_US);
    // deaths U.S.A
    F.AddDownload(TIMESERIES_URL + FILENAME_DEATHS_US, FCacheDir + FILENAME_DEATHS_US);

    F.ShowModal;
  finally
    F.Free;
  end;
end;
                                 (*
function TJohnsHopkinsDataSource.GetDataString(const ACountry, AState, ACity: String;
  ACaseType: TCaseType; out AHeader, ACounts: String): Boolean;
var
  lines: TStrings;
  fields: TStrings;
  fn: String;
  i, j: integer;
  isUSAFile: Boolean;
  combinedKey: String;
begin
  Result := false;

  if ACaseType = ctSick then
  begin
    Result := GetDataString_Sick(ACountry, AState, ACity, AHeader, ACounts);
    exit;
  end;

  isUSAFile := (ACountry = 'US') and (ACity <> '');
  if isUSAFile then
  begin
    case ACaseType of
      ctConfirmed : fn := FCacheDir + FILENAME_CONFIRMED_US;
      ctDeaths    : fn := FCacheDir + FILENAME_DEATHS_US;
      ctRecovered : exit;
    end;
    combinedKey := Format('"%s, %s, US"', [ACity, AState]);
  end else
    case ACaseType of
      ctConfirmed : fn := FCacheDir + FILENAME_CONFIRMED;
      ctDeaths    : fn := FCacheDir + FILENAME_DEATHS;
      ctRecovered : fn := FCacheDir + FILENAME_RECOVERED;
    end;

  if not FileExists(fn) then
  begin
    MessageDlg(Format('Data file "%s" not found. Please press "Update files".', [fn]), mtError, [mbOK], 0);
    exit;
  end;

  lines := TStringList.Create;
  fields := TStringList.Create;
  try
    fields.StrictDelimiter := true;
    lines.LoadFromFile(fn);

    AHeader := lines[0];
    if isUSAFile then begin
      Split(AHeader, fields);
      if ACaseType = ctDeaths then
        fields.Delete(11);
      fields.Delete(10);
      for j := 0 to 5 do
        fields.Delete(0);
      AHeader := fields.CommaText;
    end;

    for i:=1 to lines.Count-1 do begin
      if lines[i] = '' then
        Continue;
      if BeginsWithQuote(lines[i]) then
        Continue;
      Split(lines[i], fields);
      if isUSAFile then begin
        if fields[10] = combinedKey then
        begin
          if ACaseType = ctDeaths then
            fields.Delete(11);
          fields.Delete(10);
          for j := 0 to 5 do
            fields.Delete(0);
          ACounts := fields.CommaText;
          Result := true;
          exit;
        end;
      end else
      begin
        if (Unquote(fields[1]) = ACountry) and (Unquote(fields[0]) = AState) then  // Unquote needed for quoted "Korea, South"
        begin
          ACounts := lines[i];
          Result := true;
          exit;
        end;
      end;
    end;
  finally
    fields.Free;
    lines.Free;
  end;
end;

function TJohnsHopkinsDataSource.GetDataString_Sick(const ACountry, AState, ACity: String;
  out AHeader, ACounts: String): Boolean;
var
  sConfirmed, sDeaths, sRecovered: String;
  saConfirmed, saDeaths, saRecovered: TStringArray;
  nConfirmed, nDeaths, nRecovered, nSick: Integer;
  i: Integer;
begin
  Result := GetDataString(ACountry, AState, ACity, ctConfirmed, AHeader, sConfirmed) and
            GetDataString(ACountry, AState, ACity, ctDeaths, AHeader, sDeaths) and
            GetDataString(ACountry, AState, ACity, ctRecovered, AHeader, sRecovered);
  if Result then
  begin
    saConfirmed := sConfirmed.Split(',', '"');
    saDeaths := sDeaths.Split(',', '"');
    saRecovered := sRecovered.Split(',', '"');
    ACounts := saConfirmed[0] + ',' + saConfirmed[1] + ',' + saConfirmed[2] + ',' + saConfirmed[3];
    for i := 4 to High(saConfirmed) do begin
      nConfirmed := StrToIntDef(saConfirmed[i], 0);
      nDeaths := StrToIntDef(saDeaths[i], 0);
      nRecovered := StrTointDef(saRecovered[i], 0);
      nSick := nConfirmed - nDeaths - nRecovered;
      ACounts := ACounts + ',' + IntToStr(nSick);
    end;
  end;
end;
        *)
function TJohnsHopkinsDataSource.InternalLoadData(ATreeView: TTreeView;
  ACaseType: TPrimaryCaseType; IsUSAFile: Boolean): Boolean;
var
  lines: TStrings;
  fields: TStrings;
  fn: String;
  firstDate: TDate;
  country, state, district: String;
  i, j: integer;
  fs: TFormatSettings;
  countryNode, stateNode, districtNode: TTreeNode;
  data: TcDataItem;
  cases: TCaseArray = nil;
  firstDataIndex: Integer;
begin
  Result := false;

  fs := FormatSettings;
  fs.DateSeparator := '/';
  fs.ShortDateformat := 'm/d/yy';

  if IsUSAFile then
    case ACaseType of
      pctConfirmed: fn := FCacheDir + FILENAME_CONFIRMED_US;
      pctDeaths   : fn := FCacheDir + FILENAME_DEATHS_US;
      pctRecovered: exit;
    end
  else
    case ACaseType of
      pctConfirmed: fn := FCacheDir + FILENAME_CONFIRMED;
      pctDeaths   : fn := FCacheDir + FILENAME_DEATHS;
      pctRecovered: fn := FCacheDir + FILENAME_RECOVERED;
    end;

  lines := TStringList.Create;
  try
    lines.LoadFromFile(fn);

    fields := TStringList.Create;
    try
      fields.StrictDelimiter := true;
      Split(lines[0], fields);

      if IsUSAfile then
      begin
        if ACaseType = pctConfirmed
          then firstDataIndex := 11
          else firstDataIndex := 12;
        firstDate := StrToDate(fields[firstDataIndex], fs);
        for i := 1 to lines.Count-1 do begin
          Split(lines[i], fields);
          district := fields[5];
          state := fields[6];
          country := fields[7];
          countryNode := ATreeView.Items.FindNodeWithText(country);
          stateNode := countryNode.FindNode(state);
          if stateNode = nil then
            continue;
          districtNode := stateNode.FindNode(district);
          if districtNode = nil then
            continue;
          data := TcDataItem(districtNode.Data);

          // extract case count
          SetLength(cases, fields.Count-firstDataIndex);
          for j := firstDataIndex to fields.Count-1 do
            cases[j-firstDataIndex] := StrToInt64(fields[j]);

          if Length(cases) > 0 then
            data.SetCases(firstDate, cases, ACaseType)
          else
            data.Free;
        end;
      end else
      begin
        firstDate := StrToDate(fields[4], fs);
        for i := 1 to lines.Count-1 do begin
          Split(lines[i], fields);
          state := Unquote(fields[0]);
          country := Unquote(fields[1]);
          countryNode := ATreeView.Items.FindNodeWithText(country);
          if state <> '' then
          begin
            stateNode := countryNode.FindNode(state);
            if stateNode <> nil then
              data := TcDataItem(stateNode.Data)
            else
              continue;
          end else
          begin
            if countryNode <> nil then
              data := TcDataItem(countryNode.Data)
            else
              continue;
          end;

          // extract case count
          SetLength(cases, fields.Count - 4);
          for j := 4 to fields.Count-1 do
            cases[j-4] := StrToInt64(fields[j]);

          if Length(cases) > 0 then
            data.SetCases(firstDate, cases, ACaseType)
          else
            data.Free;
        end;
      end;

      Result := true;
    finally
      fields.Free;
    end;
  finally
    lines.Free;
  end;
end;

function TJohnsHopkinsDataSource.LoadData(ATreeView: TTreeView;
  ANode: TTreeNode): Boolean;
var
  node: TTreeNode;
begin
  Result :=
    // World data
    InternalLoadData(ATreeView, pctConfirmed, false) and
    InternalLoadData(ATreeView, pctDeaths, false) and
    InternalLoadData(ATreeView, pctRecovered, false) and
    // US data
    InternalLoadData(ATreeView, pctConfirmed, true) and
    InternalLoadData(ATreeView, pctDeaths, true);  // no "recovered" for US

  // Some JHU nodes belong to parents which are missing any case counts -->
  // Calculate cases for "empty" parents from the sums of the children.
  if Result then
  begin
    CalcParentCases(ATreeView.Items.FindNodeWithText('Australia'));
    CalcParentCases(ATreeView.Items.FindNodeWithText('Canada'));
    CalcParentCases(ATreeView.Items.FindNodewithText('China'));

    // Calculate the US federal states
    node := ATreeView.Items.FindNodeWithText('US').GetFirstChild;
    while (node <> nil) do
    begin
      CalcParentCases(node);
      node := node.GetNextSibling;
    end;

    // Calculate continent cases and population (missing from JHU data)
    node := ATreeView.Items.GetFirstNode.GetFirstChild;  // 1st continent node
    while node <> nil do
    begin
      CalcParentCases(node);
      CalcParentPopulation(node);
      node := node.GetNextSibling;
    end;

    // Calculate world cases and population (missing from JHU data)
    node := ATreeView.Items.GetFirstNode;   // World node
    CalcParentCases(node);
    CalcParentPopulation(node);
  end;
end;

function TJohnsHopkinsDataSource.LoadLocations(ATreeView: TTreeView): Boolean;
var
  fn: String;
  L: TStrings;
  CCList: TStringList;
  worldNode, continentNode, countryNode, stateNode, cityNode, node: TTreeNode;
  africaNode, asiaNode, europeNode, northAmericaNode, southAmericaNode, oceaniaNode: TTreeNode;
  sa: TStringArray;
  i, j, p: Integer;
  population: Int64;
  continent, country, state, city: String;
  geoID: TGeoID;
  lon, lat: Double;
  countryData: TcDataItem;
  data: TcDataItem;
  stream: TResourceStream;
begin
  Result := false;

  // All locations (countries, states) are listed also in the measurement files
  // but the FILENAME_POPULATION contains also meta data, such as population.
  fn := FCacheDir + FILENAME_POPULATION;
  if not FileExists(fn) then
    exit;

  // Create "world" node as pseudo-root and continent nodes as 1st level
  CreateTopNodes(ATreeView, worldNode,
    africaNode, asiaNode, europeNode, northAmericaNode, southAmericaNode, oceaniaNode);

  // CCList is a list of the (sorted) country names having the associated
  // continent tree node as an object.
  CCList := TStringList.Create;  // CC = continent-country
  try
    // Read contries and continents from resource
    stream := TResourceStream.Create(HINSTANCE, COUNTRIES_CONTINENTS_RESNAME, LCLType.RT_RCDATA);
    try
      CCList.LoadFromStream(stream);
    finally
      stream.Free;
    end;
    CCList.Delete(0);  // Delete header
    for i := 0 to CCList.Count-1 do begin
      sa := CCList[i].Split(',', '"');
      continent := sa[0];
      country := Unquote(sa[2]);
      if country <> 'Korea, South' then
      begin
        p := pos(',', country);
        if p > 0 then country := Copy(country, 1, p-1);
      end;
      case continent of
        'Africa': CCList.Objects[i] := africaNode;
        'Asia': CCList.Objects[i] := asiaNode;
        'Europe': CCList.Objects[i] := europeNode;
        'North America': CClist.Objects[i] := northAmericaNode;
        'South America': CCList.Objects[i] := southAmericaNode;
        'Oceania': CCList.Objects[i] := oceaniaNode;
      end;
      CCList[i] := country;
    end;
    CCList.Sorted := true;

    L := TStringList.Create;
    try
      L.LoadFromFile(fn);
      for i:=1 to L.Count-1 do begin    // skip header --> i=1
        if L[i] = '' then
          Continue;

        sa := L[i].Split(',', '"');
        if Length(sa) <> 12 then
          Continue;

        country := Unquote(sa[7]);
        state := Unquote(sa[6]);
        city := Unquote(sa[5]);
        if not TryStrToInt64(sa[0], geoID) then geoID := -1;
        if not TryStrToInt64(sa[11], population) then population := -1;
        if not TryStrToFloat(sa[9], lon, cFormatSettings) then lon := 0;
        if not TryStrToFloat(sa[8], lat, cFormatSettings) then lat := 0;

        // Remove cruizing ships found in the JHU data
        case country[1] of
          'D': if (country = 'Diamond Princess') then continue;
          'M': if (country = 'MS Zaandam') then continue;
        end;

        // Avoid too many empty nodes in the tree vieew
        if (state <> '') then
        begin
          // The 'US' data are contained in separate files not read by this program
          // No data provided in the standard files for the provinces of these countries
          case country[1] of
            'B': if (country = 'Brazil') or (country = 'Belgium') then continue;
            'C': if (country = 'Chile') or (country = 'Colombia') then continue;
            'G': if (country = 'Germany') then continue;
            'I': if (country = 'Italy') or (country = 'India') then continue;
            'J': if (country = 'Japan') then continue;
            'M': if (country = 'Mexico') then continue;
            'N': if (country = 'Nigeria') then continue;
            'P': if (country = 'Pakistan') or (country = 'Peru') then continue;
            'R': if (country = 'Russia') then continue;
            'S': if (country = 'Spain') or (country = 'Sweden') then continue;
            'U': if (country = 'Ukraine') then continue;
            else ;
          end;
        end;

        // Determine the continent node of the current country.
        if CCList.Find(country, j) then
          continentNode := TTreeNode(CCList.Objects[j])
        else
          continentNode := worldNode;

        // Lookup the continent node
        countryNode := continentNode.FindNode(country);
        if countryNode = nil then
        begin
          countryData := TcDataItem.Create;
          countryData.Name := country;
          countryData.ParentName := '';
          countryData.GeoID := geoID;
          countryData.Population := population;
          countryData.Longitude := lon;
          countryData.Latitude := lat;
          countryData.RandSeed := Random(MaxInt);
          // Some contries in the JHU files have child nodes with state/province data
          // These are handled here:
          case country of
            'Australia':
              begin
                // Information used by child nodes
                countryData.MapResource := AustraliaMapResName;
                // Information used by the node itself: it is a child node of
                // the continent node, and therefore we must define this:
                countryData.MapDataLevelDist := 1;
                countryData.MapDataAtChildLevel := true;
              end;
            'Canada':
              begin
                countryData.MapResource := CanadaMapResName;
                countryData.MapDataLevelDist := 1;
                countryData.MapDataAtChildLevel := true;
              end;
            'China':
              begin
                countryData.MapResource := ChinaMapResName;
                countryData.MapDataLevelDist := 1;
                countryData.MapDataAtChildLevel := true;
              end;
            'US':
              begin
                // Information for country node itself
                countryData.MapDataLevelDist := 1;
                countryData.MapDataAtChildLevel := true;
                // Information for states level
                countryData.MapResource := USStatesMapResName;
                // Information for county level
                countryData.OtherMapResource := USCountiesMapResName;
              end;
            else   // World map
              countryData.MapDataLevelDist := 1;        // Map: Iterate through continents...
              countryData.MapDataAtChildLevel := true;  // ... but find values in country level
          end;

          countryNode := ATreeView.Items.AddChildObject(continentNode, country, countryData);
        end;

        if state <> '' then
        begin
          stateNode := nil;
          cityNode := nil;
          node := countryNode.GetFirstChild;
          while (node <> nil) do begin
            if node.Text = state then
            begin
              stateNode := node;
              break;
            end;
            node := node.GetNextSibling;
          end;
          if stateNode = nil then
          begin
            data := TcDataItem.Create;
            data.Name := state;
            data.ParentName := country;
            data.GeoID := geoID;
            data.Population := population;
            data.Longitude := lon;
            data.Latitude := lat;
            data.MapDataLevelDist := 1;
            data.MapDataAtChildLevel := true;
            data.RandSeed := Random(MaxInt);
            case country of
              'Australia':
                begin
                  data.MapDataLevelDist := 1;
                  data.MapDataAtChildLevel := false;
                end;
              'Canada':
                begin
                  data.MapDataLevelDist := 1;
                  data.MapDataAtChildLevel := false;
                end;
              'China':
                begin
                  data.MapDataLevelDist := 1;
                  data.MapDataAtChildLevel := false;
                end;
              'US':
                begin
                  data.MapDataLevelDist := 1;
                  data.MapDataAtChildLevel := false;
                end;
            end;
            stateNode := ATreeView.Items.AddChildObject(countryNode, state, data);
          end;

          if (city <> '') then
          begin
            data := TcDataItem.Create;
            data.Name := city;
            data.ParentName := state;
            data.GeoID := geoID;
            data.Population := population;
            data.Longitude := lon;
            data.Latitude := lat;
            data.MapDataLevelDist := 2;
            data.MapDataAtChildLevel := true;
            data.RandSeed := Random(MaxInt);
            if country = 'US' then
            begin
              data.UseOtherMapResource := true;
              data.OtherMapDataLevelDist := 1;
              data.OtherMapDataAtChildLevel := true;
            end;
            citynode := ATreeView.Items.AddChildObject(stateNode, city, data);
          end;
        end;
      end;

      worldNode.Expanded := true;
      worldNode.AlphaSort;

    finally
      L.Free;
    end;

  finally
    CCList.Free;
  end;

  Result := true;
end;

(*
function TJohnsHopkinsDataSource.LoadLocations(ATreeView: TTreeView): Boolean;
var
  fn: String;
  L: TStrings;
  countryNode, stateNode, cityNode, node: TTreeNode;
  sa: TStringArray;
  i: Integer;
  population: Integer;
  country, state, city: String;
  loc: PLocationParams;
begin
  Result := false;

  // all locations (countries, states) are listed also in the measurement files
  // but the FILENAME_POPULATION contains also meta data, such as population.
  fn := FCacheDir + FILENAME_POPULATION;
  if not FileExists(fn) then
    exit;

  L := TStringList.Create;
  try
    L.LoadFromFile(fn);
    for i:=1 to L.Count-1 do begin
      if L[i] = '' then
        Continue;

      sa := L[i].Split(',', '"');
      if Length(sa) <> 12 then
        Continue;

      country := Unquote(sa[7]);
      state := Unquote(sa[6]);
      city := Unquote(sa[5]);
      population := StrToIntDef(sa[11], 0);

      // avoid too many empty nodes in the tree vieew
      if (state <> '') then
      begin
        // The 'US' data are contained in separate files not read by this program
        // No data provided in the standard files for the provinces of these countries
        case country[1] of
          'B': if (country = 'Brazil') then continue;
          'C': if (country = 'Chile') or (country = 'Colombia') then continue;
          'G': if (country = 'Germany') then continue;
          'I': if (country = 'Italy') or (country = 'India') then continue;
          'J': if (country = 'Japan') then continue;
          'M': if (country = 'Mexico') then continue;
          'P': if (country = 'Pakistan') or (country = 'Peru') then continue;
          'R': if (country = 'Russia') then continue;
          'S': if (country = 'Spain') or (country = 'Sweden') then continue;
          'U': if (country = 'Ukraine') then continue;
          else ;
        end;
      end;

      countryNode := ATreeView.Items.FindTopLvlNode(country);
      if countryNode = nil then
      begin
        New(loc);
        loc^.ID := -1;  // not used by JHU
        loc^.Population := population;
        {$IFDEF DEBUG_LOCATIONPARAMS}
        loc^.Name := country;
        {$ENDIF}
        countryNode := ATreeView.Items.AddChildObject(nil, country, loc)
      end;

      if state <> '' then
      begin
        stateNode := nil;
        cityNode := nil;
        node := countryNode.GetFirstChild;
        while (node <> nil) do begin
          if node.Text = state then
          begin
            stateNode := node;
            break;
          end;
          node := node.GetNextSibling;
        end;
        if stateNode = nil then
        begin
          New(loc);
          loc^.ID := -1;  // not used by JHU
          loc^.Population := population;
          {$IFDEF DEBUG_LOCATIONPARAMS}
          loc^.Name := state;
          {$ENDIF}
          stateNode := ATreeView.Items.AddChildObject(countryNode, state, loc);
        end;

        if (city <> '') then
        begin
          New(loc);
          loc^.ID := -1;  // not used by JHU
          loc^.Population := population;
          {$IFDEF DEBUG_LOCATIONPARAMS}
          loc^.Name = city;
          {$ENDIF}
          citynode := ATreeView.Items.AddChildObject(stateNode, city, loc);
        end;
      end;
    end;
  finally
    L.Free;
  end;

  Result := true;
end;
  *)

end.

