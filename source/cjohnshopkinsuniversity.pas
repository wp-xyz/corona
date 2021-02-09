unit cJohnsHopkinsUniversity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, StrUtils,
  cGlobal, cDataSource;

type
  TJohnsHopkinsDataSource = class(TcDataSource)
  private
    FDownloadCounter: Integer;
    FMaxDownloadCounter: Integer;
    function GetDataString_Sick(const ACountry, AState, ACity: String;
      out AHeader, ACounts: String): Boolean;
  public
    procedure DownloadToCache; override;
    function GetDataString(const ACountry, AState, ACity: String;
      ACaseType: TCaseType; out AHeader, ACounts: String): Boolean; override;
    function LoadLocations(ATreeView: TTreeView): Boolean; override;
  end;

implementation

uses
  Dialogs,
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

function BeginsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[1] = '"');
end;

function EndsWithQuote(s: String): Boolean;
begin
  Result := (s <> '') and (s[Length(s)] = '"');
end;

{ -----------------------------------------------------------------------------}

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


end.

