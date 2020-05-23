unit cJohnsHopkinsUniversity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  cGlobal, cDataSource;

type
  TJohnsHopkinsDataSource = class(TcDataSource)
  private
    function GetDataString_Sick(const ACountry, AState: String;
      out AHeader, ACounts: String): Boolean;

  public
    procedure DownloadToCache; override;
    function GetDataString(const ACountry, AState: String; ACaseType: TCaseType;
      out AHeader, ACounts: String): Boolean; override;
    function LoadLocations(ATreeView: TTreeView): Boolean; override;
  end;

implementation

uses
  Dialogs,
  cDownloader;

const
  BASE_URL = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/';
  TIMESERIES_URL = BASE_URL + 'csse_covid_19_time_series/';
  FILENAME_CONFIRMED = 'time_series_covid19_confirmed_global.csv';
  FILENAME_DEATHS = 'time_series_covid19_deaths_global.csv';
  FILENAME_RECOVERED = 'time_series_covid19_recovered_global.csv';
  FILENAME_POPULATION = 'UID_ISO_FIPS_LookUp_Table.csv';

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
const
  DOWNLOAD_ERR = 'Download error.';
  DOWNLOAD_FROM = 'Download from: ';
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    DoStatusMsg(DOWNLOAD_FROM, TIMESERIES_URL + FILENAME_CONFIRMED + '...');
    if not DownloadFile(TIMESERIES_URL + FILENAME_CONFIRMED, stream) then
    begin
      DoStatusMsg(DOWNLOAD_ERR, '');
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOK], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(FCacheDir + FILENAME_CONFIRMED);

    stream.Position := 0;
    DoStatusMsg(DOWNLOAD_FROM, TIMESERIES_URL + FILENAME_DEATHS + '...');
    if not DownloadFile(TIMESERIES_URL + FILENAME_DEATHS, stream) then
    begin
      DoStatusMsg(DOWNLOAD_ERR, '');
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOk], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(FCacheDir + FILENAME_DEATHS);

    stream.Position := 0;
    DoStatusMsg(DOWNLOAD_FROM, TIMESERIES_URL + FILENAME_RECOVERED + '...');
    if not DownloadFile(TIMESERIES_URL + FILENAME_RECOVERED, stream) then
    begin
      DoStatusMsg(DOWNLOAD_ERR, '');
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOk], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(FCacheDir + FILENAME_RECOVERED);

    stream.Position := 0;
    DoStatusmsg(DOWNLOAD_ERR, '');
    if not DownloadFile(BASE_URL + FILENAME_POPULATION, stream) then
    begin
      DoStatusMsg(DOWNLOAD_ERR, '');
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOK], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(FCacheDir + FILENAME_POPULATION);


  finally
    stream.Free;
  end;
end;

function TJohnsHopkinsDataSource.GetDataString(const ACountry, AState: String;
  ACaseType: TCaseType; out AHeader, ACounts: String): Boolean;
var
  L: TStrings;
  fn: String;
  i: integer;
  sa: TStringArray;
begin
  if ACaseType = ctSick then
  begin
    Result := GetDataString_Sick(ACountry, AState, AHeader, ACounts);
    exit;
  end;

  Result := false;

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
      if (sa[1] = ACountry) and (sa[0] = AState) then
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

function TJohnsHopkinsDataSource.GetDataString_Sick(const ACountry, AState: String;
  out AHeader, ACounts: String): Boolean;
var
  sConfirmed, sDeaths, sRecovered: String;
  saConfirmed, saDeaths, saRecovered: TStringArray;
  nConfirmed, nDeaths, nRecovered, nSick: Integer;
  i: Integer;
begin
  Result := GetDataString(ACountry, AState, ctConfirmed, AHeader, sConfirmed) and
            GetDataString(ACountry, AState, ctDeaths, AHeader, sDeaths) and
            GetDataString(ACountry, AState, ctRecovered, AHeader, sRecovered);
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
  node: TTreeNode;
  sa: TStringArray;
  i: Integer;
  population: Integer;
  country, state: String;
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

      country := AnsiDequotedStr(sa[7], '"');
      state := AnsiDequotedStr(sa[6], '"');
      population := StrToIntDef(sa[11], 0);

      // avoid too many empty nodes in the tree vieew
      if (state <> '') then
      begin
        // The 'US' data are contained in separate files not read by this program
        if (country = 'US') then
          Continue;
        // No data provided in the standard files for the provinces of these countries
        if (country = 'Spain') or (country = 'Italy') or (country = 'Germany') or
           (country = 'Brazil') or (country = 'Chile') or (country = 'Japanm') or
           (country = 'Mexico') then
          Continue;
      end;

      New(loc);
      loc^.ID := -1;  // not used by JHU
      loc^.Population := population;

      node := ATreeView.Items.FindTopLvlNode(country);
      if node = nil then
      begin
        {$IFDEF DEBUG_LOCATIONPARAMS}
        loc^.Name := country;
        {$ENDIF}
        node := ATreeView.Items.AddChildObject(nil, country, loc)
      end
      else
      if state <> '' then
      begin
        {$IFDEF DEBUG_LOCATIONPARAMS}
        loc^.Name := state;
        {$ENDIF}
        node := ATreeView.Items.AddChildObject(node, state, loc);
      end;
    end;
  finally
    L.Free;
  end;

  Result := true;
end;


end.

