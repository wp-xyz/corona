unit cJohnsHopkinsUniversity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  cGlobal, cDataSource;

type
  TJohnsHopkinsDataSource = class(TcDataSource)
  public
    procedure DownloadToCache; override;
    function GetDataString(const ACountry, AState: String; ACaseType: TCaseType;
      var AHeader, ACounts: String): Boolean; override;
    function LoadLocations(ATreeView: TTreeView): Boolean; override;
  end;

implementation

uses
  Dialogs,
  cDownloader;

const
  BASE_URL = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/';
  FILENAME_CONFIRMED = 'time_series_covid19_confirmed_global.csv';
  FILENAME_DEATHS = 'time_series_covid19_deaths_global.csv';
  FILENAME_RECOVERED = 'time_series_covid19_recovered_global.csv';

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
    DoStatusMsg(DOWNLOAD_FROM, BASE_URL + FILENAME_CONFIRMED + '...');
    if not DownloadFile(BASE_URL + FILENAME_CONFIRMED, stream) then
    begin
      DoStatusMsg(DOWNLOAD_ERR, '');
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOK], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(FCacheDir + FILENAME_CONFIRMED);

    stream.Position := 0;
    DoStatusMsg(DOWNLOAD_FROM, BASE_URL + FILENAME_DEATHS + '...');
    if not DownloadFile(BASE_URL + FILENAME_DEATHS, stream) then
    begin
      DoStatusMsg(DOWNLOAD_ERR, '');
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOk], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(FCacheDir + FILENAME_DEATHS);

    stream.Position := 0;
    DoStatusMsg(DOWNLOAD_FROM, BASE_URL + FILENAME_RECOVERED + '...');
    if not DownloadFile(BASE_URL + FILENAME_RECOVERED, stream) then
    begin
      DoStatusMsg(DOWNLOAD_ERR, '');
      MessageDlg(DOWNLOAD_ERR, mtError, [mbOk], 0);
      exit;
    end;
    stream.Position := 0;
    stream.SaveToFile(FCacheDir + FILENAME_RECOVERED);

  finally
    stream.Free;
  end;
end;

function TJohnsHopkinsDataSource.GetDataString(const ACountry, AState: String;
  ACaseType: TCaseType; var AHeader, ACounts: String): Boolean;
var
  L: TStrings;
  fn: String;
  i: integer;
  sa: TStringArray;
begin
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

function TJohnsHopkinsDataSource.LoadLocations(ATreeView: TTreeView): Boolean;
var
  fn: String;
  L: TStrings;
  node: TTreeNode;
  sa: TStringArray;
  i: Integer;
begin
  Result := false;

  fn := FCacheDir + FILENAME_CONFIRMED;
  if not FileExists(fn) then
    exit;

  L := TStringList.Create;
  try
    L.LoadfromFile(fn);
    for i:=1 to L.Count-1 do begin
      if L[i] = '' then
        Continue;
      if BeginsWithQuote(L[i]) then
        Continue;
      sa := L[i].Split(',', '"');
      if sa[0] <> '' then sa[0] := AnsiDequotedStr(sa[0], '"');
      if sa[1] <> '' then sa[1] := AnsiDequotedStr(sa[1], '"');
      node := ATreeView.Items.FindTopLvlNode(sa[1]);
      if node = nil then
        node := ATreeView.Items.AddChild(nil, sa[1]);
      if sa[0] <> '' then
        node := ATreeView.Items.AddChild(node, sa[0]);
    end;
  finally
    L.Free;
  end;

  Result := true;
end;


end.

