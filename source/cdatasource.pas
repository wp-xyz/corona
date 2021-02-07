unit cDataSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  cGlobal;

type
  TStatusbarEvent = procedure (Sender: TObject; const AMsg1, AMsg2: String) of object;
  TDownloadEvent = procedure (Sender: TObject; const AMsg1, AMsg2: string; APercentage: Integer) of object;

  TcDataSource = class
  private
    FOnDownloadMsg: TDownloadEvent;
    FOnStatusMsg: TStatusbarEvent;
  protected
    FCacheDir: String;
    procedure DoDownloadMsg(const AMsg1, AMsg2: String; APercentage: Integer);
    procedure DoStatusMsg(const AMsg1, AMsg2: String);
  public
    constructor Create(ACacheDir: String); virtual;

    // Downloads the data files from the primary online site to a local cache.
    procedure DownloadToCache; virtual; abstract;

    { Extracts the line with the data value from the cache file associated with
      the clicked tree node }
    function GetDataString(const ACountry, AState, ACity: String; ACaseType: TCaseType;
      out AHeader, ACounts: String): Boolean; virtual; abstract;

    { Loads the locations from the specified cache directory into a treeview.
      Clearing, Begin/EndUpdate is done by the calling routine. }
    function LoadLocations(ATreeView: TTreeView): Boolean; virtual; abstract;

    property OnDownloadMsg: TDownloadEvent read FOnDownloadMsg write FOnDownloadMsg;
    property OnStatusMsg: TStatusbarEvent read FOnStatusMsg write FOnStatusMsg;

  end;

  TcDataSourceClass = class of TcDataSource;


implementation

uses
  LazFileUtils;

constructor TcDataSource.Create(ACacheDir: String);
begin
  FCacheDir := AppendPathDelim(ACacheDir);
end;

procedure TcDataSource.DoDownloadMsg(const AMsg1, AMsg2: String;
  APercentage: Integer);
begin
  if Assigned(FOnDownloadMsg) then
    FOnDownloadMsg(Self, AMsg1, AMsg2, APercentage);
end;

procedure TcDataSource.DoStatusMsg(const AMsg1, AMsg2: String);
begin
  if Assigned(FOnStatusMsg) then
    FOnStatusMsg(Self, AMsg1, AMsg2);
end;

end.

