{ Code kindly provided by
    (c) 2021 Gustavo Carreno <guscarreno@gmail.com>

  Minor changes by Werner Pamler
}

unit cDownloadManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls
{$IF FPC_FULLVERSION < 30200}
  , ssockets, sslsockets
{$ENDIF}
;

type
  { TShowStatusEvent }
  TShowStatusEvent = procedure(const ALen, APos: Int64) of object;

  { TDownloadThread }
  TDownloadThread = class(TThread)
  private
    FSize: Int64;
    FPos: Int64;
    FURL: String;
    FFilename: String;
    FStream: TStream;
    FOnShowStatus: TShowStatusEvent;
{$IF FPC_FULLVERSION < 30200}
    procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean;
      out AHandler: TSocketHandler);
{$ENDIF}
    procedure DataReceived(Sender : TObject; const {%H-}ContentLength, CurrentPos : Int64);
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;
    property URL: String read FURL write FURL;
    property Filename: String read FFilename write FFilename;
    property Stream: TStream read FStream write FStream;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

  { TDownload record }
  TDownload = record
    URL: String;
    FileName: String;
    Stream: TStream;
  end;

  { TDownloadForm }
  TDownloadForm = class(TForm)
    lblURL: TLabel;
    lblBytes: TLabel;
    lblDownloads: TLabel;
    lblFileName: TLabel;
    pbDownloads: TProgressBar;
    pbBytes: TProgressBar;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FDownloads: array of TDownload;
    FFinished: Boolean;
    FErrMsg: String;
  public
    procedure AddDownload(const AURL, AFilename: String; AStream: TStream = nil);
    procedure ShowStatus(const ALen, APos: Int64);
    property ErrorMsg: String read FErrMsg;
  end;

var
  DownloadForm: TDownloadForm;

function DownloadToStream(const URL, AFileName: String; AStream: TStream): Boolean;


implementation

{$R *.lfm}

uses
  fphttpclient,
  {$IF FPC_FULLVERSION >= 30200}
  opensslsockets,
  {$ELSE}
  fpopenssl,
  openssl,
  {$ENDIF}
  cUtils;

function DownloadToStream(const URL, AFileName: String; AStream: TStream): Boolean;
var
  F: TDownloadForm;
begin
  Result := false;
  F := TDownloadForm.Create(nil);
  try
    F.AddDownload(URL, AFileName, AStream);
    F.ShowModal;
    if F.ErrorMsg <> '' then
    begin
      MessageDlg(F.ErrorMsg, mtError, [mbOK], 0);
      exit;
    end;
    Result := true;
  finally
    F.Free;
  end;
end;


{ TDownloadThread }

constructor TDownloadThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TDownloadThread.Destroy;
begin
  inherited Destroy;
end;

{$IF FPC_FULLVERSION < 30200}
procedure TDownloadThread.GetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  AHandler := TSSLSocketHandler.Create;
  TSSLSocketHandler(AHandler).SSLType := stTLSv1_2;
end;
{$ENDIF}

procedure TDownloadThread.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  FPos:= CurrentPos;
  Synchronize(@ShowStatus);
end;

procedure TDownloadThread.ShowStatus;
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(FSize, FPos);
  end;
end;

procedure TDownloadThread.Execute;
var
  http: TFPHTTPClient;
  index: Integer;
begin
{$IF FPC_FULLVERSION < 30200}
  InitSSLInterface;
{$ENDIF}
  http:= TFPHTTPClient.Create(nil);
{$IF FPC_FULLVERSION < 30200}
  http.OnGetSocketHandler:=@GetSocketHandler;
{$ENDIF}
  http.AllowRedirect:= True;
  try
    try
      http.HTTPMethod('HEAD', FURL, nil, []);
      FSize := 0;
      for index := 0 to Pred(http.ResponseHeaders.Count) do
      begin
        if LowerCase(http.ResponseHeaders.Names[index]) = 'content-length' then
        begin
          FSize:= StrToInt64(http.ResponseHeaders.ValueFromIndex[index]);
        end;
      end;
      http.OnDataReceived:= @DataReceived;
      if FStream <> nil then
        http.Get(FURL, FStream)
      else
        http.Get(FURL, FFileName);
    except
      on E: Exception do
      begin
        if http.ResponseStatusCode > 399 then
        begin
          //Log(Format('Status: %d', [http.ResponseStatusCode]));
        end;
        //Log('Error: ' + E.Message);
      end;
    end;
  finally
    http.Free;
  end;
end;


{ TDownloadForm }

{ When AFilename is given AND AStream then the url id downloaded into the stream
  but the filename is displayed in the progress bar. This is for RKI download
  where data is needed in a memory stream and will later be saved to the file. }
procedure TDownloadForm.AddDownload(const AURL, AFilename: String;
  AStream: TStream = nil);
var
  len: Integer;
begin
  { #todo 1 -ogcarreno : Maybe test for duplicates? }
  len := Length(FDownloads);
  SetLength(FDownloads, len + 1);
  FDownloads[len].URL:= AURL;
  FDownloads[len].Filename:= AFilename;
  FDownloads[len].Stream := AStream;
end;

procedure TDownloadForm.FormActivate(Sender: TObject);
var
  index: Integer;
  dlThread: TDownloadThread;
  url: String;
  fn: String;
begin
  FErrMsg := '';

  Height := pbBytes.Top + pbBytes.Height + pbBytes.BorderSpacing.Bottom;

  Application.ProcessMessages;

  pbDownloads.Max := Length(FDownloads);
  for index:= 0 to High(FDownloads) do
  begin
    url := MinimizeName(FDownloads[index].URL, lblURL.Canvas, lblURL.Width - 4, '/');
    fn := MinimizeName(FDownloads[index].FileName, lblFileName.Canvas, lblFileName.Width - 4, PathDelim);
    lblURL.Caption:= Format('URL: %s', [url]);
    lblFileName.Caption:= Format('File: %s', [fn]);
    lblDownloads.Caption:= Format('%d of %d', [index + 1, Length(FDownloads)]);
    Application.ProcessMessages;
    try
      //DoDownload(index);
      dlThread:= TDownloadThread.Create(True);
      dlThread.OnShowStatus:= @ShowStatus;
      dlThread.URL:= FDownloads[index].URL;
      dlThread.Filename:= FDownloads[index].Filename;
      dlThread.Stream := FDownloads[index].Stream;
      dlThread.Start;
      dlThread.WaitFor;
    except
      on E: Exception do
      begin
        FErrMsg := E.Message;
        break;
      end;
    end;
    pbDownloads.Position:= index + 1;
    Application.ProcessMessages;
  end;
  FFinished := True;
  Close;
end;

procedure TDownloadForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDownloadForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FFinished;
end;

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  FFinished := false;
end;

procedure TDownloadForm.ShowStatus(const ALen, APos: Int64);
var
  currentPercent: Double;
begin
  if ALen = 0 then
    exit;
  currentPercent:= (APos*100)/ALen;
  pbBytes.Position:= round(currentPercent);
  lblBytes.Caption:= Format('%s of %s', [FormatBytes(APos), FormatBytes(ALen)]);
  Application.ProcessMessages;
end;

end.

