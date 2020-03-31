unit cDownloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpopenssl, ssockets, sslsockets;   // needed for SSL to work in general

function DownloadFile(const URL: String; AStream: TStream): Boolean;

implementation

uses
  OpenSSL,
 {$IF FPC_FullVersion >= 30200}        // needed for SSL to work in general
  sslbase, OpenSSLSockets, FpHttpClient
 {$ELSE}
  chttpclient
 {$ENDIF}
  ;

type
  { TDownloader }
  TDownloader = class
  private
    procedure GetSocketHandler(Sender: TObject; const UseSSL: Boolean;
      out AHandler: TSocketHandler);
  public
    function Get(const URL: String; AStream: TStream): Boolean;
  end;


function DownloadFile(const URL: String; AStream: TStream): Boolean;
var
  D: TDownloader;
begin
  with TDownloader.Create do
    try
      Result := Get(URL, AStream);
    finally
      Free;
    end;
end;

function TDownloader.Get(const URL: String; AStream: TStream): Boolean;
var
  http: TFpHttpClient;
begin
  Result := true;
  InitSSLInterface;
  http := TFpHttpClient.Create(nil);
  try
    http.OnGetSocketHandler := @GetSocketHandler;
    http.AllowRedirect := true;
    http.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
    try
      http.Get(Url, AStream);
      Result := (http.ResponseStatusCode = 200);
    except
      on EInOutError do raise;
      on EHTTPClient do Result := false;
    end;
    AStream.Position := 0;
  finally
    http.Free;
  end;
end;

procedure TDownloader.GetSocketHandler(Sender: TObject; const UseSSL: Boolean;
  out AHandler: TSocketHandler);
begin
  {$IF FPC_FullVersion >= 30200}        // needed for SSL to work in general
  AHandler := TOpenSSLSocketHandler.Create;
  TOpenSSLSocketHandler(AHandler).SSLType := stTLSv1_2;
  {$ELSE}
  AHandler := TSSLSocketHandler.Create;
  TSSLSocketHandler(AHandler).SSLType := stTLSv1_2;
  {$ENDIF}
end;


end.

