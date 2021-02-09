unit cUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

function FormatBytes(AValue: Int64): String;

function MinimizeName(FileName: String; Canvas: TCanvas; MaxWidth: Integer;
  APathDelim: Char): String;

procedure Split(AString: String; ADest: TStrings; ADelimiter: Char = ',');

function UnQuote(const s: String): String;


implementation

{ Creates a nicely formatted string for a bytes value.

  Copyright (c) 2021 Gustavo Carreno <guscarreno@gmail.com>
  MIT License }
function FormatBytes(AValue: Int64): String;
var
  dSize: Double;
begin
  Result := '';
  dSize := 0.0;
  if AValue < 1024 then
  begin
    Result := IntToStr(AValue) + ' B';
    exit;
  end;
  if AValue < (1024*1024) then
  begin
    dSize := AValue / 1024;
    Result := FormatFloat('0.##', dSize) + ' KB';
    exit;
  end;
  if AValue < (1024*1024*1024) then
  begin
    dSize := AValue / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + ' MB';
    exit;
  end;
  if AValue < (1024*1024*1024*1024) then
  begin
    dSize := AValue / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + 'GB';
    exit;
  end;
  if AValue < (1024*1024*1024*1024*1024) then
  begin
    dSize := AValue / 1024 / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + ' TB';
  end;
end;

{
 This function will return a shortened version of FileName, so that it fits
 on the given Canvas, with a given MaxWidth.
 eg. C:\Documents and Settings\User\Application Data\Microsoft\Word\custom.dic
     would become something like: C:\...\Word\custom.dic

 Borrowed from FileCtrl to specify the path delimiter.
}
function MinimizeName(FileName: String; Canvas: TCanvas; MaxWidth: Integer;
  APathDelim: Char): String;

  procedure RemoveFirstDir(var Dir: String);
  {
   This procedure will remove the first directory from Dir
   and will set ADelim to the Delimiter that separated the first Dir
   eg. In: Dir: 'Dir1\Dir2\Dir3'
  }
  var p: Integer;
  begin
    p:= Pos(APathDelim,Dir);
    if (p > 0) then
    begin
      Dir := Copy(Dir,p+1,Length(Dir)-p);
    end;
  end;

var Drive, Dir, Fn: String;
    ComposedName: String;
    TWidth: Integer;
begin
  Result := FileName;
  //if FileName does not contain any (sub)dir then return FileName
  if Pos(APathDelim, FileName) = 0 then Exit;
  //if FileName fits, no need to do anyhing
  if Canvas.TextWidth(FileName) <= MaxWidth then Exit;
  Drive := ExtractFileDrive(FileName);
  Fn := ExtractFileName(FileName);
  Dir := ExtractFilePath(FileName);
  //Remove Drive from Dir
  if (Length(Drive) > 0) then System.Delete(Dir, 1, Length(Drive));
  //Transfer all PathDelimiters at the start of Dir to Drive
  While (Length(Dir) > 0) and (Dir[1] in ['/','\']) do
  begin
    Drive := Drive + Dir[1];
    System.Delete(Dir,1,1);
  end;
  //if Dir is empty then we cannot shorten it,
  //and we know at this point that Drive+FileName is too long, so we return only filename
  if (Length(Dir) = 0) then
  begin
    Result := Fn;
    Exit;
  end;
  repeat
    //at this point we know that Dir ends with PathDelim (otherwise we exited before this point,
    //so RemoveFirstDir will return a truncated Dir or an empty string
    RemoveFirstDir(Dir);
    ComposedName := Drive+'...'+APathDelim+Dir+Fn;
    TWidth := Canvas.TextWidth(ComposedName);
  until (Length(Dir) = 0) or (TWidth <= MaxWidth);
  if (TWidth <= MaxWidth) then Result := ComposedName else Result := Fn;
end;

// Splits the string at the delimiter. Does not split quoted.
procedure Split(AString: String; ADest: TStrings; ADelimiter: Char = ',');
var
  P, P1, P2: PChar;
  PLast: PChar;
  s: String;
begin
  ADest.Clear;
  if AString = '' then
    exit;
  P := @AString[1];
  P1 := P;
  PLast := @AString[Length(AString)];
  while P <= PLast do begin
    if P^ = '"' then
    begin
      inc(P);
      while (P < PLast) and (P^ <> '"') do begin
        inc(P);
      end
    end else
    if P^ = ADelimiter then
    begin
      if P = P1 then
        s := ''
      else
      begin
        SetLength(s, PtrUInt(P) - PtrUInt(P1));
        Move(P1^, s[1], Length(s));
      end;
      ADest.Add(s);
      P1 := P + 1;
    end;
    inc(P);
  end;
end;

// Replaces AnsiDeQuoteStr because it crashes in FPC 3.04 when the input string
// is empty. And this version is a bit faster...
function UnQuote(const s: String): String;
var
  i1, i2, L: Integer;
begin
  if s = '' then exit('');
  if s[1] = '"' then i1 := 2 else i1 := 1;
  L := Length(s);
  if s[L] = '"' then i2 := L - 1 else i2 := L;
  SetLength(Result, i2 - i1 + 1);
  Move(s[i1], Result[1], Length(Result));
end;

end.

