unit cUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Split(AString: String; ADest: TStrings; ADelimiter: Char = ',');
function UnQuote(const s: String): String;

implementation

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

