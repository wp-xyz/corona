unit cUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function UnQuote(const s: String): String;

implementation

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

