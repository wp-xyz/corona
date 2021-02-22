unit cPalette;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics;

type
  TPaletteItem = record
    Value: Double;
    Color: TColor;    // to be used when current value is > Value
  end;

  TPalette = record
    Items: array of TPaletteItem;
    BaseColor: TColor;
    Multiplier: Double;
    function GetColor(AValue: Double): TColor;
    procedure Init(ABaseColor: TColor; const AItems: Array of TPaletteItem;
      AMultiplier: Double);
  end;

const
  // To be used for normalized new cases
  INCIDENCE_PALETTE_ITEMS: array[0..10] of TPaletteItem = (
    (Value:   1; Color: $D9F0E2),    // > 1
    (Value:  10; Color: $B4D0C5),    // > 10
    (Value:  20; Color: $8ED1A9),
    (Value:  35; Color: $47AD70),
    (Value:  50; Color: $66D9FF),
    (Value: 100; Color: $83B1F4),
    (Value: 200; Color: $317DED),
    (Value: 300; Color: $115AC5),
    (Value: 500; Color: $0C3C84),
    (Value:1000; Color: $643820),
    (Value:2000; Color: $0C0C0C)
  );
  // To be used for R value mapping
  RVALUE_PALETTE_ITEMS: array[0..8] of TPaletteItem = (
    (Value: 0.01; Color: $358254),
    (Value: 0.2;  Color: $8ED1A9),
    (Value: 0.5;  Color: $D9F0E2),
    (Value: 0.9;  Color: $CCF2FF),
    (Value: 1.1;  Color: $66D9FF),
    (Value: 1.5;  Color: $83B1F4),
    (Value: 2.0;  Color: $115AC5),
    (Value: 3.0;  Color: $0C3C84),
    (Value: 5.0;  Color: $0000FF)
  );


implementation

uses
  Math;

procedure TPalette.Init(ABaseColor: TColor; const AItems: Array of TPaletteItem;
  AMultiplier: Double);
var
  i: Integer;
begin
  BaseColor := ABaseColor;
  Multiplier := AMultiplier;
  SetLength(Items, Length(AItems));
  for i := 0 to High(AItems) do
    Items[i] := AItems[i];
end;

function TPalette.GetColor(AValue: Double): TColor;
var
  i: Integer;
  item: TPaletteItem;
begin
  Result := BaseColor;
  if IsNaN(AValue) then
    exit;
  AValue := AValue / Multiplier;
  for i := High(Items) downto Low(Items) do
    if AValue > Items[i].Value then
    begin
      Result := Items[i].Color;
      exit;
    end;
end;


end.

