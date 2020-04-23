unit cGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCaseType = (ctConfirmed, ctDeaths, ctRecovered, ctSick);
  TDataType = (dtCumulative, dtNewCases, dtDoublingTime, dtCumVsNewCases, dtRValue);

const
  CASETYPE_NAMES: array [TCaseType] of string = (
    'confirmed', 'deaths', 'recovered', 'sick'
  );

  // Duration (in days) during which an infected person is infectious.
  INFECTIOUS_PERIOD = 6;  // https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Steckbrief.html#doc13776792bodyText5

  // Days used for moving-average-calculation
  ACCUMULATION_RANGE = 7;

  // maximum reproduction number allowed in charts
  MAX_R = 8;

var
  InfectiousPeriod: Integer = INFECTIOUS_PERIOD;
  SmoothingRange: Integer = ACCUMULATION_RANGE;
  RRange: Integer = (ACCUMULATION_RANGE - 1) div 2;   // +/- from center

implementation

end.

