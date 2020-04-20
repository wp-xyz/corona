unit cGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCaseType = (ctConfirmed, ctDeaths, ctRecovered, ctSick);
  TDataType = (dtCumulative, dtNewCases, dtDoublingTime, dtCumVsNewCases);

const
  CASETYPE_NAMES: array [TCaseType] of string = (
    'confirmed', 'deaths', 'recovered', 'sick'
  );

  // Duration (in days) during which an infected person is infectious.
  INFECTIOUS_PERIOD = 6;  // https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Steckbrief.html#doc13776792bodyText5

  // Days used for moving-average-calculation
  ACCUMULATION_RANGE = 7;

implementation

end.

