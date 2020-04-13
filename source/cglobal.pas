unit cGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCaseType = (ctConfirmed, ctDeaths, ctRecovered);
  TDataType = (dtCumulative, dtNewCases, dtDoublingTime, dtCumVsNewCases);

const
  INFECTIOUS_PERIOD = 6;  // https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Steckbrief.html#doc13776792bodyText5

implementation

end.

