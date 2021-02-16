unit cGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPrimaryCaseType = (pctConfirmed, pctDeaths, pctRecovered);

  TCaseType = (ctConfirmed, ctDeaths, ctRecovered, ctSick);

  TDataType = (
    dtCumulative, dtNewCases,
    dtNormalizedCumulative, dtNormalizedNewcases,
    dtCumulativeCasesDoublingTime, dtNewCasesDoublingTime,
    dtCumVsNewCases, dtRValue
  );

//  TDataPointArray = array of TDoublePoint;

  TLocationParams = record
   {$IFDEF DEBUG_LOCATIONPARAMS}
    Name: String[16];
   {$ENDIF}
    ID: Integer;
    Population: Integer;
  end;
  PLocationParams = ^TLocationParams;

const
  CASETYPE_NAMES: array [TCaseType] of string = (
    'confirmed', 'deaths', 'recovered', 'sick'
  );

  R_NUMBER_STR = 'R number';

  // Duration (in days) during which an infected person is infectious.
  INFECTIOUS_PERIOD = 5;  // https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Steckbrief.html#doc13776792bodyText5

  // Days used for moving-average-calculation
  ACCUMULATION_RANGE = 7;

  // Reference population for calculation of incidence value
  REF_POPULATION = 100*1000;

  // Format for saving date
  SAVE_DATE_FORMAT = 'yyyy-mm-dd';


var
  InfectiousPeriod: Integer = INFECTIOUS_PERIOD;
  SmoothingRange: Integer = ACCUMULATION_RANGE;
  RRange: Integer = (ACCUMULATION_RANGE - 1) div 2;           // +/- from center
  PopulationRef: Integer = 100000;         // population count for normalization

  cFormatSettings: TFormatSettings;

implementation

initialization
  cFormatSettings := DefaultFormatSettings;
  cFormatSettings.DecimalSeparator := '.';
  cFormatSettings.ShortDateFormat := 'mm/dd/yyyy';
  cformatSettings.DateSeparator := '/';

end.

