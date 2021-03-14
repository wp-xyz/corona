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
    dtNormalizedCumulative, dtNormalizedNewCases,
    dtCumulativeCasesDoublingTime, dtNewCasesDoublingTime,
    dtCumVsNewCases, dtRValue
  );

  TMapDataType = (
    mdtNormalizedNewConfirmed, mdtNormalizedNewDeaths,
    mdtRValue
  );

  TGeoID = Int64;

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
  LONG_CASETYPE_NAMES: array [TCaseType] of string = (
    'confirmed cases', 'deaths', 'recovered cases', 'sick cases'
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

  // Application title
  APP_TITLE = 'corona';


var
  InfectiousPeriod: Integer = INFECTIOUS_PERIOD;
  SmoothingRange: Integer = ACCUMULATION_RANGE;
  RRange: Integer = (ACCUMULATION_RANGE - 1) div 2;           // +/- from center
  PopulationRef: Integer = 100000;         // population count for normalization

  // Resource names of the maps used
  WorldMapResName: String = 'WORLD_MAP';
  USStatesMapResName: String = 'US_STATES_MAP';
  USCountiesMapResName: String = 'US_COUNTIES_MAP';
  ChinaMapResName: String = 'CHINA_MAP';
  CanadaMapResName: String = 'CANADA_MAP';
  AustraliaMapResName: String = 'AUSTRALIA_MAP';
  EuropeMapResName: String = 'EUROPE_MAP';
  AfricaMapResName: String = 'AFRICA_MAP';
  AmericasMapResName: String = 'AMERICAS_MAP';
  AsiaMapResName: String = 'ASIA_MAP';

  PortableInstallation: Boolean = {$IFDEF PORTABLE}true{$ELSE}false{$ENDIF};
  DataSymbolDistance: Integer = 16;

  cFormatSettings: TFormatSettings;


implementation

initialization
  cFormatSettings := DefaultFormatSettings;
  cFormatSettings.DecimalSeparator := '.';
  cFormatSettings.ShortDateFormat := 'mm/dd/yyyy';
  cformatSettings.DateSeparator := '/';

end.

