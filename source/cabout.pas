unit cAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    BitBtn1: TBitBtn;
    AppImage: TImage;
    imgRKI: TImage;
    imgJHU: TImage;
    lblCountryContinentsList: TLabel;
    lblUSMaps: TLabel;
    lblChinaMap: TLabel;
    lblCanadaMap: TLabel;
    lblWorldMap: TLabel;
    lblAustraliaMap: TLabel;
    lblVersion: TLabel;
    lblGeneralInfo: TLabel;
    lblDataSources: TLabel;
    lblAcknowledgements: TLabel;
    lblCompiler: TLabel;
    lblFPC: TLabel;
    lblIcons: TLabel;
    lblAppIcon: TLabel;
    lblIcons8: TLabel;
    lblRolandHahn: TLabel;
    lblJHUgit: TLabel;
    lblIDE: TLabel;
    lblNPGeo: TLabel;
    lblLazarus: TLabel;
    lblTitle: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLPlatformDef, Types, IntfGraphics, LazCanvas, FPCanvas, InterfaceBase,
  {$IFDEF MSWINDOWS}
  win32Proc,
  {$ENDIF}
  cUtils;

const
  URL_FPC = 'https://www.freepascal.org/';
  URL_Lazarus = 'https://www.lazarus-ide.org/';
  URL_Icons8 = 'http://www.icons8.com';
  URL_JHU = 'https://systems.jhu.edu/';
  URL_JHU_git = 'https://github.com/CSSEGISandData/COVID-19/';
  URL_RKI = 'https://www.rki.de/EN/Home/homepage_node.html';
  URL_NPGeo = 'https://npgeo-corona-npgeo-de.hub.arcgis.com/';
  URL_World_Map = 'http://thematicmapping.org/downloads/world_borders.php';
  URL_US_Maps = 'https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html';
  URL_Canada_Map = 'https://open.canada.ca/data/en/dataset/bab06e04-e6d0-41f1-a595-6cff4d71bedf';
  URL_China_Map = 'https://geodata.lib.utexas.edu/catalog/stanford-bw669kf8724';
  URL_Australia_Map = 'https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.001July%202011?OpenDocument';
  URL_CountryContinent_List = 'https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/r/0.html';

// https://forum.lazarus.freepascal.org/index.php/topic,15390.msg82563.html#msg82563
function GetOSVersion: String;
begin
  Result := 'Unknown';

 {$IFDEF MSWINDOWS}
  case WindowsVersion of
    wv95: Result := ' 95';
    wvNT4: Result := ' NT v.4';
    wv98: Result := ' 98';
    wvMe: Result := ' ME';
    wv2000: Result := ' 2000';
    wvXP: Result := ' XP';
    wvServer2003: Result := ' Server 2003';
    wvVista: Result := ' Vista';
    wv7: Result := ' 7';
    wv8: Result := ' 8';
    wv8_1: Result := ' 8.1';
    wv10: Result := ' 10';
    else Result:= '';
  end;
  Result := 'Windows' + Result;
 {$ENDIF}

 {$IFDEF UNIX}
  Result := 'Unix ';
 {$ENDIF}

  {$IFDEF LCLcarbon}
  Result := 'Mac OS X';
 {$ENDIF}

 {$IFDEF LCLcocoa}
  Result := 'macOS';
 {$ENDIF}

 {$IFDEF Linux}
  Result := 'Linux';
 {$ENDIF}
end;

procedure ScaleBitmap(BM: TBitmap; W, H: Integer);
var
  Source, Dest: TLazIntfImage;
  DestCanvas: TLazCanvas;
begin
  try
    Source := TLazIntfImage.Create(0, 0);
    Source.LoadFromBitmap(BM.Handle, 0);
    Dest := TLazIntfImage.Create(0, 0);
    Dest.LoadFromBitmap(BM.Handle, 0);
    DestCanvas := TLazCanvas.Create(Dest);
    DestCanvas.Interpolation := TFPBaseInterpolation.Create;
    DestCanvas.StretchDraw(0, 0, W, H, Source);
    BM.LoadFromIntfImage(Dest);
    BM.SetSize(W, H);
  finally
    DestCanvas.Interpolation.Free;
    DestCanvas.Free;
    Dest.Free;
    Source.Free;
  end;
end;


{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  with AppImage do
  begin
    Picture.Assign(Application.Icon);
    Picture.Icon.Current := Picture.Icon.GetBestIndexForSize(Size(256, 256));
  end;

  lblVersion.Caption := GetVersionStr;
  lblGeneralInfo.Caption := Format(
    'Operating system: %s' + LineEnding +
    'Target CPU: %s' + LineEnding +
    'Target OS: %s' + LineEnding +
    'Platform: %s', [
    GetOSVersion,
    lowerCase({$I %FPCTARGETCPU%}),
    lowerCase({$I %FPCTARGETOS%}),
    LCLPlatformDisplayNames[GetDefaultLCLWidgetType]
  ]);

  lblFPC.Hint := URL_FPC;
  lblLazarus.Hint := URL_Lazarus;
  lblIcons8.Hint := URL_Icons8;
  imgJHU.Hint := URL_JHU;
  lblJHUgit.Hint := URL_JHU_git;
  imgRKI.Hint := URL_RKI;
  lblNPGeo.Hint := URL_NPGeo;

  lblWorldMap.Hint := URL_World_Map;
  lblAustraliaMap.Hint := URL_Australia_Map;
  lblCanadaMap.Hint := URL_Canada_Map;
  lblChinaMap.Hint := URL_China_Map;
  lblUSMaps.Hint := URL_US_Maps;
  lblCountryContinentsList.Hint := URL_CountryContinent_List;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  ScaleBitmap(AppImage.Picture.Bitmap, AppImage.Width, AppImage.Height);
  ScaleBitmap(imgJHU.Picture.Bitmap, imgJHU.Width, imgJHU.Height);
  ScaleBitmap(imgRKI.Picture.Bitmap, imgRKI.Width, imgRKI.Height);
end;

procedure TAboutForm.LabelClick(Sender: TObject);
begin
  if Sender is TLabel then
    OpenURL(TLabel(Sender).Hint);
  {
  if Sender = lblFPC then
    OpenURL(URL_FPC)
  else if Sender = lblLazarus then
    OpenURL(URL_Lazarus)
  else if Sender = lblIcons8 then
    OpenURL(URL_Icons8)
  else if (Sender = imgJHU) then
    OpenURL(URL_JHU)
  else if (Sender = lblJHUgit) then
    OpenURL(URL_JHU_git)
  else if (Sender = imgRKI) then
    OpenURL(URL_RKI)
  else if (Sender = lblNPGeo) then
    OpenURL(URL_NPGeo)
  else if
  }
end;

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style + [fsUnderline];
end;

procedure TAboutForm.LabelMouseLeave(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style - [fsUnderline];
end;

end.

