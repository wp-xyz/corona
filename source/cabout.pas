unit cAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    AppImage: TImage;
    imgRKI: TImage;
    imgJHU: TImage;
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
  LCLIntf, Types, IntfGraphics, LazCanvas, FPCanvas, InterfaceBase,
  cUtils;

const
  URL_FPC = 'https://www.freepascal.org/';
  URL_Lazarus = 'https://www.lazarus-ide.org/';
  URL_Icons8 = 'http://www.icons8.com';
  URL_JHU = 'https://systems.jhu.edu/';
  URL_JHU_git = 'https://github.com/CSSEGISandData/COVID-19/';
  URL_RKI = 'https://www.rki.de/EN/Home/homepage_node.html';
  URL_NPGeo = 'https://npgeo-corona-npgeo-de.hub.arcgis.com/';

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
    'Widgetset: %s', [
    {$I %FPCTARGETOS%},
    GetLCLWidgetTypeName
  ]);

  lblFPC.Hint := URL_FPC;
  lblLazarus.Hint := URL_Lazarus;
  lblIcons8.Hint := URL_Icons8;
  imgJHU.Hint := URL_JHU;
  lblJHUgit.Hint := URL_JHU_git;
  imgRKI.Hint := URL_RKI;
  lblNPGeo.Hint := URL_NPGeo
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  ScaleBitmap(AppImage.Picture.Bitmap, AppImage.Width, AppImage.Height);
  ScaleBitmap(imgJHU.Picture.Bitmap, imgJHU.Width, imgJHU.Height);
  ScaleBitmap(imgRKI.Picture.Bitmap, imgRKI.Width, imgRKI.Height);
end;

procedure TAboutForm.LabelClick(Sender: TObject);
begin
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
    OpenURL(URL_NPGeo);
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

