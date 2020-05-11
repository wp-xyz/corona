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
  LCLIntf, Types;

const
  URL_FPC = 'https://www.freepascal.org/';
  URL_Lazarus = 'https://www.lazarus-ide.org/';
  URL_Icons8 = 'http://www.icons8.com';
  URL_JHU = 'https://systems.jhu.edu/';
  URL_JHU_git = 'https://github.com/CSSEGISandData/COVID-19/';
  URL_RKI = 'https://www.rki.de/EN/Home/homepage_node.html';
  URL_NPGeo = 'https://npgeo-corona-npgeo-de.hub.arcgis.com/';


{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  with AppImage do
  begin
    Picture.Assign(Application.Icon);
    Picture.Icon.Current := Picture.Icon.GetBestIndexForSize(Size(256, 256));
  end;

  lblFPC.Hint := URL_FPC;
  lblLazarus.Hint := URL_Lazarus;
  lblIcons8.Hint := URL_Icons8;
  imgJHU.Hint := URL_JHU;
  lblJHUgit.Hint := URL_JHU_git;
  imgRKI.Hint := URL_RKI;
  lblNPGeo.Hint := URL_NPGeo
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

