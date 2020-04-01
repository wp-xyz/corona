unit cRobertKochInstitut;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  cGlobal, cDataSource;

type
  TRobertKochDatasource = class(TcDataSource)
  private
  protected
  public
    procedure DownloadToCache; override;
    function GetDataString(const ACountry, AState: String; ACaseType: TCaseType;
      var AHeader, ACounts: String): Boolean; override;
    function LoadLocations(ATreeView: TTreeView): Boolean; override;
  end;

implementation

const
  Bundesland: array[1..16] of string = (
    'Schleswig-Holstein',      // 1
    'Hamburg',                 // 2
    'Niedersachsen',           // 3
    'Bremem',                  // 4
    'Nordrhein-Westfalen',     // 5
    'Hessen',                  // 6
    'Rheinland-Pfalz',         // 7
    'Baden-Württemberg',       // 8
    'Bayern',                  // 9
    'Saarland',                // 10
    'Berlin',                  // 11
    'Brandenburg',             // 12
    'Mecklenburg-Vorpommern',  // 13
    'Sachsen',                  // 14
    'Sachsen-Anhalt',          // 15
    'Thüringen'                // 16
  );

  const Landkreis: array[0..411] of string = (
    // 0..9
    '01001=Flensburg',                 '01002=Kiel',
    '01003=Lübeck',                    '01004=Neumünster',
    '01051=Dithmarschen',              '01053=Herzogtum Lauenburg',
    '01054=Nordfriesland',             '01055=Ostholstein',
    '01056=Pinneberg',                 '01057=Plön',
     // 10..19
    '01058=Rendsburg-Eckernförde',    '01059=Schleswig-Flensburg',
    '01060=Segeberg',                 '01061=Steinburg',
    '01062=Stormarn',                 '02000=Hamburg',
    '03101=Braunschweig',             '03102=Salzgitter',
    '03103=Wolfsburg',                '03151=Gifhorn',
    //20..29
    '03153=Goslar',                   '03154=Helmstedt',
    '03155=Northeim',                 '03157=Peine',
    '03158=Wolfenbüttel',             '03159=Göttingen',
    '03241=Region Hannover',          '03251=Diepholz',
    '03252=Hameln-Pyrmont',           '03254=Hildesheim',
    // 30..39
    '03255=Holzminden',               '03256=Nienburg (Weser)',
    '03257=Schaumburg',               '03351=Celle',
    '03352=Cuxhaven',                 '03353=Harburg',
    '03354=Lüchow-Dannenberg',        '03355=Lüneburg',
    '03356=Osterholz',                '03357=Rotenburg (Wümme)',
    // 40..49
    '03358=Heidekreis',               '03359=Stade',
    '03360=Uelzen',                   '03361=Verden',
    '03401=Delmenhorst',              '03402=Emden',
    '03403=Oldenburg (Oldb)',         '03404=Osnabrück',
    '03405=Wilhelmshaven',            '03451=Ammerland',
    // 50..59
    '03452=Aurich',                   '03453=Cloppenburg',
    '03454=Emsland',                  '03455=Friesland',
    '03456=Grafschaft Bentheim',      '03457=Leer',
    '03458=Oldenburg',                '03459=Osnabrück',
    '03460=Vechta',                   '03461=Wesermarsch',
    // 60..69
    '03462=Wittmund',                 '04011=Bremen',
    '04012=Bremerhaven',              '05111=Düsseldorf',
    '05112=Duisburg',                 '05113=Essen',
    '05114=Krefeld',                  '05116=Mönchengladbach',
    '05117=Mülheim an der Ruhr',      '05119=Oberhausen',
    // 70..79
    '05120=Remscheid',                '05122=Solingen',
    '05124=Wuppertal',                '05154=Kleve',
    '05158=Mettmann',                 '05162=Rhein-Kreis Neuss',
    '05166=Viersen',                  '05170=Wesel',
    '05314=Bonn',                     '05315=Köln',
    // 80..89
    '05316=Leverkusen',               '05334=Städteregion Aachen',
    '05358=Düren',                    '05362=Rhein-Erft-Kreis',
    '05366=Euskirchen',               '05370=Heinsberg',
    '05374=Oberbergischer Kreis',     '05378=Rheinisch-Bergischer Kreis',
    '05382=Rhein-Sieg-Kreis',         '05512=Bottrop',
    // 90..99
    '05513=Gelsenkirchen',            '05515=Münster',
    '05554=Borken',                   '05558=Coesfeld',
    '05562=Recklinghausen',           '05566=Steinfurt',
    '05570=Warendorf',                '05711=Bielefeld',
    '05754=Gütersloh',                '05758=Herford',
    // 100..109
    '05762=Höxter',                   '05766=Lippe',
    '05770=Minden-Lübbecke',          '05774=Paderborn',
    '05911=Bochum',                   '05913=Dortmund',
    '05914=Hagen',                    '05915=Hamm',
    '05916=Herne',                    '05954=Ennepe-Ruhr-Kreis',
    // 110..119
    '05958=Hochsauerlandkreis',       '05962=Märkischer Kreis',
    '05966=Olpe',                     '05970=Siegen-Wittgenstein',
    '05974=Soest',                    '05978=Unna',
    '06411=Darmstadt',                '06412=Frankfurt am Main',
    '06413=Offenbach am Main',        '06414=Wiesbaden',
    // 120..129
    '06431=Bergstraße',               '06432=Darmstadt-Dieburg',
    '06433=Groß-Gerau',               '06434=Hochtaunuskreis',
    '06435=Main-Kinzig-Kreis',        '06436=Main-Taunus-Kreis',
    '06437=Odenwaldkreis',            '06438=Offenbach',
    '06439=Rheingau-Taunus-Kreis',    '06440=Wetteraukreis',
    // 130..139
    '06531=Gießen',                   '06532=Lahn-Dill-Kreis',
    '06533=Limburg-Weilburg',         '06534=Marburg-Biedenkopf',
    '06535=Vogelsbergkreis',          '06611=Kassel',
    '06631=Fulda',                    '06632=Hersfeld-Rotenburg',
    '06633=Kassel',                   '06634=Schwalm-Eder-Kreis',
    // 140..149
    '06635=Waldeck-Frankenberg',      '06636=Werra-Meißner-Kreis',
    '07111=Koblenz',                  '07131=Ahrweiler',
    '07132=Altenkirchen (Westerwald)','07133=Bad Kreuznach',
    '07134=Birkenfeld',               '07135=Cochem-Zell',
    '07137=Mayen-Koblenz',            '07138=Neuwied',
    // 150..159
    '07140=Rhein-Hunsrück-Kreis',     '07141=Rhein-Lahn-Kreis',
    '07143=Westerwaldkreis',          '07211=Trier',
    '07231=Bernkastel-Wittlich',      '07232=Eifelkreis Bitburg-Prüm',
    '07233=Vulkaneifel',              '07235=Trier-Saarburg',
    '07311=Frankenthal (Pfalz)',      '07312=Kaiserslautern',
    // 160..169
    '07313=Landau in der Pfalz',      '07314=Ludwigshafen am Rhein',
    '07315=Mainz',                    '07316=Neustadt an der Weinstraße',
    '07317=Pirmasens',                '07318=Speyer',
    '07319=Worms',                    '07320=Zweibrücken',
    '07331=Alzey-Worms',              '07332=Bad Dürkheim',
    // 170..179
    '07333=Donnersbergkreis',         '07334=Germersheim',
    '07335=Kaiserslautern',           '07336=Kusel',
    '07337=Südliche Weinstraße',      '07338=Rhein-Pfalz-Kreis',
    '07339=Mainz-Bingen',             '07340=Südwestpfalz',
    '08111=Stuttgart',                '08115=Böblingen',
    // 180..189
    '08116=Esslingen',                '08117=Göppingen',
    '08118=Ludwigsburg',              '08119=Rems-Murr-Kreis',
    '08121=Heilbronn',                '08125=Heilbronn',
    '08126=Hohenlohekreis',           '08127=Schwäbisch Hall',
    '08128=Main-Tauber-Kreis',        '08135=Heidenheim',
    // 190..199
    '08136=Ostalbkreis',              '08211=Baden-Baden',
    '08212=Karlsruhe',                '08215=Karlsruhe',
    '08216=Rastatt',                  '08221=Heidelberg',
    '08222=Mannheim',                 '08225=Neckar-Odenwald-Kreis',
    '08226=Rhein-Neckar-Kreis',       '08231=Pforzheim',
    // 200..201
    '08235=Calw',                     '08236=Enzkreis',
    '08237=Freudenstadt',             '08311=Freiburg im Breisgau',
    '08315=Breisgau-Hochschwarzwald', '08316=Emmendingen',
    '08317=Ortenaukreis',             '08325=Rottweil',
    '08326=Schwarzwald-Baar-Kreis',   '08327=Tuttlingen',
    // 210..219
    '08335=Konstanz',                 '08336=Lörrach',
    '08337=Waldshut',                 '08415=Reutlingen',
    '08416=Tübingen',                 '08417=Zollernalbkreis',
    '08421=Ulm',                      '08425=Alb-Donau-Kreis',
    '08426=Biberach',                 '08435=Bodenseekreis',
    // 220..229
    '08436=Ravensburg',               '08437=Sigmaringen',
    '09161=Ingolstadt',               '09162=München',
    '09163=Rosenheim',                '09171=Altötting',
    '09172=Berchtesgadener Land',     '09173=Bad Tölz-Wolfratshausen',
    '09174=Dachau',                   '09175=Ebersberg',
    // 230.239
    '09176=Eichstätt',                '09177=Erding',
    '09178=Freising',                 '09179=Fürstenfeldbruck',
    '09180=Garmisch-Partenkirchen',   '09181=Landsberg am Lech',
    '09182=Miesbach',                 '09183=Mühldorf a. Inn',
    '09184=München',                  '09185=Neuburg-Schrobenhausen',
    // 240..249
    '09186=Pfaffenhofen a.d. Ilm',    '09187=Rosenheim',
    '09188=Starnberg',                '09189=Traunstein',
    '09190=Weilheim-Schongau',        '09261=Landshut',
    '09262=Passau',                   '09263=Straubing',
    '09271=Deggendorf',               '09272=Freyung-Grafenau',
    // 250..259
    '09273=Kelheim',                  '09274=Landshut',
    '09275=Passau',                   '09276=Regen',
    '09277=Rottal-Inn',               '09278=Straubing-Bogen',
    '09279=Dingolfing-Landau',        '09361=Amberg',
    '09362=Regensburg',               '09363=Weiden i.d. OPf.',
    // 260..269
    '09371=Amberg-Sulzbach',          '09372=Cham',
    '09373=Neumarkt i.d. OPf.',       '09374=Neustadt a.d. Waldnaab',
    '09375=Regensburg',               '09376=Schwandorf',
    '09377=Tirschenreuth',            '09461=Bamberg',
    '09462=Bayreuth',                 '09463=Coburg',
    // 270..279
    '09464=Hof',                      '09471=Bamberg',
    '09472=Bayreuth',                 '09473=Coburg',
    '09474=Forchheim',                '09475=Hof',
    '09476=Kronach',                  '09477=Kulmbach',
    '09478=Lichtenfels',              '09479=Wunsiedel i. Fichtelgebirge',
    // 280..289
    '09561=Ansbach',                  '09562=Erlangen',
    '09563=Fürth',                    '09564=Nürnberg',
    '09565=Schwabach',                '09571=Ansbach',
    '09572=Erlangen-Höchstadt',       '09573=Fürth',
    '09574=Nürnberger Land',          '09575=Neustadt a.d. Aisch-Bad Windsheim',
    // 290..299
    '09576=Roth',                     '09577=Weißenburg-Gunzenhausen',
    '09661=Aschaffenburg',            '09662=Schweinfurt',
    '09663=Würzburg',                 '09671=Aschaffenburg',
    '09672=Bad Kissingen',            '09673=Rhön-Grabfeld',
    '09674=Haßberge',                 '09675=Kitzingen',
    // 300..309
    '09676=Miltenberg',               '09677=Main-Spessart',
    '09678=Schweinfurt',              '09679=Würzburg',
    '09761=Augsburg',                 '09762=Kaufbeuren',
    '09763=Kempten (Allgäu)',         '09764=Memmingen',
    '09771=Aichach-Friedberg',        '09772=Augsburg',
    // 310..319
    '09773=Dillingen a.d. Donau',     '09774=Günzburg',
    '09775=Neu-Ulm',                  '09776=Lindau (Bodensee)',
    '09777=Ostallgäu',                '09778=Unterallgäu',
    '09779=Donau-Ries',               '09780=Oberallgäu',
    '10041=Regionalverband Saarbrücken', '10042=Merzig-Wadern',
    // 320..329
    '10043=Neunkirchen',              '10044=Saarlouis',
    '10045=Saarpfalz-Kreis',          '10046=St. Wendel',
    '12051=Brandenburg an der Havel', '12052=Cottbus',
    '12053=Frankfurt (Oder)',         '12054=Potsdam',
    '12060=Barnim',                   '12061=Dahme-Spreewald',
    // 330-339
    '12062=Elbe-Elster',              '12063=Havelland',
    '12064=Märkisch-Oderland',        '12065=Oberhavel',
    '12066=Oberspreewald-Lausitz',    '12067=Oder-Spree',
    '12068=Ostprignitz-Ruppin',       '12069=Potsdam-Mittelmark',
    '12070=Prignitz',                 '12071=Spree-Neiße',
    // 340..349
    '12072=Teltow-Fläming',           '12073=Uckermark',
    '13003=Rostock',                  '13004=Schwerin',
    '13071=Mecklenburgische Seenplatte', '13072=Rostock',
    '13073=Vorpommern-Rügen',         '13074=Nordwestmecklenburg',
    '13075=Vorpommern-Greifswald',    '13076=Ludwigslust-Parchim',
    // 350..359
    '14511=Chemnitz',                 '14521=Erzgebirgskreis',
    '14522=Mittelsachsen',            '14523=Vogtlandkreis',
    '14524=Zwickau',                  '14612=Dresden',
    '14625=Bautzen',                  '14626=Görlitz',
    '14627=Meißen',                   '14628=Sächsische Schweiz-Osterzgebirge',
    // 360..369
    '14713=Leipzig',                  '14729=Leipzig',
    '14730=Nordsachsen',              '15001=Dessau-Roßlau',
    '15002=Halle (Saale)',            '15003=Magdeburg',
    '15081=Altmarkkreis Salzwedel',   '15082=Anhalt-Bitterfeld',
    '15083=Börde',                    '15084=Burgenlandkreis',
    // 370..379
    '15085=Harz',                     '15086=Jerichower Land',
    '15087=Mansfeld-Südharz',         '15088=Saalekreis',
    '15089=Salzlandkreis',            '15090=Stendal',
    '15091=Wittenberg',               '16051=Erfurt',
    '16052=Gera',                     '16053=Jena',
    // 380..389
    '16054=Suhl',                     '16055=Weimar',
    '16056=Eisenach',                 '16061=Eichsfeld',
    '16062=Nordhausen',               '16063=Wartburgkreis',
    '16064=Unstrut-Hainich-Kreis',    '16065=Kyffhäuserkreis',
    '16066=Schmalkalden-Meiningen',   '16067=Gotha',
    // 390..399
    '16068=Sömmerda',                 '16069=Hildburghausen',
    '16070=Ilm-Kreis',                '16071=Weimarer Land',
    '16072=Sonneberg',                '16073=Saalfeld-Rudolstadt',
    '16074=Saale-Holzland-Kreis',     '16075=Saale-Orla-Kreis',
    '16076=Greiz',                    '16077=Altenburger Land',
    // 400..409
    '11012=Berlin Reinickendorf',     '11004=Berlin Charlottenburg-Wilmersdorf',
    '11009=Berlin Treptow-Köpenick',  '11003=Berlin Pankow',
    '11008=Berlin Neukölln',          '11011=Berlin Lichtenberg',
    '11010=Berlin Marzahn-Hellersdorf', '11005=Berlin Spandau',
    '11006=Berlin Steglitz-Zehlendorf', '11001=Berlin Mitte',
    //410, 411
    '11002=Berlin Friedrichshain-Kreuzberg', '11007=Berlin Tempelhof-Schöneberg'
  );


{------------------------------------------------------------------------------}
{                         TRobertKochDatasource                                }
{------------------------------------------------------------------------------}

procedure TRobertKochDataSource.DownloadToCache;
begin
end;

function TRobertKochDataSource.GetDataString(const ACountry, AState: String;
  ACaseType: TCaseType; var AHeader, ACounts: String): Boolean;
begin
  Result := false;

  Result := true;
end;

function TRobertKochDataSource.LoadLocations(ATreeView: TTreeView): Boolean;
var
  topnode, landnode: TTreeNode;
  i, id, prevID: PtrInt;
  s: String;
  p: Integer;
begin
  Result := false;

  prevID := -1;
  topnode := ATreeView.Items.AddChild(nil, 'Germany (RKI)');

  for i:=Low(Bundesland) to High(Bundesland) do
    landnode := ATreeView.Items.AddChildObject(topnode, Bundesland[i], Pointer(i));

  for i := Low(Landkreis) to High(Landkreis) do
  begin
    s := Copy(Landkreis[i], 1, 2);
    id := StrToInt(s);
    if id <> prevID then
    begin
      landNode := ATreeView.Items.FindNodeWithText(Bundesland[id]);
      prevID := id;
    end;
    s := Copy(Landkreis[i], 1, 5);
    id := StrToInt(s);
    s := Copy(Landkreis[i], 7, MaxInt);
    ATreeView.Items.AddChildObject(landnode, s, pointer(id));
  end;

  Result := true;
end;

end.

