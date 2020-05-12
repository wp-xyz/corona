unit cRobertKochInstitut;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  cGlobal, cDataSource;

const
  RKI_CAPTION = 'Germany (RKI)';

type
  TRobertKochDatasource = class(TcDataSource)
  private
    function BuildURL(const ID: String): String;
//    procedure ExtractData(AData: String; out AHeader, AConfirmed, ADeaths: String);
    procedure ExtractData(AStream: TStream; out AHeader, AConfirmed, ADeaths: String);
  protected
  public
    procedure ClearCache;
    procedure DownloadToCache; override;
    function GetDataString(const ACountry, AState: String; ACaseType: TCaseType;
      out AHeader, ACounts: String): Boolean; override;
    class function GetDisplayString(const AText: String): String;
    class function GetPopulation(const AText: String): Integer;
    function LoadLocations(ATreeView: TTreeView): Boolean; override;
  end;

implementation

uses
  DateUtils,
  fpJson, JSONParser, JSONScanner,
  cDownloader;

const
  FILENAME_CONFIRMED = 'RKI_confirmed.csv';
  FILENAME_DEATHS = 'RKI__deaths.csv';
//  FILENAME_RECOVERED = 'RKI_recovered.csv';

  POPULATION_GERMANY = 83149300;  // Sept 30, 2019; wikipedia.

  Bundesland: array[1..16] of string = (
    'Schleswig-Holstein|2897000',      // 1
    'Hamburg|1841000',                 // 2
    'Niedersachsen|7982000',           // 3
    'Bremen|683000',                   // 4
    'Nordrhein-Westfalen|17933000',    // 5
    'Hessen|6266000',                  // 6
    'Rheinland-Pfalz|4085000',         // 7
    'Baden-Württemberg|11070000',      // 8
    'Bayern|13077000',                 // 9
    'Saarland|991000',                 // 10
    'Berlin|3645000',                  // 11
    'Brandenburg|2512000',             // 12
    'Mecklenburg-Vorpommern|1610000',  // 13
    'Sachsen|4078000',                 // 14
    'Sachsen-Anhalt|2208000',          // 15
    'Thüringen|2143000'                // 16
  );

  const Landkreis: array[0..411] of string = (
    // 0..9
    '01001=Flensburg|89504',                  '01002=Kiel|247548',
    '01003=Lübeck|217198',                    '01004=Neumünster|79487',
    '01051=Dithmarschen|133210',              '01053=Herzogtum Lauenburg|197264',
    '01054=Nordfriesland|165507',             '01055=Ostholstein|200581',
    '01056=Pinneberg|314391',                 '01057=Plön|128647',
     // 10..19
    '01058=Rendsburg-Eckernförde|272775',     '01059=Schleswig-Flensburg|200025',
    '01060=Segeberg|276032',                  '01061=Steinburg|131347',
    '01062=Stormarn|243196',                  '02000=Hamburg|1841179',
    '03101=Braunschweig|248292',              '03102=Salzgitter|104948',
    '03103=Wolfsburg|124151',                 '03151=Gifhorn|175920',
    //20..29
    '03153=Goslar|137014',                    '03154=Helmstedt|91307',
    '03155=Northeim|132765',                  '03157=Peine|133965',
    '03158=Wolfenbüttel|119960',              '03159=Göttingen|328074',
    '03241=Region Hannover|1157624',          '03251=Diepholz|216886',
    '03252=Hameln-Pyrmont|148559',            '03254=Hildesheim|276594',
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
    '03458=Oldenburg',                '03459=Osnabrück (Landkreis)',
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
    '08111=Stuttgart|634830',                '08115=Böblingen|391640',
    // 180..189
    '08116=Esslingen|533859',                '08117=Göppingen|257253',
    '08118=Ludwigsburg|543984',              '08119=Rems-Murr-Kreis|426158',
    '08121=Heilbronn|125960',                '08125=Heilbronn (Landkreis)|343068',
    '08126=Hohenlohekreis|112010',           '08127=Schwäbisch Hall|195861',
    '08128=Main-Tauber-Kreis|132321',        '08135=Heidenheim|132472',
    // 190..199
    '08136=Ostalbkreis|314002',              '08211=Baden-Baden|55123',
    '08212=Karlsruhe|313092',                '08215=Karlsruhe (Landkreis)|444232',
    '08216=Rastatt|231018',                  '08221=Heidelberg|160355',
    '08222=Mannheim|309370',                 '08225=Neckar-Odenwald-Kreis|143535',
    '08226=Rhein-Neckar-Kreis|547625',       '08231=Pforzheim|125542',
    // 200..201
    '08235=Calw|158397',                     '08236=Enzkreis|198905',
    '08237=Freudenstadt|117935',             '08311=Freiburg im Breisgau|230241',
    '08315=Breisgau-Hochschwarzwald|262795', '08316=Emmendingen|165383',
    '08317=Ortenaukreis|429479',             '08325=Rottweil|139455',
    '08326=Schwarzwald-Baar-Kreis|212381',   '08327=Tuttlingen|140152',
    // 210..219
    '08335=Konstan|285325',                  '08336=Lörrach|228639',
    '08337=Waldshut|170619',                 '08415=Reutlingen|286748',
    '08416=Tübingen|227331',                 '08417=Zollernalbkreis|188935',
    '08421=Ulm|126329',                      '08425=Alb-Donau-Kreis|196047',
    '08426=Biberach|199742',                 '08435=Bodenseekreis|216227',
    // 220..229
    '08436=Ravensburg|284285',               '08437=Sigmaringen|130873',
    '09161=Ingolstadt|136981',               '09162=München|1471508',
    '09163=Rosenheim|63324',                 '09171=Altötting|111210',
    '09172=Berchtesgadener Land|105722',     '09173=Bad Tölz-Wolfratshausen|127227',
    '09174=Dachau|153884',                   '09175=Ebersberg|142142',
    // 230.239
    '09176=Eichstätt|132341',                '09177=Erding|137660',
    '09178=Freising|179116',                 '09179=Fürstenfeldbruck|219320',
    '09180=Garmisch-Partenkirchen|88467',    '09181=Landsberg am Lech|120071',
    '09182=Miesbach|99726',                  '09183=Mühldorf a. Inn|115250',
    '09184=München (Landkreis)|348871',      '09185=Neuburg-Schrobenhausen|96680',
    // 240..249
    '09186=Pfaffenhofen a.d. Ilm|127151',    '09187=Rosenheim (Landkreis)|260983',
    '09188=Starnberg|136092',                '09189=Traunstein|177089',
    '09190=Weilheim-Schongau|135348',        '09261=Landshut|72404',
    '09262=Passau|52469',                    '09263=Straubing|47794',
    '09271=Deggendorf|119326',               '09272=Freyung-Grafenau|78355',
    // 250..259
    '09273=Kelheim|122258',                  '09274=Landshut (Landkreis)|158698',
    '09275=Passau (Landkreis)|192043',       '09276=Regen|77656',
    '09277=Rottal-Inn|120659',               '09278=Straubing-Bogen|100649',
    '09279=Dingolfing-Landau|96217',         '09361=Amberg|41970',
    '09362=Regensburg|152610',               '09363=Weiden i.d. OPf.|42520',
    // 260..269
    '09371=Amberg-Sulzbach|103109',          '09372=Cham|127882',
    '09373=Neumarkt i.d. OPf.|133561',       '09374=Neustadt a.d. Waldnaab|94352',
    '09375=Regensburg (Landkreis)|193572',   '09376=Schwandorf|147189',
    '09377=Tirschenreuth|72504',             '09461=Bamberg|77592',
    '09462=Bayreuth|74657',                  '09463=Coburg|41249',
    // 270..279
    '09464=Hof|45930',                       '09471=Bamberg (Landkreis)|147086',
    '09472=Bayreuth (Landkreis)|103656',     '09473=Coburg (Landkreis)|86906',
    '09474=Forchheim|116099',                '09475=Hof (Landkreis)|95311',
    '09476=Kronach|67135',                   '09477=Kulmbach|71845',
    '09478=Lichtenfels|66838' ,              '09479=Wunsiedel i. Fichtelgebirge|73178',
    // 280..289
    '09561=Ansbach|41847' ,                  '09562=Erlangen|111962',
    '09563=Fürth|127748',                    '09564=Nürnberg|518365',
    '09565=Schwabach|40792',                 '09571=Ansbach (Landkreis)|183949',
    '09572=Erlangen-Höchstadt|136271',       '09573=Fürth (Landkreis)|117387',
    '09574=Nürnberger Land|170365',          '09575=Neustadt a.d. Aisch-Bad Windsheim|100364',
    // 290..299
    '09576=Roth|126958',                     '09577=Weißenburg-Gunzenhausen|94393',
    '09661=Aschaffenburg|70527',             '09662=Schweinfurt|54032',
    '09663=Würzburg|127880',                 '09671=Aschaffenburg (Landkreis)|174208',
    '09672=Bad Kissingen|103218',            '09673=Rhön-Grabfeld|79690',
    '09674=Haßberge|84599',                  '09675=Kitzingen|90909',
    // 300..309
    '09676=Miltenberg|128756',               '09677=Main-Spessart|126365',
    '09678=Schweinfurt (Landkreis)|115106',  '09679=Würzburg (Landkreis)|161834',
    '09761=Augsburg|295135',                 '09762=Kaufbeuren|43893',
    '09763=Kempten (Allgäu)|68907',          '09764=Memmingen|43837',
    '09771=Aichach-Friedberg|133596',        '09772=Augsburg (Landkreis)|251534',
    // 310..319
    '09773=Dillingen a.d. Donau|96021',      '09774=Günzburg|125747',
    '09775=Neu-Ulm|174200',                  '09776=Lindau (Bodensee)|81669',
    '09777=Ostallgäu|140316',                '09778=Unterallgäu|144041',
    '09779=Donau-Ries|133496',               '09780=Oberallgäu|155362',
    '10041=Regionalverband Saarbrücken|10041000', '10042=Merzig-Wadern|103366',
    // 320..329
    '10043=Neunkirchen|132206',              '10044=Saarlouis|195201',
    '10045=Saarpfalz-Kreis|142631',          '10046=St. Wendel|87397',
    '12051=Brandenburg an der Havel|72124',  '12052=Cottbus|100219',
    '12053=Frankfurt (Oder)|57873',          '12054=Potsdam|178089',
    '12060=Barnim|182760',                   '12061=Dahme-Spreewald|169067',
    // 330-339
    '12062=Elbe-Elster|102638',              '12063=Havelland|161909',
    '12064=Märkisch-Oderland|194328',        '12065=Oberhavel|211249',
    '12066=Oberspreewald-Lausitz|110476',    '12067=Oder-Spree|178658',
    '12068=Ostprignitz-Ruppin|99078',        '12069=Potsdam-Mittelmark|214664',
    '12070=Prignitz|76508',                  '12071=Spree-Neiße|114429',
    // 340..349
    '12072=Teltow-Fläming|168296',           '12073=Uckermark|119552',
    '13003=Rostock|208886',                  '13004=Schwerin|95818',
    '13071=Mecklenburgische Seenplatte|259130', '13072=Rostock (Landkreis)|215113',
    '13073=Vorpommern-Rügen|224684',         '13074=Nordwestmecklenburg|156729',
    '13075=Vorpommern-Greifswald|236697',    '13076=Ludwigslust-Parchim|212618',
    // 350..359
    '14511=Chemnitz|247237',                 '14521=Erzgebirgskreis|337696',
    '14522=Mittelsachsen|306185',            '14523=Vogtlandkreis|227796',
    '14524=Zwickau|317531',                  '14612=Dresden|554649',
    '14625=Bautze|300880',                   '14626=Görlitz|254894',
    '14627=Meißen|242165',                   '14628=Sächsische Schweiz-Osterzgebirge|245611',
    // 360..369
    '14713=Leipzig|587857',                  '14729=Leipzig (Landkreis)|257763',
    '14730=Nordsachsen|197673',              '15001=Dessau-Roßlau|81237',
    '15002=Halle (Saale)|239257',            '15003=Magdeburg|238697',
    '15081=Altmarkkreis Salzwedel|83765',    '15082=Anhalt-Bitterfeld|159854',
    '15083=Börde|171734',                    '15084=Burgenlandkreis|180190',
    // 370..379
    '15085=Harz|214446',                     '15086=Jerichower Land|89928',
    '15087=Mansfeld-Südharz|136249',         '15088=Saalekreis|184582',
    '15089=Salzlandkreis|190560',            '15090=Stendal|111982',
    '15091=Wittenberg|125840',               '16051=Erfurt|213699',
    '16052=Gera|94152',                      '16053=Jena|111407',
    // 380..389
    '16054=Suhl|34835',                      '16055=Weimar|65090',
    '16056=Eisenach|42370',                  '16061=Eichsfeld|100380',
    '16062=Nordhausen|83822',                '16063=Wartburgkreis|123025',
    '16064=Unstrut-Hainich-Kreis|102912',    '16065=Kyffhäuserkreis|75009',
    '16066=Schmalkalden-Meiningen|122347',   '16067=Gotha|135452',
    // 390..399
    '16068=Sömmerda|69655',                  '16069=Hildburghausen|63553',
    '16070=Ilm-Kreis|108742',                '16071=Weimarer Land|81947',
    '16072=Sonneberg|56196',                 '16073=Saalfeld-Rudolstadt|106356',
    '16074=Saale-Holzland-Kreis|83051',      '16075=Saale-Orla-Kreis|80868',
    '16076=Greiz|98159',                     '16077=Altenburger Land|90118',
    // 400..409
    '11012=Berlin Reinickendorf|265225',     '11004=Berlin Charlottenburg-Wilmersdorf|342332',
    '11009=Berlin Treptow-Köpenick|271153',  '11003=Berlin Pankow|407765',
    '11008=Berlin Neukölln|329691',          '11011=Berlin Lichtenberg|291452',
    '11010=Berlin Marzahn-Hellersdorf|268548', '11005=Berlin Spandau|243977',
    '11006=Berlin Steglitz-Zehlendorf|308697', '11001=Berlin Mitte|384172',
    //410, 411
    '11002=Berlin Friedrichshain-Kreuzberg|289762', '11007=Berlin Tempelhof-Schöneberg|351644'
  );

  DATE_MASK = 'yyyy-mm-dd hh:nn:ss';

  BASE_URL = 'https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/Covid19_RKI_Sums/FeatureServer/0/query';

  // Landkreis-Abfrage
  LK_URL_MASK =
    '?f=json'+
    '&where='+
        '(Meldedatum >= timestamp ''%s'') AND ' +    // Start date formatted as YYYY-MM-DD HH:NN:SS     2020-03-01 22:59:59
        '(Meldedatum < timestamp ''%s'') AND ' +     // End date formatted as YYYY-MM-DD HH:NN:SS       2020-03-27 23:00:00:
        '(IdLandkreis = ''%s'')'+            // IdLandkreis=''%s'' with Landkreis ID, 5 digits, leading zero; or IdBundesland=''%s'' with Bundesland ID
    '&returnGeometry=false'+
    '&spatialRel=esriSpatialRelIntersects'+
    '&outFields=SummeFall,SummeTodesFall,Meldedatum'+
    '&orderByFields=Meldedatum asc'+
    '&resultOffset=0'+
    '&resultRecordCount=2000'+
    '&cacheHint=true';

  // Bundesland-Abfrage
  BL_URL_MASK =
    '?f=json'+
    '&where='+
      '(Meldedatum >= timestamp ''%s'') AND ' +
      '(Meldedatum < timestamp  ''%s'') AND ' +
      '(IdBundesland = ''%s'')' +
    '&outFields=SummeFall,SummeTodesfall,Meldedatum'+
    '&returnGeometry=false'+
    '&spatialRel=esriSpatialRelIntersects'+
    '&orderByFields=Meldedatum asc'+
    '&groupByFieldsForStatistics=Meldedatum'+
    '&outStatistics=['+
      '{"statisticType":"sum","onStatisticField":"SummeFall","outStatisticFieldName":"SummeFall"},'+
      '{"statisticType":"sum","onStatisticField":"SummeTodesfall","outStatisticFieldName":"SummeTodesfall"}'+
    ']';

  // Deutschland (RKI)-Abfrage
  DE_URL_MASK =
    '?f=json'+
    '&where='+
      '(Meldedatum >= timestamp ''%s'') AND ' +
      '(Meldedatum < timestamp  ''%s'')' +
    '&outFields=SummeFall,SummeTodesfall,Meldedatum'+
    '&returnGeometry=false'+
    '&spatialRel=esriSpatialRelIntersects'+
    '&orderByFields=Meldedatum asc'+
    '&groupByFieldsForStatistics=Meldedatum'+
    '&outStatistics=['+
      '{"statisticType":"sum","onStatisticField":"SummeFall","outStatisticFieldName":"SummeFall"},'+
      '{"statisticType":"sum","onStatisticField":"SummeTodesfall","outStatisticFieldName":"SummeTodesfall"}'+
    ']';


{------------------------------------------------------------------------------}
{                         TRobertKochDatasource                                }
{------------------------------------------------------------------------------}

function TRobertKochDataSource.BuildURL(const ID: String): String;
var
  startDate: TDate;
  endDate: TDate;
  url: String;
  i: Integer;
begin
  startDate := EncodeDate(2020, 3, 1);
  endDate := trunc(Now);

  if (ID = '') then
    // Total for Germany
    url := Format(DE_URL_MASK, [
    FormatDateTime(DATE_MASK, startDate),
    FormatDateTime(DATE_MASK, endDate)
  ])
  else
  if Length(ID) <= 2 then
    // Federal state (Bundesland)
    url := Format(BL_URL_MASK, [
      FormatDateTime(DATE_MASK, startDate),
      FormatDateTime(DATE_MASK, endDate),
      ID
    ])
  else
    // County (Landkreis)
    url := Format(LK_URL_MASK, [
      FormatDateTime(DATE_MASK, startDate),
      FormatDateTime(DATE_MASK, endDate),
      ID
    ]);

  Result := BASE_URL;
  for i := 1 to Length(url) do
    case url[i] of
      ' ' : Result := Result + '%20';
      '"' : Result := Result + '%22';
      '''': Result := Result + '%27';
      ':' : Result := Result + '%3A';
      '<' : Result := Result + '%3C';
      '>' : Result := Result + '%3E';
      '[' : Result := Result + '%5B';
      ']' : Result := Result + '%5D';
      '{' : Result := Result + '%7B';
      '}' : Result := Result + '%7D';
      else  Result := Result + url[i];
    end;
end;

procedure TRobertKochDataSource.ClearCache;
begin
  // Delete the cache files. Cache will be rebuilt when querying data.
  DeleteFile(FCacheDir + FILENAME_CONFIRMED);
  DeleteFile(FCacheDir + FILENAME_DEATHS);
end;

procedure TRobertKochDataSource.DownloadToCache;
begin
  // Delete the cache files. Cache will be rebuilt when querying data.
  ClearCache;
end;

procedure TRobertKochDataSource.ExtractData(AStream: TStream;
  out AHeader, AConfirmed, ADeaths: String);
var
  json: TJSONObject;
  p: TJSONParser;
  jFeatures: TJSONArray;
  jObj: TJSONObject;
  jData: TJSONObject;
  i: Integer;
  s: String;
  d: TDateTime;
begin
  AHeader := '';
  AConfirmed := '';
  ADeaths := '';
  AStream.Position := 0;
  p := TJSONParser.Create(AStream, [joUTF8]);
  try
    json := p.Parse as TJSONObject;
    if json = nil then
      exit;
    jFeatures := json.Find('features', jtArray) as TJSONArray;
    if jFeatures = nil then
      exit;
    AHeader := '';
    AConfirmed := '';
    ADeaths := '';
    for i := 0 to jFeatures.Count - 1 do begin
      jObj := jFeatures.Objects[i];
      jData := jObj.Items[0] as TJsonObject;
      s := jData.Find('Meldedatum').AsString;
      d := UnixToDateTime(StrToInt64(s) div 1000);
      AHeader := AHeader + ',' + FormatDateTime('mm"/"dd"/"yy', d);
      s := jData.Find('SummeFall').AsString;
      AConfirmed := AConfirmed + ',' + s;
      s := jData.Find('SummeTodesfall').AsString;
      ADeaths := ADeaths + ',' + s;
    end;
  finally
    p.Free;
  end;
end;

function TRobertKochDataSource.GetDataString(const ACountry, AState: String;
  ACaseType: TCaseType; out AHeader, ACounts: String): Boolean;
var
  url: String;
  stream: TStream;
  s: String;
  L: TStrings;
  sa: TStringArray;
  fn: String;
  i: Integer;
  sConfirmed, sDeaths: String;
begin
  Result := false;
  AHeader := '';
  ACounts := '';

  case ACaseType of
    ctConfirmed: fn := FCacheDir + FILENAME_CONFIRMED;
    ctDeaths: fn := FCacheDir + FILENAME_DEATHS;
    ctRecovered: exit;
    ctSick: exit;
  end;

  L := TStringList.Create;
  try
    if FileExists(fn) then begin
      L.StrictDelimiter := true;
      L.LoadFromFile(fn);
      for i:=1 to L.Count-1 do
      begin
        sa := L[i].Split(',', '"');
        if (sa[0] = AState) and (sa[1] = ACountry) then begin
          // Dataset exists in cache --> use it
          AHeader := L[0];
          ACounts := L[i];
          Result := true;
          exit;
        end;
      end;
    end;

    // Dataset does not yet exist --> Read from RKI server and add to cache
    stream := TMemoryStream.Create;
    try
      if (ACountry = RKI_CAPTION) then
        url := BuildURL('')             // Total
      else
      if AState <> '' then
        url := BuildURL(AState)        // Landkreis
      else
        url := BuildURL(ACountry);     // Bundesland
      Result := DownloadFile(url, stream);
      if Result then
      begin
        TMemoryStream(stream).SaveToFile('test.txt');

        ExtractData(stream, AHeader, sConfirmed, sDeaths);
        AHeader := 'state,country,lat,long' + AHeader;
        sConfirmed := AState + ',' + ACountry + ',0,0' + sConfirmed;
        sDeaths := AState + ',' + ACountry + ',0,0' + sDeaths;
        if L.Count = 0 then
          L.Add(AHeader);
        if ACaseType = ctConfirmed then begin
          ACounts := sConfirmed;
          L.Add(sConfirmed);
        end else
        if ACaseType = ctDeaths then begin
          ACounts := sDeaths;
          L.Add(sDeaths);
        end;
        L.SaveToFile(fn);

        // Cache the "other" data contained in downloaded file.
        L.Clear;
        case ACaseType of
          ctConfirmed:
            begin
              fn := FCacheDir + FILENAME_DEATHS;
              s := sDeaths;
            end;
          ctDeaths:
            begin
              fn := FCacheDir + FILENAME_CONFIRMED;
              s := sConfirmed;
            end;
        end;
        if FileExists(fn) then
        begin
          L.LoadFromFile(fn);
          for i:=1 to L.Count-1 do
          begin
            sa := L[i].Split(',', '"');
            if (sa[0] = AState) and (sa[1] = ACountry) then
              exit;
          end;
        end;
        if L.Count = 0 then
          L.Add(AHeader);
        L.Add(s);
        L.SaveToFile(fn);
      end;
    finally
      stream.Free;
    end;
  finally
    L.Free;
  end;
end;
                                     (*
class function TRobertKochDataSource.GetBundesLandPopulation(AIndex: Integer): Integer;
var
  p: Integer;
begin
  p := pos('|', Bundesland[AIndex]);
  if p > 0 then
    Result := StrToInt(Copy(Bundesland[AIndex], p+1, MaxInt))
  else
    Result := 0;
end;                                   *)

class function TRobertKochDataSource.GetDisplayString(const AText: String): String;
var
  p: Integer;
  population: Double;
begin
  p := pos('=', AText);
  if p > 0 then
    Result := Copy(AText, p+1, MaxInt)
  else
    Result := AText;

  p := pos('|', Result);
  if p > 0 then
    Result := Copy(Result, 1, p-1)
end;
                                               (*
class function TRobertKochDataSource.GetLandkreisPopulation(AState: String): Integer;
var
  i: Integer;
  s: String;
  p: Integer;
begin
  for i := Low(Landkreis) to High(Landkreis) do
  begin
    s := Copy(LandKreis[i], 1, 5);
    if s = AState then
    begin
      p := pos('|', Landkreis[i]);
      if p > 0 then
      begin
        s := copy(Landkreis[i], p+1, MaxInt);
        Result := StrToInt(s);
      end
      else
        Result := 0;
    end;
  end;
end;             *)

class function TRobertKochDataSource.GetPopulation(const AText: String): Integer;
var
  p: Integer;
begin
  p := pos('|', AText);
  if p > 0 then
    Result := StrToInt(Copy(AText, p+1, MaxInt))
  else
    Result := 0;
end;

function TRobertKochDataSource.LoadLocations(ATreeView: TTreeView): Boolean;
var
  topnode, landnode: TTreeNode;
  i, id, prevID: PtrInt;
  s: String;
  p: Integer;
  loc: PLocationParams;
begin
  Result := false;

  prevID := -1;
  New(loc);
  {$IFDEF DEBUG_LOCATIONPARAMS}
  loc^.Name := 'Germany';
  {$ENDIF}
  loc^.ID := -1;
  loc^.Population := POPULATION_GERMANY;
  topnode := ATreeView.Items.AddChildObject(nil, 'Germany (RKI)', loc);

  for i:=Low(Bundesland) to High(Bundesland) do begin
    s := GetDisplayString(Bundesland[i]);
    New(loc);
    {$IFDEF DEBUG_LOCATIONPARAMS}
    loc^.Name := s;
    {$ENDIF}
    loc^.ID := i;
    loc^.Population := GetPopulation(Bundesland[i]);
    landnode := ATreeView.Items.AddChildObject(topnode, s, loc);
  end;

  for i := Low(Landkreis) to High(Landkreis) do
  begin
    s := Copy(Landkreis[i], 1, 2);
    id := StrToInt(s);
    New(loc);
    {$IFDEF DEBUG_LOCATIONPARAMS}
    loc^.Name := s;
    {$ENDIF}
    if id <> prevID then
    begin
      s := GetDisplayString(Bundesland[id]);
      landNode := ATreeView.Items.FindNodeWithText(s);
      prevID := id;
    end;
    s := GetDisplayString(Landkreis[i]);
    loc^.ID := StrToInt(Copy(Landkreis[i], 1, 5));
    loc^.Population := GetPopulation(Landkreis[i]);
    ATreeView.Items.AddChildObject(landnode, s, loc);
  end;

  Result := true;
end;

end.

