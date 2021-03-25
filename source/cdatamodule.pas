unit cDataModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TMainDatamodule }

  TMainDatamodule = class(TDataModule)
    ImageList: TImageList;
  private

  public

  end;

var
  MainDatamodule: TMainDatamodule;

implementation

{$R *.lfm}

end.

