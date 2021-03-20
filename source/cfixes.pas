unit cFixes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion, ComCtrls;

{$IF LCL_FullVersion < 2010000}
type
  TTreeNodeHelper = class helper for TTreeNode
    function GetFirstSibling: TTreeNode;
  end;
{$IFEND}

implementation

{$IF LCL_FullVersion < 2010000}
function TTreeNodeHelper.GetFirstSibling: TTreeNode;
begin
  if Self.Parent = nil then
    Result := Self.Owner.Owner.Items.GetFirstNode
  else
    Result := self.Parent.GetFirstChild;
end;
{$IFEND}

end.

