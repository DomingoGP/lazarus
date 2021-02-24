unit lazdiffregisteride;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType,
  // LazUtils
  //LazFileUtils,
  // IdeIntf
  LazIDEIntf, MenuIntf, IdeCommands;

procedure Register;

implementation

uses
  lazdiffform, lazdiffUIConsts;

procedure DoCompareFilesMenu(Sender: TObject);
var
  index: integer;
  //  ActiveSrcEdit: TSourceEditor;
  //  ActiveUnitInfo: TUnitInfo;
begin
  index := 0;
  //  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  //  if ActiveSrcEdit<>nil then
  //    index:=ActiveSrcEdit.PageIndex;

  //TODO: find active editor index.
  ShowDiffDialog(index);
end;


procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  CmdMyTool: TIDECommand;
begin
  // register IDE shortcut and menu item
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdMyTool := RegisterIDECommand(Cat, FORMAT_CURRENT_MENU2, MENU_CMD_DESC2, Key, nil, @DoCompareFilesMenu);
  RegisterIDEMenuCommand(itmSecondaryTools, 'LazDiffCompareFiles',  FORMAT_CURRENT_MENU2, nil, nil, CmdMyTool);
end;

end.
