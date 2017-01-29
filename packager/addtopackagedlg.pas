{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TAddToPackageDlg is the form for adding files to an open package.
}
unit AddToPackageDlg;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, AVL_Tree,
  // LCL
  LCLProc, LCLType, Forms, Controls, Buttons, ExtDlgs, StdCtrls, ExtCtrls,
  Dialogs, ComCtrls, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  NewItemIntf, PackageIntf, FormEditingIntf, IDEWindowIntf, ComponentReg, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, InputHistory, IDEDefs, EnvironmentOpts,
  PackageSystem, PackageDefs, AddDirToPkgDlg, ProjPackChecks;
  
type

  { TAddToPkgResult }

  TAddToPkgResult = class
  public
    Pkg: TLazPackage;
    AddType: TAddToPkgType;
    Dependency: TPkgDependency;
    UnitFilename: string;
    Unit_Name: string;
    AncestorType: string;
    NewClassName: string;
    PageName: string;
    FileType: TPkgFileType;
    PkgFileFlags: TPkgFileFlags;
    UsedUnitname: string;
    IconFile: string;
    AutoAddLFMFile: boolean;
    AutoAddLRSFile: boolean;
    NewItem: TNewIDEItemTemplate;
    Next: TAddToPkgResult;
    procedure Clear;
    destructor Destroy; override;
  end;
  
  TOnGetUnitRegisterInfo = procedure(Sender: TObject; const AFilename: string;
    out TheUnitName: string; out HasRegisterProc: boolean) of object;

  { TAddToPackageDlg }

  TAddToPackageDlg = class(TForm)
    AddFilesBtnPanel: TPanel;
    AddFilesPage: TTabSheet;
    AncestorComboBox: TComboBox;
    AncestorShowAllCheckBox: TCheckBox;
    AncestorTypeLabel: TLabel;
    ButtonPanel1: TButtonPanel;
    ClassNameEdit: TEdit;
    ClassNameLabel: TLabel;
    ComponentIconLabel: TLabel;
    ComponentIconBitBtn: TBitBtn;
    ComponentUnitFileBrowseButton: TButton;
    ComponentUnitFileEdit: TEdit;
    ComponentUnitFileLabel: TLabel;
    ComponentUnitFileShortenButton: TButton;
    ComponentUnitNameEdit: TEdit;
    ComponentUnitNameLabel: TLabel;
    FilesDeleteButton: TBitBtn;
    FilesDirButton: TBitBtn;
    FilesListView: TListView;
    FilesShortenButton: TBitBtn;
    LabelIconInfo: TLabel;
    NewComponentPage: TTabSheet;
    PageControl1: TPageControl;
    PalettePageCombobox: TComboBox;
    PalettePageLabel: TLabel;
    procedure AddToPackageDlgClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure AddToPackageDlgKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AncestorComboBoxChange(Sender: TObject);
    procedure AncestorComboBoxCloseUp(Sender: TObject);
    procedure AncestorShowAllCheckBoxClick(Sender: TObject);
    procedure CancelAddFileButtonClick(Sender: TObject);
    procedure CancelAddUnitButtonClick(Sender: TObject);
    procedure ClassNameEditChange(Sender: TObject);
    procedure ComponentIconBitBtnClick(Sender: TObject);
    procedure ComponentUnitFileBrowseButtonClick(Sender: TObject);
    procedure ComponentUnitFileShortenButtonClick(Sender: TObject);
    procedure ComponentUnitNameEditChange(Sender: TObject);
    procedure FilesAddButtonClick(Sender: TObject);
    procedure FilesDeleteButtonClick(Sender: TObject);
    procedure FilesDirButtonClick(Sender: TObject);
    procedure FilesListViewSelectItem(Sender: TObject; {%H-}Item: TListItem; {%H-}Selected: Boolean);
    procedure FilesShortenButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewComponentButtonClick(Sender: TObject);
    procedure NewComponentPageResize(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    fLastNewComponentAncestorType: string;
    fLastNewComponentClassName: string;
    FLazPackage: TLazPackage;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    fPkgComponents: TAVLTree;// tree of TPkgComponent
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    FComponentIconFilename: string;
    function GetActivatePage: TAddToPkgType;
    procedure SetActivatePage(AValue: TAddToPkgType);
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure SetupNewComponentPage;
    procedure SetupAddFilesPage;
    procedure OnIterateComponentClasses(PkgComponent: TPkgComponent);
    function CheckNewCompOk: Boolean;
    function CheckFilesButtonsOk: Boolean;
    procedure AutoCompleteNewComponent;
    procedure AutoCompleteNewComponentUnitName;
    function SwitchRelativeAbsoluteFilename(const Filename: string): string;
    function FindFileInFilesList(AFilename: string): Integer;
    procedure LoadComponentIcon(AFilename: string);
  public
    Params: TAddToPkgResult;
    procedure UpdateAvailableAncestorTypes;
    procedure UpdateAvailablePageNames;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
    property OnGetIDEFileInfo: TGetIDEFileStateEvent read FOnGetIDEFileInfo
                                                     write FOnGetIDEFileInfo;
    property OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo
                       read FOnGetUnitRegisterInfo write FOnGetUnitRegisterInfo;
    property ActivatePage: TAddToPkgType read GetActivatePage write SetActivatePage;
  end;
  
function ShowAddToPackageDlg(Pkg: TLazPackage; out Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent;
  OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
  var Page: TAddToPkgType): TModalResult;


implementation

{$R *.lfm}

function ShowAddToPackageDlg(Pkg: TLazPackage; out Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent;
  OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo; var Page: TAddToPkgType
  ): TModalResult;
var
  AddDlg: TAddToPackageDlg;
begin
  Params:=nil;
  AddDlg:=TAddToPackageDlg.Create(nil);
  AddDlg.OnGetIDEFileInfo:=OnGetIDEFileInfo;
  AddDlg.OnGetUnitRegisterInfo:=OnGetUnitRegisterInfo;
  AddDlg.LazPackage:=Pkg;
  AddDlg.ActivatePage:=Page;

  //auto press AddFiles if called with "Add files" page
  if Page=d2ptFiles then
    AddDlg.FilesDirButton.Click;
  //hide tabs for simpler use
  AddDlg.PageControl1.ShowTabs:=false;
  AddDlg.PageControl1.TabStop:=false;

  Result:=AddDlg.ShowModal;
  Page:=AddDlg.ActivatePage;
  if Result=mrOk then begin
    Params:=AddDlg.Params;
    AddDlg.Params:=nil;
  end;
  AddDlg.Free;
end;

{ TAddToPackageDlg }

procedure TAddToPackageDlg.AddToPackageDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddToPackageDlg.AddToPackageDlgKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
    ModalResult:=mrCancel;
end;

procedure TAddToPackageDlg.AncestorComboBoxChange(Sender: TObject);
begin
  CheckNewCompOk;
end;

procedure TAddToPackageDlg.AncestorComboBoxCloseUp(Sender: TObject);
begin
  if fLastNewComponentAncestorType<>AncestorComboBox.Text then
    AutoCompleteNewComponent;
end;

procedure TAddToPackageDlg.AncestorShowAllCheckBoxClick(Sender: TObject);
begin
  UpdateAvailableAncestorTypes;
end;

procedure TAddToPackageDlg.CancelAddFileButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAddToPackageDlg.CancelAddUnitButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAddToPackageDlg.ClassNameEditChange(Sender: TObject);
begin
  AutoCompleteNewComponentUnitName;
  CheckNewCompOk;
end;

procedure TAddToPackageDlg.ComponentIconBitBtnClick(Sender: TObject);
var
  Dlg: TOpenPictureDialog;
begin
  Dlg:=TOpenPictureDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(Dlg);
    Dlg.InitialDir:=LazPackage.GetFileDialogInitialDir(ExtractFilePath(ComponentUnitFileEdit.Text));
    Dlg.Title:=lisTitleOpenComponentIcon24x24;
    Dlg.Options:=Dlg.Options+[ofPathMustExist];
    Dlg.Filter:=Format('%s|*.png|%s|*.bmp|%s|*.xpm|%s|%s',
      [dlgFilterImagesPng,
       dlgFilterImagesBitmap,
       dlgFilterImagesPixmap,
       dlgFilterAll, GetAllFilesMask]);

    if Dlg.Execute then begin
      LoadComponentIcon(Dlg.FileName);
    end;
    InputHistories.StoreFileDialogSettings(Dlg);
  finally
    Dlg.Free;
  end;
end;

procedure TAddToPackageDlg.ComponentUnitFileBrowseButtonClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  AFilename: string;
begin
  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.InitialDir := LazPackage.GetFileDialogInitialDir(SaveDialog.InitialDir);
    SaveDialog.Title := lisSaveAs;
    SaveDialog.Options := SaveDialog.Options+[ofPathMustExist];
    SaveDialog.Filter := Format('%s|*.pas;*.pp', [dlgFilterPascalFile]);
    if SaveDialog.Execute then begin
      AFilename := CleanAndExpandFilename(SaveDialog.Filename);
      if FilenameIsPascalUnit(AFilename) then begin
        LazPackage.ShortenFilename(AFilename,true);
        ComponentUnitFileEdit.Text := AFilename;
      end else begin
        IDEMessageDialog(lisA2PInvalidFile,
         lisA2PAPascalUnitMustHaveTheExtensionPPOrPas,
         mtError,[mbCancel]);
      end;
    end;
    InputHistories.StoreFileDialogSettings(SaveDialog);
  finally
    SaveDialog.Free;
  end;
end;

procedure TAddToPackageDlg.ComponentUnitFileShortenButtonClick(Sender: TObject);
begin
  if ''=ComponentUnitFileEdit.Text then exit;
  ComponentUnitFileEdit.Text:=SwitchRelativeAbsoluteFilename(ComponentUnitFileEdit.Text);
end;

procedure TAddToPackageDlg.ComponentUnitNameEditChange(Sender: TObject);
begin
  CheckNewCompOk;
end;

procedure TAddToPackageDlg.FilesAddButtonClick(Sender: TObject);
var
  i: Integer;
  Filename: String;
  NewFileType: TPkgFileType;
  HasRegisterProc: boolean;
  CurParams, LastParams: TAddToPkgResult;
  ok: Boolean;
begin
  ok:=false;
  try
    LastParams:=nil;
    i:=0;
    while i<FilesListView.Items.Count do begin
      if not FilesListView.Items[i].Selected then begin
        Inc(i);
        Continue;
      end;
      Filename:=FilesListView.Items[i].Caption;
      LazPackage.LongenFilename(Filename);
      Assert(not DirPathExists(Filename));
      if LazPackage.FindPkgFile(Filename,true,false)<>nil then begin
        FilesListView.Items.Delete(i);     // file already in package
        continue;
      end;

      if LastParams<>nil then begin
        LastParams.Next:=TAddToPkgResult.Create;
        CurParams:=LastParams.Next;
      end else
        CurParams:=Params;
      CurParams.Clear;
      CurParams.AddType:=d2ptFile;
      CurParams.UnitFilename:=Filename;
      NewFileType:=FileNameToPkgFileType(Filename);
      CurParams.FileType:=NewFileType;
      if NewFileType=pftUnit then begin
        CurParams.AddType:=d2ptUnit;
        Include(CurParams.PkgFileFlags,pffAddToPkgUsesSection);

        // check filename
        if not CheckAddingPackageUnit(LazPackage,CurParams.AddType,
          OnGetIDEFileInfo,CurParams.UnitFilename)
        then begin
          FilesListView.Items.Delete(i);
          exit;
        end;

        CurParams.AutoAddLFMFile:=true;
        CurParams.AutoAddLRSFile:=true;
        if Assigned(OnGetUnitRegisterInfo) then begin
          OnGetUnitRegisterInfo(Self,Filename,CurParams.Unit_Name,HasRegisterProc);
          if HasRegisterProc then
            Include(CurParams.PkgFileFlags,pffHasRegisterProc);
        end;

        // check unitname
        if CompareText(CurParams.Unit_Name,
          ExtractFileNameOnly(CurParams.UnitFilename))<>0
        then begin
          if IDEMessageDialog(lisA2PInvalidUnitName,
              Format(lisA2PTheUnitNameAndFilenameDiffer,
                     [CurParams.Unit_Name, LineEnding, CurParams.UnitFilename]),
            mtError,[mbIgnore,mbCancel])<>mrIgnore
          then begin
            FilesListView.Items.Delete(i);
            exit;
          end;
        end;
      end;
      LastParams:=CurParams;
      inc(i);
    end;
    ButtonPanel1.OKButton.Enabled:=FilesListView.SelCount>0;
    ok:=LastParams<>nil;
  finally
    if not ok then Params.Clear;
  end;
  if LastParams=nil then begin
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.FilesDeleteButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=FilesListView.Items.Count-1 downto 0 do
    if FilesListView.Items[i].Selected then
      FilesListView.Items.Delete(i);
  CheckFilesButtonsOk;
end;

procedure TAddToPackageDlg.FilesDirButtonClick(Sender: TObject);
var
  i: Integer;
  Files: TStrings;
  AFilename: string;
  NewListItem: TListItem;
  NewPgkFileType: TPkgFileType;
begin
  Files:=nil;
  try
    if ShowAddDirToPkgDialog(LazPackage,Files)<>mrOk then exit;
    for i:=0 to Files.Count-1 do begin
      AFilename:=Files[i];
      if FindFileInFilesList(AFilename)<0 then begin
        LazPackage.ShortenFilename(AFilename,true);
        NewListItem:=FilesListView.Items.Add;
        NewListItem.Caption:=AFilename;
        NewPgkFileType:=FileNameToPkgFileType(AFilename);
        NewListItem.SubItems.Add(GetPkgFileTypeLocalizedName(NewPgkFileType));
        NewListItem.Selected:=True;
      end;
    end;
    CheckFilesButtonsOk;
  finally
    Files.Free;
  end;
end;

procedure TAddToPackageDlg.FilesListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  CheckFilesButtonsOk;
end;

procedure TAddToPackageDlg.FilesShortenButtonClick(Sender: TObject);
var
  SwitchToAbsolute: Boolean;
  i: Integer;
  Filename: String;
begin
  if FilesListView.Items.Count=0 then exit;
  if (not LazPackage.HasDirectory)
  or (not FilenameIsAbsolute(LazPackage.Directory)) then exit;
  SwitchToAbsolute:=not FilenameIsAbsolute(FilesListView.Items[0].Caption);
  for i:=0 to FilesListView.Items.Count-1 do begin
    Filename:=FilesListView.Items[i].Caption;
    if SwitchToAbsolute then
      Filename:=CreateAbsolutePath(Filename,LazPackage.Directory)
    else
      Filename:=CreateRelativePath(Filename,LazPackage.Directory);
    FilesListView.Items[i].Caption:=Filename;
  end;
end;

function TAddToPackageDlg.CheckFilesButtonsOk: Boolean;
begin
  FilesDeleteButton.Enabled:=FilesListView.SelCount>0;
  Result:=FilesListView.Items.Count>0;
  FilesShortenButton.Enabled:=Result;
  ButtonPanel1.OKButton.Enabled:=FilesListView.SelCount>0;
end;

procedure TAddToPackageDlg.FormCreate(Sender: TObject);
begin
  Caption:=lisA2PAddToPackage;
  fPkgComponents:=TAVLTree.Create(@CompareIDEComponentByClassName);
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  Params:=TAddToPkgResult.Create;

  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  SetupComponents;
end;

procedure TAddToPackageDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fPkgComponents);
  FreeAndNil(fPackages);
  FreeAndNil(Params);
end;

procedure TAddToPackageDlg.FormShow(Sender: TObject);
begin
  SelectNext(PageControl1.ActivePage, True, True);
end;

procedure TAddToPackageDlg.NewComponentButtonClick(Sender: TObject);
var
  PkgFile: TPkgFile;
  PkgComponent: TPkgComponent;
  ARequiredPackage: TLazPackage;
begin
  Params.Clear;
  Params.AddType:=d2ptNewComponent;
  Params.FileType:=pftUnit;
  Params.PkgFileFlags:=[pffHasRegisterProc,pffAddToPkgUsesSection];
  Params.AncestorType:=AncestorComboBox.Text;
  Params.NewClassName:=ClassNameEdit.Text;
  Params.PageName:=PalettePageCombobox.Text;
  Params.Unit_Name:=ComponentUnitNameEdit.Text;
  Params.UnitFilename:=ComponentUnitFileEdit.Text;
  Params.UsedUnitname:='';
  Params.IconFile:=FComponentIconFilename;

  // check Ancestor Type
  if not IsValidIdent(Params.AncestorType) then begin
    IDEMessageDialog(lisA2PInvalidAncestorType,
      Format(lisA2PTheAncestorTypeIsNotAValidPascalIdentifier, [Params.AncestorType]),
      mtError,[mbCancel]);
    exit;
  end;

  // check pagename
  if length(Params.PageName)>100 then begin
    IDEMessageDialog(lisA2PPageNameTooLong,
      Format(lisA2PThePageNameIsTooLongMax100Chars, [Params.PageName]),
      mtError,[mbCancel]);
    exit;
  end;

  // check unitname - filename redundancy
  if CompareText(Params.Unit_name,ExtractFileNameOnly(Params.UnitFilename))<>0
  then begin
    IDEMessageDialog(lisA2PUnitNameInvalid,
      Format(lisA2PTheUnitNameDoesNotCorrespondToTheFilename, [Params.Unit_Name]),
      mtError,[mbCancel]);
    exit;
  end;

  // check classname
  if not IsValidIdent(Params.NewClassName) then begin
    IDEMessageDialog(lisA2PInvalidClassName,
      Format(lisA2PTheClassNameIsNotAValidPascalIdentifier, [Params.NewClassName]),
      mtError,[mbCancel]);
    exit;
  end;

  // check classname<>ancestortype
  if CompareText(Params.NewClassName,Params.AncestorType)=0 then begin
    IDEMessageDialog(lisA2PInvalidCircularDependency,
      Format(lisA2PTheClassNameAndAncestorTypeAreTheSame,[Params.NewClassName,Params.AncestorType]),
      mtError,[mbCancel]);
    exit;
  end;

  // check ancestor type is not unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.AncestorType,true,true);
  if PkgFile<>nil then begin
    if IDEMessageDialog(lisA2PAmbiguousAncestorType,
      Format(lisA2PTheAncestorTypeHasTheSameNameAsTheUnit,
             [Params.AncestorType, LineEnding, PkgFile.Filename]),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check classname does not interfere with an existing unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.NewClassName,true,true);
  if PkgFile<>nil then begin
    if IDEMessageDialog(lisA2PAmbiguousClassName,
      Format(lisA2PTheClassNameHasTheSameNameAsTheUnit,
             [Params.AncestorType, LineEnding, PkgFile.Filename]),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check if classname already exists
  PkgComponent:=TPkgComponent(IDEComponentPalette.FindComponent(Params.NewClassname));
  if PkgComponent<>nil then begin
    if IDEMessageDialog(lisA2PClassNameAlreadyExists,
      Format(lisA2PTheClassNameExistsAlreadyInPackageFile, [Params.NewClassName, LineEnding,
        PkgComponent.PkgFile.LazPackage.IDAsString, LineEnding, PkgComponent.PkgFile.Filename]),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check filename
  if not CheckAddingPackageUnit(LazPackage,Params.AddType,
    OnGetIDEFileInfo,Params.UnitFilename) then exit;

  // create dependency if needed
  PkgComponent:=TPkgComponent(IDEComponentPalette.FindComponent(Params.AncestorType));
  if PkgComponent<>nil then begin
    Params.UsedUnitname:=PkgComponent.GetUnitName;
    ARequiredPackage:=PkgComponent.PkgFile.LazPackage;
    ARequiredPackage:=TLazPackage(PackageEditingInterface.RedirectPackageDependency(ARequiredPackage));
    if (LazPackage<>ARequiredPackage)
    and (not LazPackage.Requires(PkgComponent.PkgFile.LazPackage))
    then
      Params.Dependency:=ARequiredPackage.CreateDependencyWithOwner(nil);
  end;
  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.NewComponentPageResize(Sender: TObject);
var
  x: Integer;
begin
  x:=0;
  x:=Max(x,AncestorTypeLabel.Left+AncestorTypeLabel.Width);
  x:=Max(x,ClassNameLabel.Left+ClassNameLabel.Width);
  x:=Max(x,PalettePageLabel.Left+PalettePageLabel.Width);
  x:=Max(x,ComponentUnitFileLabel.Left+ComponentUnitFileLabel.Width);
  x:=Max(x,ComponentUnitNameLabel.Left+ComponentUnitNameLabel.Width);
  x:=Max(x,ComponentIconLabel.Left+ComponentIconLabel.Width);
  AncestorComboBox.Left:=x+6;
end;

procedure TAddToPackageDlg.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  Params.Pkg:=FLazPackage;
  UpdateAvailableAncestorTypes;
  UpdateAvailablePageNames;
end;

function TAddToPackageDlg.GetActivatePage: TAddToPkgType;
begin
  if PageControl1.ActivePage=NewComponentPage then
    Result:=d2ptNewComponent
  else {if PageControl1.ActivePage=AddFilesPage then } begin
    Assert(PageControl1.ActivePage=AddFilesPage,
      'TAddToPackageDlg.GetActivatePage: PageControl1.ActivePage <> AddFilesPage');
    Result:=d2ptFiles;
  end;
end;

procedure TAddToPackageDlg.SetActivatePage(AValue: TAddToPkgType);
begin
  case AValue of
  d2ptNewComponent: PageControl1.ActivePage:=NewComponentPage;
  d2ptFiles: PageControl1.ActivePage:=AddFilesPage;
  else raise Exception.Create('TAddToPackageDlg.SetActivatePage: invalid value.');
  end;
end;

function TAddToPackageDlg.CheckNewCompOk: Boolean;
begin
  Result:=(AncestorComboBox.Text<>'') and (ClassNameEdit.Text<>'') and (ComponentUnitNameEdit.Text<>'');
  ButtonPanel1.OKButton.Enabled:=Result;
end;

procedure TAddToPackageDlg.PageControl1Change(Sender: TObject);
begin
  case PageControl1.PageIndex of
    0: begin              // New Component
      ButtonPanel1.OkButton.Caption:=lisA2PCreateNewComp;
      ButtonPanel1.OkButton.OnClick:=@NewComponentButtonClick;
      CheckNewCompOk;
    end;
    1: begin              // Add Files
      ButtonPanel1.OkButton.Caption:=lisA2PAddFilesToPackage;
      ButtonPanel1.OkButton.OnClick:=@FilesAddButtonClick;
      CheckFilesButtonsOk;
    end;
  end;
end;

procedure TAddToPackageDlg.SetupComponents;
begin
  NewComponentPage.Caption:=lisA2PNewComponent;
  AddFilesPage.Caption:=lisA2PAddFiles;
  ButtonPanel1.CancelButton.Caption:=lisCancel;
  PageControl1.PageIndex:=0;
  PageControl1Change(PageControl1);
  SetupNewComponentPage;
  SetupAddFilesPage;
end;

procedure TAddToPackageDlg.SetupNewComponentPage;
begin
  AncestorTypeLabel.Caption:=lisA2PAncestorType;
  AncestorComboBox.Text:='';
  AncestorShowAllCheckBox.Caption:=lisA2PShowAll;
  ClassNameLabel.Caption:=lisA2PNewClassName;
  ClassNameEdit.Text:='';
  PalettePageLabel.Caption:=lisA2PPalettePage;
  PalettePageCombobox.Text:='';
  ComponentUnitFileLabel.Caption:=lisA2PUnitFileName2;
  ComponentUnitFileEdit.Text:='';
  with ComponentUnitFileBrowseButton do begin
    Caption:='...';
    ShowHint:=true;
    Hint:=lisA2PSaveFileDialog;
  end;
  with ComponentUnitFileShortenButton do begin
    Caption:='<>';
    ShowHint:=true;
    Hint:=lisA2PShortenOrExpandFilename;
  end;
  ComponentUnitNameLabel.Caption:=lisA2PUnitName;
  ComponentUnitNameEdit.Text:='';
  ComponentIconLabel.Caption:=lisA2PIconAndSize;
  ComponentIconBitBtn.Width:=ComponentPaletteBtnWidth;
  ComponentIconBitBtn.Height:=ComponentPaletteBtnHeight;
end;

procedure TAddToPackageDlg.SetupAddFilesPage;
var
  CurColumn: TListColumn;
begin
  with FilesListView do begin
    CurColumn:=Columns[0];
    CurColumn.Width:=200;
    CurColumn.Caption:=lisA2PFilename2;
    CurColumn:=Columns[1];
    CurColumn.Caption:=dlgEnvType;
  end;
  
  with FilesDirButton do begin
    Caption:=lisAddFilesInDirectory;
    LoadGlyphFromResourceName(HInstance, 'pkg_files');
  end;

  with FilesShortenButton do begin
    Caption:=lisA2PSwitchPaths;
    ShowHint:=true;
    Hint:=lisToggleShowingFilenamesWithFullPathOrWithRelativePa;
  end;

  with FilesDeleteButton do begin
    Caption:=lisDelete;
    ShowHint:=true;
    Hint:=lisDeleteSelectedFiles;
    LoadGlyphFromResourceName(HInstance, 'laz_delete');
  end;

  LabelIconInfo.Caption:=lisNoneClickToChooseOne;
end;

procedure TAddToPackageDlg.OnIterateComponentClasses(PkgComponent: TPkgComponent);
begin
  if fPkgComponents.Find(PkgComponent)=nil then
    fPkgComponents.Add(PkgComponent);
end;

procedure TAddToPackageDlg.AutoCompleteNewComponent;
var
  PkgComponent: TPkgComponent;
begin
  fLastNewComponentAncestorType:=AncestorComboBox.Text;
  if not IsValidIdent(fLastNewComponentAncestorType) then exit;
  PkgComponent:=TPkgComponent(
    IDEComponentPalette.FindComponent(fLastNewComponentAncestorType));

  // create unique classname
  if not IsValidIdent(ClassNameEdit.Text) then
    ClassNameEdit.Text:=IDEComponentPalette.CreateNewClassName(
                                                 fLastNewComponentAncestorType);
  // choose the same page name
  if (PalettePageCombobox.Text='')
  and (PkgComponent<>nil) and (PkgComponent.RealPage<>nil) then
    PalettePageCombobox.Text:=PkgComponent.RealPage.PageName;
  // filename
  AutoCompleteNewComponentUnitName;
  ButtonPanel1.OkButton.Enabled:=True;
end;

procedure TAddToPackageDlg.AutoCompleteNewComponentUnitName;
var
  CurClassName: String;
  NewUnitName: String;
  NewFileName: String;
begin
  // check if update needed
  CurClassName:=ClassNameEdit.Text;
  if fLastNewComponentClassName=CurClassName then exit;
  fLastNewComponentClassName:=CurClassName;

  // check classname
  if not IsValidIdent(CurClassName) then exit;

  // create unitname
  NewUnitName:=CurClassName;
  if NewUnitName[1]='T' then
    NewUnitName:=copy(NewUnitName,2,length(NewUnitName)-1);
  NewUnitName:=PackageGraph.CreateUniqueUnitName(NewUnitName);
  ComponentUnitNameEdit.Text:=NewUnitName;

  // create filename
  NewFileName:=NewUnitName;

  if EnvironmentOptions.CharcaseFileAction in [ccfaAsk, ccfaAutoRename] then
    NewFileName:=lowercase(NewFileName);

  // append pascal file extension
  NewFileName:=NewFileName
       +EnvironmentOpts.PascalExtension[EnvironmentOptions.PascalFileExtension];
  // prepend path
  if LazPackage.HasDirectory then
    NewFileName:=LazPackage.Directory+NewFileName;
  ComponentUnitFileEdit.Text:=NewFileName;
end;

function TAddToPackageDlg.SwitchRelativeAbsoluteFilename(const Filename: string): string;
begin
  Result:=Filename;
  if (not LazPackage.HasDirectory)
  or (not FilenameIsAbsolute(LazPackage.Directory)) then exit;
  if FilenameIsAbsolute(Filename) then
    Result:=TrimFilename(CreateRelativePath(Filename,LazPackage.Directory))
  else
    Result:=TrimFilename(CreateAbsoluteSearchPath(Filename,LazPackage.Directory));
end;

function TAddToPackageDlg.FindFileInFilesList(AFilename: string): Integer;
var
  i: Integer;
  Item: TListItem;
  OtherFilename: String;
begin
  if not FilenameIsAbsolute(AFilename) then
    LazPackage.LongenFilename(AFilename);
  for i:=0 to FilesListView.Items.Count-1 do begin
    Item:=FilesListView.Items[i];
    OtherFilename:=Item.Caption;
    if not FilenameIsAbsolute(OtherFilename) then
      LazPackage.LongenFilename(OtherFilename);
    if CompareFilenames(AFilename,OtherFilename)=0 then begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
end;

procedure TAddToPackageDlg.LoadComponentIcon(AFilename: string);
var
  ShortFilename: String;
  Image: TImage;
begin
  try
    Image:=TImage.Create(nil);
    try
      Image.Picture.LoadFromFile(AFilename);
      ComponentIconBitBtn.Glyph.Assign(Image.Picture.Graphic);
      ShortFilename:=AFilename;
      LazPackage.ShortenFilename(ShortFilename,true);
      LabelIconInfo.Caption:= Format('%s (%dx%d)',
        [ShortFilename, ComponentIconBitBtn.Glyph.Width, ComponentIconBitBtn.Glyph.Height]);
      FComponentIconFilename:=AFilename;
    finally
      Image.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisErrorLoadingFile2,[AFilename]) + LineEnding + E.Message,
        mtError, [mbCancel]);
      ComponentIconBitBtn.Glyph.Clear;
      FComponentIconFilename:='';
      LabelIconInfo.Caption:=lisNoneClickToChooseOne;
    end;
  end;
end;

procedure TAddToPackageDlg.UpdateAvailableAncestorTypes;
var
  ANode: TAVLTreeNode;
  sl: TStringList;
  OldAncestorType: String;
begin
  // get all available registered components
  fPkgComponents.Clear;
  if AncestorShowAllCheckBox.Checked then begin
    PackageGraph.IterateAllComponentClasses(@OnIterateComponentClasses);
  end else begin
    PackageGraph.IterateComponentClasses(LazPackage,@OnIterateComponentClasses,
                                         true,true);
  end;
  // put them into a list
  sl:=TStringList.Create;
  ANode:=fPkgComponents.FindLowest;
  while ANode<>nil do begin
    sl.Add(TPkgComponent(ANode.Data).ComponentClass.ClassName);
    ANode:=fPkgComponents.FindSuccessor(ANode);
  end;
  // add at least TComponent
  sl.Add('TComponent');
  sl.Sort;
  
  // put them into the combobox
  OldAncestorType:=AncestorComboBox.Text;
  AncestorComboBox.Items.Assign(sl);
  AncestorComboBox.Text:=OldAncestorType;
  sl.Free;
end;

procedure TAddToPackageDlg.UpdateAvailablePageNames;
var
  i: Integer;
  APageName: String;
  sl: TStringList;
begin
  // get all current pagenames (excluding the hidden page)
  sl:=TStringList.Create;
  for i:=0 to IDEComponentPalette.Pages.Count-1 do begin
    APageName:=IDEComponentPalette.Pages[i].PageName;
    if APageName<>'' then
      sl.Add(APageName);
  end;
  sl.Sort;
  PalettePageCombobox.Items.Assign(sl);
  sl.Free;
end;

{ TAddToPkgResult }

procedure TAddToPkgResult.Clear;
begin
  AddType:=d2ptUnit;
  Dependency:=nil;
  UnitFilename:='';
  Unit_Name:='';
  AncestorType:='';
  NewClassName:='';
  PageName:='';
  FileType:=pftUnit;
  PkgFileFlags:=[];
  UsedUnitname:='';
  AutoAddLFMFile:=false;
  AutoAddLRSFile:=false;
  FreeThenNil(Next);
end;

destructor TAddToPkgResult.Destroy;
begin
  FreeThenNil(Next);
  inherited Destroy;
end;

end.

