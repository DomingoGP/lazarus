{ TreeFilterEdit

  Copyright (C) 2012 Lazarus team

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

}
unit TreeFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, ComCtrls, EditBtn,
  LResources, LCLType, LCLProc, FileUtil, LazUTF8, AvgLvlTree, fgl;

type
  TImageIndexEvent = function (Str: String; Data: TObject;
                               var AIsEnabled: Boolean): Integer of object;

  TTreeFilterEdit = class;

  { TTreeFilterBranch }

  // A branch in the tree which can be sorted and filtered
  TTreeFilterBranch = class
  private
    fOwner: TTreeFilterEdit;
    fRootNode: TTreeNode;
    fOriginalData: TStringList;     // Data supplied by caller.
    fSortedData: TStringList;       // Data sorted for viewing.
    fImgIndex: Integer;
    // Full filename in node data is needed when showing the directory hierarchy.
    // It is stored automatically if AFullFilename is passed to contructor.
    fNodeTextToFullFilenameMap: TStringToStringTree;
    fNodeTextToDataMap: TStringToPointerTree;
    fTVNodeStack: TList;
    function CompareFNs(AFilename1,AFilename2: string): integer;
    procedure SortAndFilter;
    procedure ApplyFilter;
    procedure TVDeleteUnneededNodes(p: integer);
    procedure TVClearUnneededAndCreateHierachy(Filename: string);
    procedure RemoveChildrenData(ARootNode : TTreeNode);
  public
    constructor Create(AOwner: TTreeFilterEdit; ARootNode: TTreeNode);
    destructor Destroy; override;
    procedure AddNodeData(ANodeText: string; AData: TObject; AFullFilename: string = '');
    procedure FreeNodeData(ANode: TTreeNode);
    function GetData(AIndex: integer): TObject;
    procedure Clear;
    procedure MoveFile(CurIndex, NewIndex: integer);
  end;

  TBranchList = specialize TFPGObjectList<TTreeFilterBranch>;

  { TTreeFilterEdit }

  TTreeFilterEdit = class(TCustomControlFilterEdit)
  private
    fFilteredTreeview: TCustomTreeview; // A control showing the (filtered) data.
    fImageIndexDirectory: integer;  // Needed if directory structure is shown.
    fSelectionList: TStringList;    // Store/restore the old selections here.
    fShowDirHierarchy: Boolean;     // Show directories / files as a tree structure.
    fBranches: TBranchList;         // Items under these nodes can be sorted.
    fExpandAllInitially: Boolean;   // Expand all levels when searched for the first time.
    fIsFirstTime: Boolean;          // Needed for fExpandAllInitially.
    fTempSelected: Boolean;
    fOnGetImageIndex: TImageIndexEvent;
    procedure SetFilteredTreeview(const AValue: TCustomTreeview);
    procedure SetShowDirHierarchy(const AValue: Boolean);
    function FilterTree(Node: TTreeNode): Boolean;
    procedure OnBeforeTreeDestroy(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MoveNext; override;
    procedure MovePrev; override;
    function ReturnKeyHandled: Boolean; override;
    procedure SortAndFilter; override;
    procedure ApplyFilterCore; override;
    function GetDefaultGlyph: TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
    function GetExistingBranch(ARootNode: TTreeNode): TTreeFilterBranch;
    function GetBranch(ARootNode: TTreeNode): TTreeFilterBranch;
    function DeleteBranch(ARootNode: TTreeNode): Boolean;
  public
    property ImageIndexDirectory: integer read fImageIndexDirectory write fImageIndexDirectory;
    property SelectionList: TStringList read fSelectionList;
    property ShowDirHierarchy: Boolean read fShowDirHierarchy write SetShowDirHierarchy;
  published
    property FilteredTreeview: TCustomTreeview read fFilteredTreeview write SetFilteredTreeview;
    property ExpandAllInitially: Boolean read fExpandAllInitially write fExpandAllInitially default False;
    property OnGetImageIndex: TImageIndexEvent read fOnGetImageIndex write fOnGetImageIndex;
  end;

  { TTFENodeData - TreeFilterEditNodeData }

  TTFENodeData = class
  public
    Node: TTreeNode;
    Branch: TTreeFilterBranch;
  end;

  { TFileNameItem }

  TFileNameItem = class(TTFENodeData)
  public
    Data: Pointer;
    Filename: string;
    constructor Create(AFilename: string; aData: Pointer);
  end;

var
  TreeFilterGlyph: TBitmap;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LazControls',[TTreeFilterEdit]);
end;

{ TTreeFilterBranch }

constructor TTreeFilterBranch.Create(AOwner: TTreeFilterEdit; ARootNode: TTreeNode);
begin
  inherited Create;
  fOwner:=AOwner;
  fRootNode:=ARootNode; // RootNode can also be Nil. Then all items are at top level.
  fOriginalData:=TStringList.Create;
  fSortedData:=TStringList.Create;
  fNodeTextToFullFilenameMap:=TStringToStringTree.Create(True);
  fNodeTextToDataMap:=TStringToPointerTree.Create(True);
  fImgIndex:=-1;
end;

destructor TTreeFilterBranch.Destroy;
begin
  Clear;
  FreeAndNil(fNodeTextToFullFilenameMap);
  FreeAndNil(fNodeTextToDataMap);
  FreeAndNil(fSortedData);
  FreeAndNil(fOriginalData);
  inherited Destroy;
end;

procedure TTreeFilterBranch.AddNodeData(ANodeText: string; AData: TObject; AFullFilename: string);
begin
  fOriginalData.AddObject(ANodeText, AData);
  fNodeTextToDataMap[ANodeText]:=AData;
  if AFullFilename <> '' then
    fNodeTextToFullFilenameMap[ANodeText]:=AFullFilename;
end;

function TTreeFilterBranch.GetData(AIndex: integer): TObject;
begin
  if AIndex<fSortedData.Count then
    Result:=fSortedData.Objects[AIndex]
  else
    Result:=Nil;
end;

function TTreeFilterBranch.CompareFNs(AFilename1,AFilename2: string): integer;
begin
  if fOwner.SortData then
    Result:=CompareFilenames(AFilename1, AFilename2)
  else if fOwner.fShowDirHierarchy then
    Result:=CompareFilenames(ExtractFilePath(AFilename1), ExtractFilePath(AFilename2))
  else
    Result:=0;
end;

procedure TTreeFilterBranch.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi, i: Integer;
  s: string;
begin
  fSortedData.Clear;
  for Origi:=0 to fOriginalData.Count-1 do begin
    s:=fOriginalData[Origi];
    if (fOwner.Filter='') or (Pos(fOwner.Filter,lowercase(s))>0) then begin
      i:=fSortedData.Count-1;
      while i>=0 do begin
        if CompareFNs(s,fSortedData[i])>=0 then break;
        dec(i);
      end;
      fSortedData.InsertObject(i+1, s, fOriginalData.Objects[Origi]);
    end;
  end;
end;

procedure TTreeFilterBranch.ApplyFilter;
var
  TVNode: TTreeNode;
  i: Integer;
  FileN, s: string;
  ena: Boolean;
  AObject: TObject;
begin
  if Assigned(fRootNode) then Begin
    Clear;
    fRootNode.DeleteChildren;      // Delete old tree nodes.
  end
  else
    fOwner.fFilteredTreeview.Items.Clear;
  if fOwner.ShowDirHierarchy then
    fTVNodeStack:=TList.Create;
  for i:=0 to fSortedData.Count-1 do begin
    FileN:=fSortedData[i];
    if fOwner.ShowDirHierarchy then begin
      TVClearUnneededAndCreateHierachy(FileN);
      TVNode:=TTreeNode(fTVNodeStack[fTVNodeStack.Count-1]);
    end
    else
      TVNode:=fOwner.fFilteredTreeview.Items.AddChild(fRootNode,FileN);
    // Save the long filename to Node.Data
    AObject := TObject(fNodeTextToDataMap[FileN]);
    if fNodeTextToFullFilenameMap.Count > 0 then begin
      s:=FileN;
      if fNodeTextToFullFilenameMap.Contains(FileN) then
        s:=fNodeTextToFullFilenameMap[FileN];           // Full file name.
      AObject := TFileNameItem.Create(s, AObject);
    end;
    If Assigned(AObject) and (AObject is TTFENodeData) then Begin
      TTFENodeData(AObject).Node := TVNode;
      TTFENodeData(AObject).Branch := Self;
    end;
    TVNode.Data:=AObject;
    // Get ImageIndex for Node
    ena := True;
    if Assigned(fOwner.OnGetImageIndex) then
      fImgIndex:=fOwner.OnGetImageIndex(FileN, fSortedData.Objects[i], ena);
    TVNode.ImageIndex:=fImgIndex;
    TVNode.SelectedIndex:=fImgIndex;
  end;
  if fOwner.ShowDirHierarchy then
    fTVNodeStack.Free;
  if Assigned(fRootNode) then
    fRootNode.Expanded:=True;
end;

procedure TTreeFilterBranch.TVDeleteUnneededNodes(p: integer);
// delete all nodes behind the nodes in the stack, and depth>=p
var
  i: Integer;
  Node: TTreeNode;
begin
  for i:=fTVNodeStack.Count-1 downto p do begin
    Node:=TTreeNode(fTVNodeStack[i]);
    while Node.GetNextSibling<>nil do
      Node.GetNextSibling.Free;
  end;
  fTVNodeStack.Count:=p;
end;

procedure TTreeFilterBranch.TVClearUnneededAndCreateHierachy(Filename: string);
// TVNodeStack contains a path of TTreeNode for the filename
var
  DelimPos: Integer;
  FilePart: String;
  Node: TTreeNode;
  p: Integer;
begin
  p:=0;
  while Filename<>'' do begin
    // get the next file name part
    DelimPos:=System.Pos(PathDelim,Filename);
    if DelimPos>0 then begin
      FilePart:=copy(Filename,1,DelimPos-1);
      Filename:=copy(Filename,DelimPos+1,length(Filename));
    end else begin
      FilePart:=Filename;
      Filename:='';
    end;
    //debugln(['ClearUnneededAndCreateHierachy FilePart=',FilePart,' Filename=',Filename,' p=',p]);
    if p < fTVNodeStack.Count then begin
      Node:=TTreeNode(fTVNodeStack[p]);
      if (FilePart=Node.Text) and (Node.Data=nil) then begin
        // same sub directory
      end
      else begin
        // change directory => last directory is complete
        // => delete unneeded nodes after last path
        TVDeleteUnneededNodes(p+1);
        if Node.GetNextSibling<>nil then begin
          Node:=Node.GetNextSibling;
          Node.Text:=FilePart;
        end
        else
          Node:=fOwner.fFilteredTreeview.Items.Add(Node,FilePart);
        fTVNodeStack[p]:=Node;
      end;
    end else begin
      // new sub node
      Assert(p=fTVNodeStack.Count, Format('TVClearUnneededAndCreateHierachy: p (%d) > fTVNodeStack.Count (%d).',
                                          [p, fTVNodeStack.Count]));
      if p>0 then
        Node:=TTreeNode(fTVNodeStack[p-1])
      else
        Node:=fRootNode;
      Assert(Assigned(Node), Format('TVClearUnneededAndCreateHierachy: Node=nil, p=%d', [p]));
      if Node.GetFirstChild<>nil then begin
        Node:=Node.GetFirstChild;
        Node.Text:=FilePart;
      end
      else
        Node:=fOwner.fFilteredTreeview.Items.AddChild(Node,FilePart);
      fTVNodeStack.Add(Node);
    end;
    if (Filename<>'') then begin
      Node.ImageIndex:=fOwner.ImageIndexDirectory;
      Node.SelectedIndex:=Node.ImageIndex;
    end;
    inc(p);
  end;
end;

procedure TTreeFilterBranch.FreeNodeData(ANode : TTreeNode);
Var
  AObject : TObject;
Begin
  If Assigned(ANode) And Assigned(ANode.Data) Then Begin
    AObject := TObject(ANode.Data);
    If AObject.InheritsFrom(TTFENodeData) Then Begin
      TTFENodeData(AObject).Node := NIL;
      TTFENodeData(AObject).Branch := NIL;
    end;
    If AObject is TFileNameItem Then
      AObject.Free;
    ANode.Data := NIL;
  end;
end;

procedure TTreeFilterBranch.RemoveChildrenData(ARootNode : TTreeNode);

  procedure ProcessSubNodes(ANode : TTreeNode);
  Var
    BNode : TTreeNode;
  begin
    FreeNodeData(ANode);
    BNode := ANode.GetFirstChild;
    While Assigned(BNode) Do Begin
      ProcessSubNodes(BNode);
      BNode := BNode.GetNextSibling;
    end;
  end;

Begin
  if ARootNode=nil then exit;
  ProcessSubNodes(ARootNode);
end;

procedure TTreeFilterBranch.Clear;
Begin
  RemoveChildrenData(fRootNode);
end;

procedure TTreeFilterBranch.MoveFile(CurIndex, NewIndex: integer);
begin
  fOriginalData.Move(CurIndex, NewIndex);
end;

{ TFileNameItem }

constructor TFileNameItem.Create(AFilename: string; aData: Pointer);
begin
  Filename:=AFilename;
  Data:=aData;
end;

{ TTreeFilterEdit }

constructor TTreeFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSelectionList:=TStringList.Create;
  fImageIndexDirectory := -1;
  fExpandAllInitially := False;
  fIsFirstTime := True;
end;

destructor TTreeFilterEdit.Destroy;
begin
  FilteredTreeview:=nil;
  FreeAndNil(fBranches);
  FreeAndNil(fSelectionList);
  inherited Destroy;
end;

function TTreeFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := TreeFilterGlyph;
end;

procedure TTreeFilterEdit.OnBeforeTreeDestroy(Sender: TObject);
begin
  FreeAndNil(fBranches);
end;

procedure TTreeFilterEdit.SetFilteredTreeview(const AValue: TCustomTreeview);
begin
  if fFilteredTreeview = AValue then Exit;
  if fFilteredTreeview <> nil then begin
    fFilteredTreeview.RemoveFreeNotification(Self);
    fFilteredTreeview.RemoveHandlerOnBeforeDestruction(@OnBeforeTreeDestroy);
  end;
  fFilteredTreeview := AValue;
  if fFilteredTreeview <> nil then begin
    fFilteredTreeview.FreeNotification(Self);
    fFilteredTreeview.AddHandlerOnBeforeDestruction(@OnBeforeTreeDestroy);
  end;
end;

procedure TTreeFilterEdit.SetShowDirHierarchy(const AValue: Boolean);
begin
  if fShowDirHierarchy=AValue then exit;
  if not Assigned(fFilteredTreeview) then
    raise Exception.Create('Showing directory hierarchy requires Treeview.');
  fShowDirHierarchy:=AValue;
end;

function TTreeFilterEdit.FilterTree(Node: TTreeNode): Boolean;
// Filter all tree branches recursively, setting Node.Visible as needed.
// Returns True if Node or its siblings or child nodes have visible items.
var
  Pass, Done: Boolean;
begin
  Result:=False;
  Done:=False;
  while Node<>nil do
  begin
    // Call OnFilterItem handler.
    if Assigned(OnFilterItem) then
      Pass:=OnFilterItem(TObject(Node.Data), Done)
    else
      Pass:=False;
    // Filter by item's title text if needed.
    if not (Pass or Done) then
      Pass:=(Filter='') or (Pos(Filter,UTF8LowerCase(Node.Text))>0);
    // Recursive call for child nodes.
    Node.Visible:=FilterTree(Node.GetFirstChild) or Pass;
    if Node.Visible then
      Result:=True;
    Node:=Node.GetNextSibling;
  end;
end;

procedure TTreeFilterEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (FilteredTreeview=AComponent) then
  begin
    IdleConnected:=False;
    fNeedUpdate:=False;
    fFilteredTreeview.RemoveHandlerOnBeforeDestruction(@OnBeforeTreeDestroy);
    fFilteredTreeview:=nil;
    FreeAndNil(fBranches);
  end;
end;

procedure TTreeFilterEdit.SortAndFilter;
// Sort and filter either branches or the whole tree depending on operation mode.
var
  i: Integer;
begin
  if Assigned(fBranches) then begin      // Filter the brances
    for i:=0 to fBranches.Count-1 do
      fBranches[i].SortAndFilter;
  end
  else begin                  // Filter the whole tree (done in ApplyFilterCore).
    //
  end;
end;

procedure TTreeFilterEdit.ApplyFilterCore;
var
  i: Integer;
begin
  if fFilteredTreeview = nil then Exit;
  fFilteredTreeview.BeginUpdate;
  if Assigned(fBranches) then begin      // Apply filter for the branches
    for i:=0 to fBranches.Count-1 do
      fBranches[i].ApplyFilter;
  end
  else begin                             // Apply filter for the whole tree.
    if fExpandAllInitially and fIsFirstTime then begin
      fFilteredTreeview.FullExpand;
      fIsFirstTime := False;
    end;
    FilterTree(fFilteredTreeview.Items.GetFirstNode);
  end;
  fFilteredTreeview.ClearInvisibleSelection;
  fFilteredTreeview.EndUpdate;
end;

procedure TTreeFilterEdit.StoreSelection;
var
  ANode: TTreeNode;
begin
  if fFilteredTreeview = nil then Exit;
  ANode:=fFilteredTreeview.Selected;
  if (ANode=nil) then Exit;
  if fTempSelected and (ANode=fFilteredTreeview.Items.GetFirstVisibleNode) then Exit;
  fSelectionList.Clear;       // Clear old selection only if there is new one.
  fSelectionList.Add(ANode.Text);
end;

procedure TTreeFilterEdit.RestoreSelection;
var
  ANode: TTreeNode;
  CurText: string;
  i: Integer;
begin
  fTempSelected:=fSelectionList.Count>0;
  for i:=fSelectionList.Count-1 downto 0 do begin
    CurText:=fSelectionList[i];
    ANode:=fFilteredTreeview.Items.GetFirstVisibleNode;
    while (ANode<>nil) and (ANode.Text<>CurText) do
      ANode:=ANode.GetNextVisible;
    if ANode<>nil then begin                // Selection found
      fFilteredTreeview.Selected:=ANode;
      fSelectionList.Delete(i);
      fTempSelected:=False;
    end;
  end;
  if fTempSelected then  // Original selection will be restored later.
    fFilteredTreeview.Selected:=fFilteredTreeview.Items.GetFirstVisibleNode;
end;

function TTreeFilterEdit.GetExistingBranch(ARootNode: TTreeNode): TTreeFilterBranch;
// Get an existing branch for a given tree-node, or Nil if there is none.
var
  i: Integer;
begin
  Result := Nil;
  if not Assigned(fBranches) then Exit;
  for i := 0 to fBranches.Count-1 do
    if fBranches[i].fRootNode = ARootNode then begin
      Result := fBranches[i];
      Break;
    end;
end;

function TTreeFilterEdit.GetBranch(ARootNode: TTreeNode): TTreeFilterBranch;
// Get a new or existing branch for a given tree-node.
begin
  if not Assigned(fBranches) then
    fBranches := TBranchList.Create;
  Result := GetExistingBranch(ARootNode);
  if Assigned(Result) then
    Result.fOriginalData.Clear
  else begin
    Result := TTreeFilterBranch.Create(Self, ARootNode);
    fBranches.Add(Result);
  end;
end;

function TTreeFilterEdit.DeleteBranch(ARootNode: TTreeNode): Boolean;
// Delete the branch connected to a given root node. Returns True if found and deleted.
var
  i: Integer;
begin
  Result := False;
  if not Assigned(fBranches) then Exit;
  for i := 0 to fBranches.Count-1 do
    if fBranches[i].fRootNode = ARootNode then begin
      fBranches.Delete(i);
      Result := True;
      Break;
    end;
end;

procedure TTreeFilterEdit.MoveNext;
begin
  fFilteredTreeview.MoveToNextNode;
end;

procedure TTreeFilterEdit.MovePrev;
begin
  fFilteredTreeview.MoveToPrevNode;
end;

function TTreeFilterEdit.ReturnKeyHandled: Boolean;
// Retuns true if the Return press was forwarded to the Tree
var
  Key: Char;
begin
  Key:=Char(VK_RETURN);
  Result:=Assigned(fFilteredTreeview.OnKeyPress);
  if Result then
    fFilteredTreeview.OnKeyPress(fFilteredTreeview, Key);
end;

end.

