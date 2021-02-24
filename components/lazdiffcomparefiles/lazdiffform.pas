{
 /***************************************************************************
                              lazdiffform.pas
                              --------------


 ***************************************************************************/

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

  Author: Domingo Galmés
  some code borrowed from diffdialog.pas, author   Mattias Gaertner

  Abstract:
    The TDiffForm is a dialog for showing differences between two files.

}
unit lazdiffform;


{$mode objfpc}{$H+} {$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Buttons, StdCtrls, ExtCtrls, Dialogs, ComCtrls, LCLType,
  // LazUtils
  FileUtil, IntegerList,
  // SynEdit
  SynEdit, SynHighlighterDiff,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEImagesIntf,
  SrcEditorIntf,
  // IDE
  //LazarusIDEStrConsts,
  //EditorOptions,
  //InputHistory,
  //DiffPatch,
  //SourceEditor,
  //EnvironmentOpts,
  diff,
  Types, SynEditTypes,
  SynHighlighterPas, Graphics, SynGutterLineNumber;

type

  { TAvailableDiffFile }

  TAvailableDiffFile = class
  private
    Name: string;
    Editor: TSourceEditorInterface;
    SelectionAvailable: boolean;
  public
    constructor Create(const NewName: string; NewEditor: TSourceEditorInterface;
      NewSelectionAvailable: boolean);
  end;

  { TAvailableDiffFiles }

  TAvailableDiffFiles = class(TList)
  private
    function GetItems(Index: integer): TAvailableDiffFile;
    procedure SetItems(Index: integer; const AValue: TAvailableDiffFile);
  public
    procedure Clear; override;
    function Add(DiffFile: TAvailableDiffFile): integer;
    function IndexOfName(const Name: string): integer;
  public
    property Items[Index: integer]: TAvailableDiffFile read GetItems write SetItems;
      default;
  end;

  TTextDiffFlag2 = (
    tdfIgnoreCase,            // ignore case of letters
    tdfIgnoreEmptyLineChanges,// ignore if empty lines were added or removed
    tdfIgnoreHeadingSpaces,   // ignore spaces at start of line
    tdfIgnoreLineEnds,        // ignore if line end chars differ (e.g. #10 = #13#10)
    tdfIgnoreSpaceCharAmount, // ignore if space chars were added or removed
    // except if all spaces were removed
    tdfIgnoreSpaceChars,      // ignore spaces (newline chars not included)
    tdfIgnoreTrailingSpaces   // ignore spaces at end of line
    );
  TTextDiffFlags2 = set of TTextDiffFlag2;

  TDiffForm = class;

  { TSelectedDiffFile }

  TSelectedDiffFile = class
  private
    fOwner: TDiffForm;
    fFile: TAvailableDiffFile;        // Selected File
    fCombobox: TComboBox;             // Reference for the user selection GUI.
    fOnlySelCheckBox: TCheckBox;
    procedure GetLines(ALines: TStrings; var aFirstLine: integer; var aFileName: string);
    procedure SetIndex(NewIndex: integer);
    procedure SetFileName(aFileName: string);
    procedure UpdateIndex;
  public
    constructor Create(aOwner: TDiffForm; aCombobox: TComboBox;
      aOnlySelCheckBox: TCheckBox);
  end;

  { TDiffForm }

  TDiffForm = class(TForm)
    btnFindL: TBitBtn;
    btnFindNextR: TBitBtn;
    btnFindPreviousR: TBitBtn;
    btnFindR: TBitBtn;
    btnFindPreviousL: TBitBtn;
    btnFindNextL: TBitBtn;
    btnFirst: TButton;
    btnCompare: TButton;
    btnLast: TButton;
    btnNext: TButton;
    btnPrevious: TButton;
    CancelScanningButton: TBitBtn;
    DiffSynEdit: TSynEdit;
    edLeft: TSynEdit;
    edRight: TSynEdit;
    HelpButton: TBitBtn;
    CloseButton: TBitBtn;
    OpenInEditorButton: TBitBtn;
    OptionsGroupBox: TCheckGroup;
    PageControl1: TPageControl;
    PanelButtons: TPanel;
    ProgressBar1: TProgressBar;
    SynDiffSyn1: TSynDiffSyn;
    dlgOpen: TOpenDialog;
    SynFreePascalSyn1: TSynFreePascalSyn;
    tbsOptions: TTabSheet;
    tbsCompare: TTabSheet;
    tbsPatch: TTabSheet;
    Text1Combobox: TComboBox;
    Text1FileOpenButton: TButton;
    Text1GroupBox: TGroupBox;
    Text1OnlySelectionCheckBox: TCheckBox;
    Text2Combobox: TComboBox;
    Text2FileOpenButton: TButton;
    Text2GroupBox: TGroupBox;
    Text2OnlySelectionCheckBox: TCheckBox;

    procedure btnCompareClick(Sender: TObject);
    procedure btnFindLClick(Sender: TObject);
    procedure btnFindNextLClick(Sender: TObject);
    procedure btnFindNextRClick(Sender: TObject);
    procedure btnFindPreviousLClick(Sender: TObject);
    procedure btnFindPreviousRClick(Sender: TObject);
    procedure btnFindRClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnpreviousClick(Sender: TObject);
    procedure CancelScanningButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure edSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure edLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure edRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FileOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OnChangeFlag(Sender: TObject);
    procedure LeftSynGutterLineNumber1FormatLineNumber(
      Sender: TSynGutterLineNumber;
      ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
    procedure OpenInEditorButtonClick(Sender: TObject);
    procedure RightSynGutterLineNumber1FormatLineNumber(
      Sender: TSynGutterLineNumber;
      ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
    procedure tbsCompareResize(Sender: TObject);
    procedure Text1ComboboxChange(Sender: TObject);
    procedure Text2ComboboxChange(Sender: TObject);
  private
    fUpdating: boolean;
    fAvailableFiles: TAvailableDiffFiles;
    fSelectedFile1: TSelectedDiffFile;
    fSelectedFile2: TSelectedDiffFile;
    fDiff: TDiff;
    fLeftLines: TStringList;
    fRightLines: TStringList;
    fLeftFirstLineNumber: integer;
    fRightFirstLineNumber: integer;
    fLeftTextHashes: TCardinalList;
    fRightTextHashes: TCardinalList;
    fCurrentOptions: TTextDiffFlags2;

    fRightSearchOptions: TSynSearchOptions;
    fRightSearchText: string;
    fLeftSearchOptions: TSynSearchOptions;
    fLeftSearchText: string;

    procedure SetupComponents;
    procedure UpdateDiff;
    function GetNomalizedKindType(aLineIndex: integer): TChangeKind;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure FillTextComboBoxes;
    procedure SaveSettings;
    procedure LoadSettings;
    function GetDiffOptions: TTextDiffFlags2;
  end;

procedure ShowDiffDialog(Text1Index: integer);

const
  IgnoreCaseCheckBox = 0;
  IgnoreEmptyLineChangesCheckBox = 1;
  IgnoreHeadingSpacesCheckBox = 2;
  IgnoreLineEndsCheckBox = 3;
  IgnoreSpaceCharAmountCheckBox = 4;
  IgnoreSpaceCharsCheckBox = 5;
  IgnoreTrailingSpacesCheckBox = 6;


implementation

{$R *.lfm}

uses
  LCLIntf, dateutils, crc, strutils
  , FindReplaceDialog2
  , lazdiffUIConsts,LazConfigStorage,BaseIDEIntf,IDEMsgIntf,IDEExternToolIntf;

//********************************    support functions
// todo: move to unit   difmerge.pas ??
const
  EMPTY_LINE_HASH = 0;

{ IncrementalCRC32 }

type

  IncrementalCRC32 = record
    crc: cardinal;
    bytescount: cardinal;
    procedure Start;
    procedure NextByte(AByte: byte);
    procedure Finalize;
  end;


var
  crc32_Table: PCardinal;

procedure IncrementalCRC32.Start;
begin
  if crc32_Table = nil then
    crc32_Table := get_crc32_table;
  crc := 0 xor $FFFFFFFF;
  bytescount := 0;
end;

procedure IncrementalCRC32.NextByte(AByte: byte);
begin
  crc := crc32_table[(crc xor AByte) and $ff] xor (crc shr 8);
  Inc(bytescount);
end;

procedure IncrementalCRC32.Finalize;
begin
  if bytescount > 0 then
    crc := crc xor $FFFFFFFF
  else
    crc := EMPTY_LINE_HASH;
end;

function HashLine(const ALine: string; AOptions: TTextDiffFlags2 = []): cardinal;
var
  lLen: integer;
  lstart: integer;
  lLine: string;
  lCrc: IncrementalCRC32;
begin
  lLine := ALine;
  lLen := Length(ALine);
  lstart := 1;

  if tdfIgnoreCase in AOptions then
    lLine := AnsiLowerCase(ALine);

  //tdfIgnoreEmptyLineChanges  Not affect hash.

  if tdfIgnoreLineEnds in AOptions then
  begin
    if (llen > 0) and (lLine[llen] = #10) then
      Dec(lLen);
    if (llen > 0) and (lLine[llen] = #13) then
      Dec(lLen);
  end;

  if tdfIgnoreSpaceChars in AOptions then
  begin
    lCrc.Start;
    while (lstart <= llen) do
    begin
      if not (lLine[lstart] in [' ', #9]) then
        lCrc.NextByte(byte(lLine[lstart]));
      Inc(lstart);
    end;
    lCrc.Finalize;
    Result := lCrc.crc;
    exit;
  end;

  if tdfIgnoreHeadingSpaces in AOptions then
  begin
    while (lstart <= llen) and (lLine[lstart] in [' ', #9]) do
    begin
      Inc(lstart);
    end;
  end;
  if tdfIgnoreTrailingSpaces in AOptions then
  begin
    while (lLen > 0) and (lLine[lLen] in [' ', #9]) do
    begin
      Dec(lLen);
    end;
  end;
  if tdfIgnoreSpaceCharAmount in AOptions then
  begin
    lCrc.Start;
    while (lstart <= llen) do
    begin
      if lLine[lstart] in [' ', #9] then
      begin
        lCrc.NextByte(Ord(' ')); //first space
        Inc(lStart);
        //skip next spaces.
        while (lstart <= llen) and (lLine[lstart] in [' ', #9]) do
          Inc(lStart);
      end
      else
        lCrc.NextByte(byte(lLine[lstart]));
      Inc(lstart);
    end;
    lCrc.Finalize;
    Result := lCrc.crc;
    exit;
  end;
  if (lLen - lstart + 1) <= 0 then
    Result := CRC32(0, nil, 0)
  else
    Result := CRC32(0, pbyte(@lLine[lstart]), lLen - lstart + 1);
end;

//Line number 1 based as TSynEdit.
procedure FillHashList(AList: TCardinalList;
  ALines: TStrings; AOptions: TTextDiffFlags2 = []; AFirstLine: integer = 1;
  ALastLine: integer = maxint);
var
  i: integer;
begin
  AList.Clear;
  Dec(AFirstLine);
  Dec(ALastLine);
  if AFirstLine < 0 then
    AFirstLine := 0;
  if ALastLine > ALines.Count - 1 then
    ALastLine := ALines.Count - 1;
  for i := AFirstLine to ALastLine do
  begin
    AList.Add(HashLine(ALines[i], AOptions));
  end;
end;

function EditorGetLastColumn(AEditor: TSynEdit; ALineNumber: integer): integer; overload;
var
  lPoint: TPoint;
begin
  lPoint.X := length(AEditor.Lines[ALineNumber - 1]) + 1;
  lPoint.Y := ALineNumber;
  lPoint := AEditor.PhysicalToLogicalPos(lPoint);
  Result := lPoint.X;
end;

procedure EditorDeleteLine(AEditor: TSynEdit; ALineNumber: integer); overload;
var
  lStartPoint, lEndPoint: Tpoint;
begin
  if (ALineNumber < 0) or (ALineNumber > AEditor.Lines.Count) then
    Exit;
  lStartPoint.X := 1;
  lStartPoint.Y := ALineNumber;
  lEndPoint.X := 1;
  lEndPoint.Y := ALineNumber + 1;
  if lEndPoint.Y > AEditor.Lines.Count then
  begin
    lEndPoint.Y := ALineNumber;
    lEndPoint.X := EditorGetLastColumn(AEditor, ALineNumber);
    if ALineNumber > 1 then
    begin
      lStartPoint.X := EditorGetLastColumn(AEditor, ALineNumber - 1);
      lStartPoint.Y := ALineNumber - 1;
    end;
  end;
  AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, '');
end;

procedure EditorInsertLine(AEditor: TSynEdit; ALineNumber: integer;
  AText: string); overload;
var
  lStartPoint, lEndPoint: Tpoint;
  lColumn: integer;
begin
  if (ALineNumber < 0) or (ALineNumber > AEditor.Lines.Count + 1) then
    Exit;
  if ALineNumber = (AEditor.Lines.Count + 1) then   //append at the end.
  begin
    Dec(ALineNumber);
    lColumn := EditorGetLastColumn(AEditor, ALineNumber);
    lStartPoint.X := lColumn;
    lStartPoint.Y := ALineNumber;
    lEndPoint.X := lColumn;
    lEndPoint.Y := ALineNumber;
    AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, #10 + AText);
  end
  else
  begin                                         //insert
    lStartPoint.X := 1;
    lStartPoint.Y := ALineNumber;
    lEndPoint.X := 1;
    lEndPoint.Y := ALineNumber;
    AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, AText + #10);
  end;
end;

procedure EditorReplaceLine(AEditor: TSynEdit; ALineNumber: integer;
  ANewText: string); overload;
var
  lStartPoint, lEndPoint: Tpoint;
begin
  if (ALineNumber < 0) or (ALineNumber > AEditor.Lines.Count) then
    Exit;
  lStartPoint.X := 1;
  lStartPoint.Y := ALineNumber;
  lEndPoint.X := 1;
  lEndPoint.Y := ALineNumber;
  lEndPoint.X := EditorGetLastColumn(AEditor, ALineNumber);
  AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, ANewText);
end;

procedure Compare(ADiff: TDiff; AText0: TStrings; AText1: TStrings;
  AList0TextHashes, AList1TextHashes: TCardinalList; AEditor0: TSynEdit; AEditor1: TSynEdit);
var
  lI: integer;
begin
  try
    ADiff.Execute(AList0TextHashes, AList1TextHashes);
    AEditor0.BeginUpdate;
    AEditor1.BeginUpdate;
    AEditor0.Lines.Clear;
    AEditor1.Lines.Clear;
    for lI := 0 to ADiff.Count - 1 do
    begin
      with ADiff.Compares[lI] do
      begin
        case Kind of
          ckNone:
          begin
            AEditor0.Lines.Add(aText0[oldIndex1]);
            AEditor1.Lines.Add(aText1[oldIndex2]);
          end;
          ckAdd:
          begin
            AEditor0.Lines.Add('');
            AEditor1.Lines.Add(aText1[oldIndex2]);
          end;
          ckDelete:
          begin
            AEditor0.Lines.Add(aText0[oldIndex1]);
            AEditor1.Lines.Add('');
          end;
          ckModify:
          begin
            AEditor0.Lines.Add(aText0[oldIndex1]);
            AEditor1.Lines.Add(aText1[oldIndex2]);
          end;
        end;
      end;
    end;
  finally
    AEditor0.EndUpdate;
    AEditor1.EndUpdate;
  end;
end;

// modified from dateutil.inc
const
  FmtUTC = 'yyyy"-"mm"-"dd" "hh":"nn":"ss"."zzz000000 ';

function DateToUnifiedPatchFormat(const ADate: TDateTime;
  AInputIsUTC: boolean = True): string;
const
  FmtOffset: string = '%.02d%.02d';
  Sign: array[boolean] of char = ('+', '-');
var
  Offset: integer;
begin
  Result := FormatDateTime(FmtUTC, ADate);
  Offset := GetLocalTimeOffset;//(ADate, AInputIsUTC);
  if AInputIsUTC or (Offset = 0) then
    Result := Result + 'Z'
  else
  begin
    Result := Result + Sign[Offset > 0];
    Offset := Abs(Offset);
    Result := Result + Format(FmtOffset, [Offset div MinsPerHour,
      Offset mod MinsPerHour]);
  end;
end;

procedure MakeUnifiedPatch(ADiff: TDiff; ALeftLines: TStrings;
  ARightLines: TStrings; APatchLines: TStrings; const ALeftFileName: string;
  const ARightFilename: string; ALeftFileDate: TDateTime = 0;
  ARigthFileDate: TDateTime = 0; ALeftFirstLineNumber: integer = 1;
  ARightFirstLineNumber: integer = 1);
const
  CONTEXT_LINES = 2;
  NEW_HUNK_LINES = 8;
var
  LineIndex, wJ: integer;
  DiffFound: boolean;
  StartLine: integer;
  CountContext: integer;
  HunkHeaderIndex: integer;
  PushLineIndex: integer;
  PushedLineIndexModify: integer;
  LeftLineStart, RightLineStart, LeftLinesCount, RightLinesCount: integer;
  NewHunk: boolean;
  LeftDateStr, RightDateStr: string;
  IgnoreEmptyLine: boolean;

  procedure SetInitialLines(ALeftLine: integer; ARightLine: integer);
  begin
    if (ALeftLine >= 0) and (LeftLineStart < 0) then
      LeftLineStart := ALeftLine + 1;
    if (ARightLine >= 0) and (RightLineStart < 0) then
      RightLineStart := ARightLine + 1;
  end;

  procedure TestNoNewLineAtEndOfFile(AStrings: TStrings; AIndex: integer);
  begin
    if AIndex = AStrings.Count - 1 then
    begin
      if AStrings[AIndex] <> '' then
        APatchLines.Add('\ no new line at end of file');    //don't translate
    end;
  end;

  procedure AddPatchLine(const APrefix: string; const Astrings: TStrings;
    AIndex: integer);
  begin
    APatchLines.Add(APrefix + AStrings[AIndex]);
    TestNoNewLineAtEndOfFile(AStrings, AIndex);
  end;

begin
  Dec(ALeftFirstLineNumber); // 0 offset
  Dec(ARightFirstLineNumber);
  APatchLines.Clear;
  if (ADiff.DiffStats.adds = 0) and (ADiff.DiffStats.deletes = 0) and
    (ADiff.DiffStats.modifies = 0) then
    Exit;
  LeftDateStr := '';
  RightDateStr := '';
  if ALeftFileDate <> 0 then
    LeftDateStr := DateToUnifiedPatchFormat(ALeftFileDate, False);
  if ARigthFileDate <> 0 then
    RightDateStr := DateToUnifiedPatchFormat(ARigthFileDate, False);
  APatchLines.Add('--- ' + ALeftFileName + ' ' + LeftDateStr);
  APatchLines.Add('+++ ' + ARightFilename + ' ' + RightDateStr);
  // find blocks with differences
  LineIndex := 0;
  StartLine := 0;
  while (LineIndex < ADiff.Count) do
  begin
    DiffFound := False;
    while (LineIndex < ADiff.Count) and (DiffFound = False) do
    begin
      if ADiff.Compares[LineIndex].Kind <> ckNone then
      begin
        DiffFound := True;
        break;
      end;
      Inc(LineIndex);
    end;
    if DiffFound = False then
      break;
    // move back context lines
    wJ := LineIndex - 1;
    CountContext := 0;
    while (wJ >= StartLine) and (ADiff.Compares[wJ].Kind = ckNone) and
      (CountContext < CONTEXT_LINES) do
    begin
      Inc(CountContext);
      Dec(wJ);
    end;
    LineIndex := wJ + 1;
    HunkHeaderIndex := APatchLines.Count;
    APatchLines.Add('@@ -ll,rl +lr,rr @@'); // < updated later at end of hunk.
    LeftLineStart := -1;
    LeftLinesCount := 0;
    RightLineStart := -1;
    RightLinesCount := 0;
    // Add context lines
    for wJ := 1 to CountContext do
    begin
      AddPatchLine(' ', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
      SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
        ADiff.Compares[LineIndex].oldIndex2);
      Inc(LineIndex);
      Inc(LeftLinesCount);
      Inc(RightLinesCount);
    end;
    while (LineIndex < ADiff.Count) do
    begin
      //Add changed lines
      while (LineIndex < ADiff.Count) and (ADiff.Compares[LineIndex].Kind <> ckNone) do
      begin
        case ADiff.Compares[LineIndex].Kind of
          //ckNone: ;
          ckAdd:
          begin
            AddPatchLine('+', ARightLines, ADiff.Compares[LineIndex].oldIndex2);
            SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
              ADiff.Compares[LineIndex].oldIndex2);
            Inc(RightLinesCount);
          end;
          ckDelete:
          begin
            AddPatchLine('-', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
            SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
              ADiff.Compares[LineIndex].oldIndex2);
            Inc(LeftLinesCount);
          end;
          ckModify:
          begin
          {$DEFINE FIRSTDELETEALLMODIFIEDLINES}
          {$IFDEF FIRSTDELETEALLMODIFIEDLINES}
            PushedLineIndexModify := LineIndex;
            while (LineIndex < ADiff.Count) and
              (ADiff.Compares[LineIndex].Kind = ckModify) do
            begin
              AddPatchLine('-', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
              SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
                ADiff.Compares[LineIndex].oldIndex2);
              Inc(LineIndex);
            end;
            LineIndex := PushedLineIndexModify;
            while (LineIndex < ADiff.Count) and
              (ADiff.Compares[LineIndex].Kind = ckModify) do
            begin
              AddPatchLine('+', ARightLines, ADiff.Compares[LineIndex].oldIndex2);
              SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
                ADiff.Compares[LineIndex].oldIndex2);
              Inc(LeftLinesCount);
              Inc(RightLinesCount);
              Inc(LineIndex);
            end;
            if LineIndex > PushedLineIndexModify then
              Dec(LineIndex);
          {$ELSE}
            AddPatchLine('-', LeftLines, Diff.Compares[LineIndex].oldIndex1);
            AddPatchLine('+', RightLines, Diff.Compares[LineIndex].oldIndex2);
            SetInitialLines(Diff.Compares[LineIndex].oldIndex1, Diff.Compares[LineIndex].oldIndex2);
            Inc(LeftLinesCount);
            Inc(RightLinesCount);
          {$ENDIF}
          end;
        end;
        Inc(LineIndex);
      end;
      // Add context lines
      // check if there are less than NEW_HUNK_LINES lines without modifying and add them
      // if there are more, just add the first CONTEXT_LINES and jump to next HUNK.
      PushLineIndex := LineIndex;
      CountContext := 0;
      NewHunk := False;
      while (LineIndex < ADiff.Count) do
      begin
        if ADiff.Compares[LineIndex].Kind = ckNone then
        begin
          Inc(CountContext);
          if CountContext >= NEW_HUNK_LINES then
          begin
            CountContext := CONTEXT_LINES;
            NewHunk := True;
            break;
          end;
        end
        else
        begin
          break;
        end;
        Inc(LineIndex);
      end;
      if (LineIndex >= ADiff.Count) and (ADiff.Compares[ADiff.Count - 1].Kind =
        ckNone) then
      begin
        CountContext := CONTEXT_LINES;
        NewHunk := True;
      end;
      LineIndex := PushLineIndex;
      for PushLineIndex := 1 to CountContext do
      begin
        if LineIndex >= ADiff.Count then
          break;
        AddPatchLine(' ', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
        SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
          ADiff.Compares[LineIndex].oldIndex2);
        Inc(LineIndex);
        Inc(LeftLinesCount);
        Inc(RightLinesCount);
      end;
      if NewHunk then
        break;
    end;
    // Update hunk data.
    if LeftLineStart < 0 then LeftLineStart := 0;
    if RightLineStart < 0 then RightLineStart := 0;
    if LeftLinesCount < 0 then LeftLinesCount := 0;
    if RightLinesCount < 0 then RightLinesCount := 0;
    APatchLines[HunkHeaderIndex] :=
      '@@ -' + IntToStr(LeftLineStart + ALeftFirstLineNumber) + ',' +
      IntToStr(LeftLinesCount) + ' +' + IntToStr(RightLineStart +
      ARightFirstLineNumber) + ',' + IntToStr(RightLinesCount) + ' @@';
    StartLine := LineIndex;
  end;
end;

// Tries to apply the first patch to the editor.
// Only tries the first.
function ApplyUnifiedPatch(AEditor: TSynEdit; APatch: TStrings;
  AOnlyTest: boolean; out AErrorMsg: string): boolean;
const
  lisDiffMsgOk = 'Ok';
  lisDiffMsgInvalidFileFormat = 'Invalid patch file format';
  lisDiffMsgLineNotExists = 'Error, line: %d not exists in editor';
  lisDiffMsgLinesDontMatch = 'Error, editor line: %d don''t match: [%s] editor [%s]';
var
  PatchLine: string;
  Index: integer;
  FirstChar: char;
  LineNumber: integer;
  EditorLineCount: integer;
  DeltaLines: integer;

  function GetLineNumber: integer;
  var
    I: integer;
  begin
    Result := -9999;
    I := 4;
    if (I <= length(PatchLine)) and (PatchLine[I] in ['0'..'9']) then
    begin
      Result := Ord(PatchLine[I]) - Ord('0');
      Inc(I);
    end;
    while (I <= length(PatchLine)) and (PatchLine[I] in ['0'..'9']) do
    begin
      Result := 10 * Result + (Ord(PatchLine[I]) - Ord('0'));
      Inc(I);
    end;
  end;

begin
  // first test if we can apply the patch.
  if AOnlyTest = False then
  begin
    if not ApplyUnifiedPatch(AEditor, APatch, True, AErrorMsg) then
    begin
      exit(False);
    end;
  end;
  try
    if not AOnlyTest then
    begin
      AEditor.BeginUpdate;
      AEditor.BeginUndoBlock;
    end;
    AErrorMsg := lisDiffMsgInvalidFileFormat;
    if aPatch.Count < 4 then
      exit(False);
    Index := 0;
    PatchLine := APatch[Index];
    // Skip git header if present.
    if AnsiStartsStr('From ', PatchLine) then
    begin
      while Index < aPatch.Count do
      begin
        Inc(Index);
        PatchLine := APatch[Index];
        if AnsiStartsStr('index', PatchLine) then
        begin
          Inc(Index);
          PatchLine := APatch[Index];
          break;
        end;
      end;
    end;
    // Skip subversion header if present.
    if AnsiStartsStr('Index', PatchLine) then
    begin
      while Index < aPatch.Count do
      begin
        Inc(Index);
        PatchLine := APatch[Index];
        if AnsiStartsStr('=====', PatchLine) then
        begin
          Inc(Index);
          PatchLine := APatch[Index];
          break;
        end;
      end;
    end;
    if aPatch.Count < (Index + 4) then
      exit(False);
    if not AnsiStartsStr('---', PatchLine) then
      exit(False);
    Inc(Index);
    PatchLine := APatch[Index];
    if not AnsiStartsStr('+++', PatchLine) then
      exit(False);
    Inc(Index);
    PatchLine := APatch[Index];
    if Copy(PatchLine, 1, 4) <> '@@ -' then
      exit(False);
    EditorLineCount := AEditor.Lines.Count;
    DeltaLines := 0;
    while Index < APatch.Count do
    begin
      PatchLine := APatch[Index];
      if length(PatchLine) < 1 then   // empty line.
      begin
        FirstChar := ' ';
        PatchLine := '';
      end
      else
      begin
        FirstChar := PatchLine[1];
        PatchLine := Copy(PatchLine, 2, Length(PatchLine) - 1);
      end;
      case FirstChar of
        '@':
        begin
          LineNumber := GetLineNumber;
          if LineNumber = -9999 then
            exit(False);
        end;
        ' ':
        begin  //compare if lines are equal.
          if LineNumber > EditorLineCount then
          begin
            AErrorMsg := Format(lisDiffMsgLineNotExists, [LineNumber]);
            exit(False);
          end;
          if PatchLine <> AEditor.Lines[LineNumber + DeltaLines - 1] then
          begin
            AErrorMsg := Format(lisDiffMsgLinesDontMatch,
              [LineNumber, PatchLine, AEditor.Lines[LineNumber + DeltaLines - 1]]);
            exit(False);
          end;
          Inc(LineNumber);
        end;
        '+':
        begin
          if not AOnlyTest then
          begin
            EditorInsertLine(aEditor, LineNumber + DeltaLines, PatchLine);
            Inc(DeltaLines);
          end;
        end;
        '-':
        begin
          if not AOnlyTest then
          begin
            EditorDeleteLine(aEditor, LineNumber + DeltaLines);
            Dec(DeltaLines);
          end;
          Inc(LineNumber);
        end;
        '\':
        begin
        end;
        'd':  // 'diff' next patch in the file git
        begin
          break;
        end;
        'I', '=':  // 'Index' next patch in the file subversion
        begin
          break;
        end;
        else
          exit(False);
      end;
      Inc(Index);
    end;
  finally
    if not AOnlyTest then
    begin
      AEditor.EndUndoBlock;
      AEditor.EndUpdate;
    end;
  end;
  AErrorMsg := lisDiffMsgOk;
  exit(True);
end;

// faster than aTo.Text:=aFrom.Text
procedure CopyStringList(AFrom: TStrings; ATo: TStrings); overload;
var
  lI: integer;
begin
  ATo.Clear;
  ATo.Capacity := AFrom.Count;
  for lI := 0 to AFrom.Count - 1 do
  begin
    ATo.Add(AFrom[lI]);
  end;
end;

procedure CopyStringList(AFrom: TStrings; ATo: TStrings; AFromLine: integer;
  AToLine: integer); overload;
var
  lI: integer;
begin
  ATo.Clear;
  ATo.Capacity := AToLine - AFromLine + 1;
  for lI := AFromLine to AToLine do
    ATo.Add(AFrom[lI]);
end;

//***************************************  end of support functions

procedure ShowDiffDialog(Text1Index: integer);
var
  DiffDlg: TDiffForm;
begin
  DiffDlg := TDiffForm.Create(nil);
  try
    DiffDlg.fSelectedFile1.SetIndex(Text1Index);
    DiffDlg.Init;
    //Result :=  DiffDlg.ShowModal;
    //    DiffDlg.Show;
    IDEWindowCreators.ShowForm(DiffDlg, False);
  finally
  end;
end;

{ TSelectedDiffFile }

constructor TSelectedDiffFile.Create(aOwner: TDiffForm; aCombobox: TComboBox;
  aOnlySelCheckBox: TCheckBox);
begin
  inherited Create;
  fOwner := aOwner;
  fCombobox := aCombobox;
  fOnlySelCheckBox := aOnlySelCheckBox;
end;

procedure TSelectedDiffFile.GetLines(ALines: TStrings; var aFirstLine: integer;
  var aFileName: string);
begin
  aFirstLine := 1;
  aFileName := '';
  if fFile = nil then Exit;
  if fFile.Editor = nil then
  begin
    ALines.LoadFromFile(fFile.Name);
    AFileName := fFile.Name;
  end
  else
  begin
    AFileName := fFile.Editor.FileName;
    if (fFile.SelectionAvailable and fOnlySelCheckBox.Checked) then
    begin
      aFirstLine := fFile.Editor.BlockBegin.Y;
      //ALines.Text := fFile.Editor.EditorComponent.SelText;
      CopyStringList(fFile.Editor.Lines, ALines, aFirstLine - 1,
        fFile.Editor.BlockEnd.Y - 1);
    end
    else
      //ALines.Text := fFile.Editor.EditorComponent.Lines.Text;
      CopyStringList(fFile.Editor.Lines, ALines);
  end;
end;

procedure TSelectedDiffFile.SetIndex(NewIndex: integer);
var
  OldFile: TAvailableDiffFile;
begin
  OldFile := fFile;
  if (NewIndex >= 0) and (NewIndex < fOwner.fAvailableFiles.Count) then
  begin
    fFile := fOwner.fAvailableFiles[NewIndex];
    fCombobox.Text := fFile.Name;
    fOnlySelCheckBox.Enabled := fFile.SelectionAvailable;
  end
  else
  begin
    fFile := nil;
    fCombobox.Text := '';
    fOnlySelCheckBox.Enabled := False;
  end;
  if fFile <> OldFile then fOwner.UpdateDiff;
end;

procedure TSelectedDiffFile.SetFileName(aFileName: string);
// Assumes that aFileName is already in fCombobox.Items.
begin
  fCombobox.ItemIndex := fCombobox.Items.IndexOf(aFileName);
  SetIndex(fCombobox.Items.IndexOf(aFileName));
end;

procedure TSelectedDiffFile.UpdateIndex;
begin
  SetIndex(fCombobox.Items.IndexOf(fCombobox.Text));
end;

{ TDiffForm }

constructor TDiffForm.Create(TheOwner: TComponent);
var
  i:integer;
  SrcEdit: TSourceEditorInterface;
begin
  inherited Create(TheOwner);
  fUpdating := False;
  fSelectedFile1 := TSelectedDiffFile.Create(Self, Text1Combobox,
    Text1OnlySelectionCheckBox);
  fSelectedFile2 := TSelectedDiffFile.Create(Self, Text2Combobox,
    Text2OnlySelectionCheckBox);
  Caption := lisCaptionCompareFiles2;
  IDEDialogLayoutList.ApplyLayout(Self, 600, 500);
  SetupComponents;
  fDiff := TDiff.Create(nil);
  fLeftLines := TStringList.Create;
  fRightLines := TStringList.Create;
  fLeftTextHashes := TCardinalList.Create;
  fRightTextHashes := TCardinalList.Create;
  fAvailableFiles := TAvailableDiffFiles.Create;
   // Get available files
   for i := 0 to SourceEditorManagerIntf.SourceEditorCount - 1 do
   begin
     SrcEdit := SourceEditorManagerIntf.SourceEditors[i];
     // FindSourceEditorWithPageIndex(i);
     fAvailableFiles.Add(TAvailableDiffFile.Create(SrcEdit.PageName, SrcEdit,
       SrcEdit.SelectionAvailable));
   end;

end;

destructor TDiffForm.Destroy;
begin
  SaveSettings;
  fSelectedFile2.Free;
  fSelectedFile1.Free;
  fDiff.Free;
  fLeftLines.Free;
  fRightLines.Free;
  fLeftTextHashes.Free;
  fRightTextHashes.Free;
  fAvailableFiles.Free;
  inherited Destroy;
end;

procedure TDiffForm.OnChangeFlag(Sender: TObject);
begin
  UpdateDiff;
end;

function MyShowDot(Sender: TSynGutterLineNumber; AEditor: TSynEdit; ALine: integer): boolean;
begin
  Result := ((ALine mod Sender.ShowOnlyLineNumbersMultiplesOf) <> 0) and
    (ALine <> AEditor.CaretY) and (ALine <> 1) and (ALine <> AEditor.Lines.Count);
end;

procedure TDiffForm.LeftSynGutterLineNumber1FormatLineNumber(
  Sender: TSynGutterLineNumber; ALine: integer; out AText: string;
  const ALineInfo: TSynEditGutterLineInfo);
begin
  if (FDiff.Count > 0) and (aLine > 0) and (aLine <= FDiff.Count) then
  begin
    if FDiff.Compares[aLine - 1].Kind = ckAdd then
    begin
      aText := '+';
      Exit;
    end
    else
      aLine := FDiff.Compares[aLine - 1].oldIndex1 + fLeftFirstLineNumber;
    aText := Sender.FormatLineNumber(aLine, MyShowDot(Sender, edLeft, ALine)); //AlineInfo.ShowDot);
  end;
end;

{
function TMainIDE.DoDiff: TModalResult;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  DiffText: string;
  NewDiffFilename: String;
begin
  Result:=mrCancel;
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if ActiveSrcEdit=nil then exit;

  Result:=ShowDiffDialog(ActiveSrcEdit.PageIndex, DiffText);
  if Result = mrYes then begin
    NewDiffFilename:=CreateSrcEditPageName('','FileDifference.diff', nil);
    Result:=DoNewEditorFile(FileDescriptorText,NewDiffFilename,DiffText,
                            [nfOpenInEditor,nfIsNotPartOfProject]);
    GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
    if ActiveSrcEdit=nil then exit;
  end;
end;
}


procedure TDiffForm.OpenInEditorButtonClick(Sender: TObject);

begin
  //TODO: create new editor.

end;

procedure TDiffForm.RightSynGutterLineNumber1FormatLineNumber(
  Sender: TSynGutterLineNumber; ALine: integer; out AText: string;
  const ALineInfo: TSynEditGutterLineInfo);
begin
  if (FDiff.Count > 0) and (aLine > 0) and (aLine <= FDiff.Count) then
  begin
    if FDiff.Compares[aLine - 1].Kind = ckDelete then
    begin
      aText := '-';
      Exit;
    end
    else
      aLine := FDiff.Compares[aLine - 1].oldIndex2 + fRightFirstLineNumber;
    aText := Sender.FormatLineNumber(aLine, MyShowDot(Sender, edRight, ALine)); //ALineInfo.ShowDot);
  end;
end;

procedure TDiffForm.tbsCompareResize(Sender: TObject);
const
  MARGIN = 5;
  SEPARATION = 10;
var
  lWidth: integer;
begin
  lWidth := (TTabSheet(Sender).Width - MARGIN - MARGIN - SEPARATION) div 2;
  edLeft.Width := lWidth;
  edRight.Left := edLeft.Left + lWidth + SEPARATION;
  edRight.Width := lWidth;
  //center buttons.
  PanelButtons.Left := (TTabSheet(Sender).Width div 2) - (PanelButtons.Width div 2);
end;

procedure TDiffForm.FileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    //only add new files
    if Text1ComboBox.Items.IndexOf(dlgOpen.FileName) = -1 then
    begin
      fAvailableFiles.Add(TAvailableDiffFile.Create(dlgOpen.FileName, nil, False));
      Text1ComboBox.Items.Add(dlgOpen.FileName);
      Text2ComboBox.Items.Add(dlgOpen.FileName);
    end;
    //set the combobox and make the diff
    if TButton(Sender) = Text1FileOpenButton then
      fSelectedFile1.SetFileName(dlgOpen.FileName)
    else
      fSelectedFile2.SetFileName(dlgOpen.FileName);
  end;
end;

procedure TDiffForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDiffForm.CancelScanningButtonClick(Sender: TObject);
begin
  fDiff.Cancel;
end;

procedure TDiffForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TDiffForm.btnCompareClick(Sender: TObject);
var
  Text1Src, Text2Src: string;
  FileNameLeft, FileNameRight: string;
  Digits: integer;
const
  TAB_COMPARE_INDEX = 1;
begin
  if fUpdating then Exit;
  fUpdating := True;
  DiffSynEdit.Lines.Clear;
  fSelectedFile1.GetLines(fLeftLines, fLeftFirstLineNumber, FileNameLeft);
  fSelectedFile2.GetLines(fRightLines, fRightFirstLineNumber, FileNameRight);
  fCurrentOptions := GetDiffOptions;
  if (fLeftLines.Count > 0) and (fRightLines.Count > 0) then
  begin
    Text1GroupBox.Enabled := False;
    Text2GroupBox.Enabled := False;
    OpenInEditorButton.Enabled := False;
    CancelScanningButton.Enabled := True;
    CancelScanningButton.Visible := True;
    ProgressBar1.Style := pbstMarquee;
    FillHashList(fLeftTextHashes, fLeftLines, fCurrentOptions);
    FillHashList(fRightTextHashes, fRightLines, fCurrentOptions);
    Compare(fDiff, fLeftLines, fRightLines, fLeftTextHashes, fRightTextHashes,
      edLeft, edRight);
    if not fDiff.Cancelled then
    begin
      MakeUnifiedPatch(fDiff, fLeftLines, fRightLines, DiffSynEdit.Lines,
        ExtractFileName(FileNameLeft), ExtractFileName(FileNameRight), 0,
        0, fLeftFirstLineNumber, fRightFirstLineNumber);
      //adjust gutter sizes.
      Digits := fDiff.Count + fLeftFirstLineNumber;
      TSynGutterLineNumber(edLeft.Gutter.Parts[1]).DigitCount :=
        length(IntToStr(Digits));
      Digits := fDiff.Count + fRightFirstLineNumber;
      TSynGutterLineNumber(edRight.Gutter.Parts[1]).DigitCount :=
        length(IntToStr(Digits));
      ProgressBar1.Style := pbstNormal;
      PageControl1.TabIndex := TAB_COMPARE_INDEX;  //compare.
    end;
    CancelScanningButton.Visible := False;
    CancelScanningButton.Enabled := False;
    OpenInEditorButton.Enabled := True;
    Text2GroupBox.Enabled := True;
    Text1GroupBox.Enabled := True;
  end;
  fUpdating := False;
end;


//  *****************    search        ***********************
procedure DoFind(aEditor: TSynEdit;
  const aSearchText: string; aOptions: TSynSearchOptions; aForward: boolean);
var
  StartPoint: TPoint;
begin
  if length(aSearchText) <> 0 then
  begin
    //Include(aOptions, ssoFindContinue);
    if aForward then
      Exclude(aOptions, ssoBackwards)
    else
      Include(aOptions, ssoBackwards);
    if not (ssoFindContinue in aOptions) then
      StartPoint := Point(1, 1)
    else
    begin
      StartPoint := aEditor.LogicalCaretXY;
      if ssoBackwards in aOptions then
      begin
        if aEditor.SelText = aSearchText then
          Dec(StartPoint.X, length(aSearchText));
      end
      else
      begin
        if aEditor.SelText = aSearchText then
          Inc(StartPoint.X, length(aSearchText));
      end;
    end;
    if aEditor.SearchReplaceEx(aSearchText, '', aOptions, StartPoint) = 0 then
      ShowMessage(Format(lisUESearchStringNotFound2, [aSearchText]));
  end;
end;

procedure DoFindDialog(aEditor: TSynEdit;
  var aSearchText: string; var aSearchOptions: TSynSearchOptions);
var
  LazFindReplaceDialog2: TLazFindReplaceDialog2;
begin
  LazFindReplaceDialog2 := TLazFindReplaceDialog2.Create(nil);
  try
    LazFindReplaceDialog2.FindText := aSearchText;
    LazFindReplaceDialog2.Options := aSearchOptions;

    LazFindReplaceDialog2.ReplaceWithCheckbox.Checked := False;
    LazFindReplaceDialog2.ReplaceWithCheckboxChange(nil);
    LazFindReplaceDialog2.ReplaceWithCheckbox.Enabled := False;

    if LazFindReplaceDialog2.ShowModal = mrOk then
    begin
      aSearchText := LazFindReplaceDialog2.FindText;
      aSearchOptions := LazFindReplaceDialog2.Options;
      DoFind(aEditor, aSearchText, aSearchOptions, not (ssoBackwards in aSearchOptions));
    end;
    LazFindReplaceDialog2.ReplaceWithCheckbox.Enabled := True;
  finally
    LazFindReplaceDialog2.Free;

  end;
end;


procedure TDiffForm.btnFindRClick(Sender: TObject);
begin
  DoFindDialog(edRight, fRightSearchText, fRightSearchOptions);
end;

procedure TDiffForm.btnFindNextRClick(Sender: TObject);
begin
  DoFind(edRight, fRightSearchText, fRightSearchOptions + [ssoFindContinue], True);
end;

procedure TDiffForm.btnFindPreviousRClick(Sender: TObject);
begin
  DoFind(edRight, fRightSearchText, fRightSearchOptions + [ssoFindContinue], False);
end;

procedure TDiffForm.btnFindLClick(Sender: TObject);
begin
  DoFindDialog(edLeft, fLeftSearchText, fLeftSearchOptions);
end;

procedure TDiffForm.btnFindNextLClick(Sender: TObject);
begin
  DoFind(edLeft, fLeftSearchText, fLeftSearchOptions + [ssoFindContinue], True);
end;

procedure TDiffForm.btnFindPreviousLClick(Sender: TObject);
begin
  DoFind(edLeft, fLeftSearchText, fLeftSearchOptions + [ssoFindContinue], False);
end;
//  **********************************************************

procedure TDiffForm.btnFirstClick(Sender: TObject);
begin
  edLeft.CaretY := 1;
  if fDiff.Count > 0 then
  begin
    if fDiff.Compares[0].Kind = ckNone then
      btnNextClick(Self);
  end;
end;

procedure TDiffForm.btnLastClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := edLeft.Lines.Count - 1;
  edLeft.CaretY := Idx + 1;
  if fDiff.Count >= Idx then
  begin
    if fDiff.Compares[Idx].Kind = ckNone then
      btnpreviousClick(Self);
  end;
end;

// normalize kind type if we ignore blank line aditions or deletions
function TDiffForm.GetNomalizedKindType(aLineIndex: integer): TChangeKind;
begin
  Result := fDiff.Compares[aLineIndex - 1].Kind;
  if tdfIgnoreEmptyLineChanges in fCurrentOptions then
  begin
    if Result = ckAdd then
    begin
      if fDiff.Compares[aLineIndex - 1].int2 = EMPTY_LINE_HASH then
        Result := ckNone;
    end
    else if Result = ckDelete then
    begin
      if fDiff.Compares[aLineIndex - 1].int1 = EMPTY_LINE_HASH then
        Result := ckNone;
    end;
  end;
end;

procedure TDiffForm.btnNextClick(Sender: TObject);
var
  LineIndex: integer;
  Kind, KindTest: TChangeKind;
begin
  LineIndex := edLeft.CaretY;
  if (LineIndex > fDiff.Count) or (LineIndex < 1) then
    Exit;
  Kind := GetNomalizedKindType(LineIndex);
  Inc(LineIndex);
  if LineIndex > fDiff.Count then
    Exit;
  while LineIndex < fDiff.Count do
  begin
    KindTest := GetNomalizedKindType(LineIndex);
    if KindTest <> Kind then
    begin
      if KindTest <> ckNone then
      begin
        edLeft.CaretY := LineIndex;
        break;
      end;
      Kind := ckNone;
    end;
    Inc(LineIndex);
  end;
end;

procedure TDiffForm.btnpreviousClick(Sender: TObject);
var
  LineIndex: integer;
  Kind, KindTest: TChangeKind;
begin
  LineIndex := edLeft.CaretY;
  if (LineIndex > fDiff.Count) or (LineIndex < 1) then
    Exit;
  Kind := GetNomalizedKindType(LineIndex);
  Dec(LineIndex);
  if LineIndex < 1 then
    Exit;
  while LineIndex >= 1 do
  begin
    KindTest := GetNomalizedKindType(LineIndex);
    if KindTest <> Kind then
    begin
      if KindTest <> ckNone then
      begin
        edLeft.CaretY := LineIndex;
        break;
      end;
      Kind := ckNone;
    end;
    Dec(LineIndex);
  end;
end;

procedure TDiffForm.edSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
var
  r, g, b: byte;
const
  COLOR_ADDED_LINE = $ffdddd;
  COLOR_ADDED_EMPTY_LINE = $fff5f5;
  COLOR_DELETED_LINE = $ddddff;
  COLOR_DELETED_EMPTY_LINE = $f5f5ff;
  COLOR_MODIFIED_LINE = $ddffdd;
  DARKNESS = $a;
begin

  if (fDiff.Count > 0) and (Line > 0) and (Line <= fDiff.Count) then
  begin
    case fDiff.Compares[Line - 1].Kind of
      //ckNone:
      //  BG := clWhite;
      ckAdd:
      begin
        BG := COLOR_ADDED_LINE;
        if (tdfIgnoreEmptyLineChanges in fCurrentOptions) and
          (fDiff.Compares[Line - 1].int2 = EMPTY_LINE_HASH) then
        begin
          BG := COLOR_ADDED_EMPTY_LINE;
        end;
      end;
      ckDelete:
      begin
        BG := COLOR_DELETED_LINE;
        // if (tdfIgnoreEmptyLineChanges in fCurrentOptions) and (edLeft.Lines[fDiff.Compares[Line-1].oldIndex1]='') then
        if (tdfIgnoreEmptyLineChanges in fCurrentOptions) and
          (fDiff.Compares[Line - 1].int1 = EMPTY_LINE_HASH) then
        begin
          BG := COLOR_DELETED_EMPTY_LINE;
        end;
      end;
      ckModify:
        BG := COLOR_MODIFIED_LINE;
    end;
  end;
  if Line = TSynEdit(Sender).CaretY then  //current line.
  begin
    r := Red(BG) - DARKNESS;
    g := Green(BG) - DARKNESS;
    b := Blue(BG) - DARKNESS;
    BG := RGB(r, g, b);
  end;
  Special := True;
end;

procedure Syncronize(Changes: TSynStatusChanges; aMaster: TSynEdit; aSlave: TSynEdit);
begin
  if scTopLine in Changes then
    aSlave.TopLine := aMaster.TopLine;
  if scLeftChar in Changes then
    aSlave.LeftChar := aMaster.LeftChar;
  if scCaretY in Changes then
  begin
    if aSlave.CaretY <> aMaster.CaretY then    // if then to allow text selection.
      aSlave.CaretY := aMaster.CaretY;
  end;
end;

procedure TDiffForm.edLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  Syncronize(Changes, edLeft, edRight);
end;

procedure TDiffForm.edRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  Syncronize(Changes, edRight, edLeft);
end;

procedure TDiffForm.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TDiffForm.Text1ComboboxChange(Sender: TObject);
begin
  fSelectedFile1.UpdateIndex;
end;

procedure TDiffForm.Text2ComboboxChange(Sender: TObject);
begin
  fSelectedFile2.UpdateIndex;
end;

procedure TDiffForm.SetupComponents;
const
  GetAllFilesMask2='*.*';
begin
  // text 1
  Text1GroupBox.Caption := lisDiffDlgFile12;
  Text1OnlySelectionCheckBox.Caption := lisDiffDlgOnlySelection2;
  Text1FileOpenButton.Caption := '...';

  // text 2
  Text2GroupBox.Caption := lisDiffDlgFile22;
  Text2OnlySelectionCheckBox.Caption := lisDiffDlgOnlySelection2;
  Text2FileOpenButton.Caption := '...';

  // options
  with OptionsGroupBox do
  begin
    Caption := lisOptions2;
    Items.Add(lisDiffDlgCaseInsensitive2);
    Items.Add(lisDiffDlgIgnoreIfEmptyLinesWereAdd2);
    Items.Add(lisDiffDlgIgnoreSpacesAtStartOfLine2);
    Items.Add(lisDiffDlgIgnoreSpacesAtEndOfLine2);
    Items.Add(lisDiffDlgIgnoreIfLineEndCharsDiffe2);
    Items.Add(lisDiffDlgIgnoreIfSpaceCharsWereAdd2);
    Items.Add(lisDiffDlgIgnoreSpaces2);
  end;

  // buttons
  IDEImages.AssignImage(CancelScanningButton, 'btn_cancel');
  CloseButton.Caption := lisClose2;
  OpenInEditorButton.Caption := lisDiffDlgOpenDiffInEditor2;
  HelpButton.Caption := lisMenuHelp2;

  OpenInEditorButton.LoadGlyphFromStock(idButtonOpen);
  if OpenInEditorButton.Glyph.Empty then
    IDEImages.AssignImage(OpenInEditorButton, 'laz_open');
  // dialogs
  dlgOpen.Title := lisOpenExistingFile2;
  dlgOpen.Filter:=dlgFilterAll2+' ('+GetAllFilesMask2+')|'+GetAllFilesMask2
                 +'|'+dlgFilterLazarusUnit2+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+dlgFilterLazarusProject2+' (*.lpi)|*.lpi'
                 +'|'+dlgFilterLazarusForm2+' (*.lfm;*.dfm)|*.lfm;*.dfm'
                 +'|'+dlgFilterLazarusPackage2+' (*.lpk)|*.lpk'
                 +'|'+dlgFilterLazarusProjectSource2+' (*.lpr)|*.lpr';
  //tabsheets
  tbsOptions.Caption := lisDiffTabOptions2;
  tbsCompare.Caption := lisDiffTabCompare2;
  tbsPatch.Caption := lisDiffTabPatch2;

  //buttons
  btnCompare.Caption := lisDiffBtnCompare2;
  btnFirst.Caption := lisDiffBtnFirst2;
  btnLast.Caption := lisDiffBtnLast2;
  btnNext.Caption := lisDiffBtnNext2;
  btnprevious.Caption := lisDiffBtnPrevious2;
  // diff
  //  EditorOpts.GetSynEditSettings(DiffSynEdit);

  SourceEditorManagerIntf.GetEditorControlSettings(edLeft);
  SourceEditorManagerIntf.GetEditorControlSettings(edRight);
  // code folding not allowed.
  edLeft.Gutter.Parts[4].Visible := False;
  edRight.Gutter.Parts[4].Visible := False;
  SourceEditorManagerIntf.GetHighlighterSettings(SynFreePascalSyn1);
  IDEImages.AssignImage(btnFindL, 'menu_search_find');
  IDEImages.AssignImage(btnFindR, 'menu_search_find');
  IDEImages.AssignImage(btnFindPreviousL, 'menu_search_find_previous');
  IDEImages.AssignImage(btnFindPreviousR, 'menu_search_find_previous');
  IDEImages.AssignImage(btnFindNextL, 'menu_search_find_next');
  IDEImages.AssignImage(btnFindNextR, 'menu_search_find_next');
end;

procedure TDiffForm.UpdateDiff;
begin
end;

procedure TDiffForm.Init;
var
  LastText2Name: string;
  i: integer;
begin
  // fill all diff file names
  FillTextComboBoxes;

  // get recent Text 2
  i := 0;
  //  LastText2Name:=InputHistories.DiffText2;
  if LastText2Name <> '' then
    i := fAvailableFiles.IndexOfName(LastText2Name);
  if i < 0 then i := 0;
  if i = fAvailableFiles.IndexOf(fSelectedFile2.fFile) then Inc(i);
  fSelectedFile2.SetIndex(i);
  // set recent options
  LoadSettings;
end;

procedure TDiffForm.FillTextComboBoxes;
var
  i: integer;
begin
  // Text 1
  Text1Combobox.Items.BeginUpdate;
  Text1Combobox.Items.Clear;
  for i := 0 to fAvailableFiles.Count - 1 do
    Text1Combobox.Items.Add(fAvailableFiles[i].Name);
  Text1Combobox.Items.EndUpdate;

  // Text 2
  Text2Combobox.Items.BeginUpdate;
  Text2Combobox.Items.Clear;
  for i := 0 to fAvailableFiles.Count - 1 do
    Text2Combobox.Items.Add(fAvailableFiles[i].Name);
  Text2Combobox.Items.EndUpdate;
end;

procedure TDiffForm.FormCreate(Sender: TObject);
begin
  //Text1Combobox.DropDownCount:=EnvironmentOptions.DropDownCount;
  //Text2Combobox.DropDownCount:=EnvironmentOptions.DropDownCount;
  fCurrentOptions := GetDiffOptions;
  PageControl1.TabIndex := 0;
end;

procedure TDiffForm.SaveSettings;
var
  DiffFlags: TTextDiffFlags2;
  Config: TConfigStorage;
const
  Version = 1;
begin
  DiffFlags := GetDiffOptions;
  try
    Config := GetIDEConfigStorage('lazdiffcomparefiles.xml', False);
    try
      // store the version number so future extensions can handle old config files
      Config.SetDeleteValue('Version', Version, 0);
      // store string variable "SomeValue"
      // if SomeValue has the default value the entry is not stored,
      // so only the differences to the default are stored.
      // This way the xml is kept short and defaults may change in future.
      Config.SetDeleteValue('Options/IgnoreCase', tdfIgnoreCase in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreEmptyLineChanges',
        tdfIgnoreEmptyLineChanges in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreHeadingSpaces', tdfIgnoreHeadingSpaces in
        DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreLineEnds', tdfIgnoreLineEnds in
        DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreSpaceCharAmount', tdfIgnoreSpaceCharAmount in
        DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreSpaceChars', tdfIgnoreSpaceChars in
        DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreTrailingSpaces', tdfIgnoreTrailingSpaces in
        DiffFlags, False);
      if (fSelectedFile2 <> nil) and (fSelectedFile2.fFile <> nil) then
      begin
        Config.SetDeleteValue('Options/DiffText2', fSelectedFile2.fFile.Name, '');
        Config.SetDeleteValue('Options/Text2OnlySelection',
          Text2OnlySelectionCheckBox.Checked, False);
      end
      else
      begin
        Config.SetDeleteValue('Options/DiffText2', '', '');
        Config.SetDeleteValue('Options/Text2OnlySelection', False, False);
      end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
       AddIDEMessage(mluWarning,'Saving lazdiffcomparefiles.xml failed: '+ E.Message);
    end;
  end;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TDiffForm.LoadSettings;
var
  DiffFlags: TTextDiffFlag2;
  Config: TConfigStorage;
  Version:Integer;
  i:integer;
  LastText2Name:string;
begin
  try
    Config := GetIDEConfigStorage('lazdiffcomparefiles.xml', true);
    try
      Version:=Config.GetValue('Version', 1);
      OptionsGroupBox.Checked[IgnoreCaseCheckBox] :=Config.GetValue('Options/IgnoreCase', False);
      OptionsGroupBox.Checked[IgnoreEmptyLineChangesCheckBox] :=Config.GetValue('Options/IgnoreEmptyLineChanges', False);
      OptionsGroupBox.Checked[IgnoreHeadingSpacesCheckBox] := Config.GetValue('Options/IgnoreHeadingSpaces', False);
      OptionsGroupBox.Checked[IgnoreLineEndsCheckBox] := Config.GetValue('Options/IgnoreLineEnds', False);
      OptionsGroupBox.Checked[IgnoreSpaceCharAmountCheckBox] := Config.GetValue('Options/IgnoreSpaceCharAmount', False);
      OptionsGroupBox.Checked[IgnoreSpaceCharsCheckBox] := Config.GetValue('Options/IgnoreSpaceChars', False);
      OptionsGroupBox.Checked[IgnoreTrailingSpacesCheckBox] := Config.GetValue('Options/IgnoreTrailingSpaces', False);
      // get recent Text 2
      i := 0;
      LastText2Name := Config.GetValue('Options/DiffText2', '');
      Text2OnlySelectionCheckBox.Checked := Config.GetValue('Options/Text2OnlySelection', False);
      if LastText2Name <> '' then
        i := fAvailableFiles.IndexOfName(LastText2Name);
      if i < 0 then i := 0;
      if i = fAvailableFiles.IndexOf(fSelectedFile2.fFile) then Inc(i);
      fSelectedFile2.SetIndex(i);
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      AddIDEMessage(mluWarning,'Loading lazdiffcomparefiles.xml failed: '+ E.Message);
    end;
  end;
end;

function TDiffForm.GetDiffOptions: TTextDiffFlags2;
begin
  Result := [];
  if OptionsGroupBox.Checked[IgnoreCaseCheckBox] then
    Include(Result, tdfIgnoreCase);
  if OptionsGroupBox.Checked[IgnoreEmptyLineChangesCheckBox] then
    Include(Result, tdfIgnoreEmptyLineChanges);
  if OptionsGroupBox.Checked[IgnoreHeadingSpacesCheckBox] then
    Include(Result, tdfIgnoreHeadingSpaces);
  if OptionsGroupBox.Checked[IgnoreLineEndsCheckBox] then
    Include(Result, tdfIgnoreLineEnds);
  if OptionsGroupBox.Checked[IgnoreSpaceCharAmountCheckBox] then
    Include(Result, tdfIgnoreSpaceCharAmount);
  if OptionsGroupBox.Checked[IgnoreSpaceCharsCheckBox] then
    Include(Result, tdfIgnoreSpaceChars);
  if OptionsGroupBox.Checked[IgnoreTrailingSpacesCheckBox] then
    Include(Result, tdfIgnoreTrailingSpaces);
end;

{ TAvailableDiffFile }

constructor TAvailableDiffFile.Create(const NewName: string;
  NewEditor: TSourceEditorInterface; NewSelectionAvailable: boolean);
begin
  Name := NewName;
  Editor := NewEditor;
  SelectionAvailable := NewSelectionAvailable;
end;

{ TAvailableDiffFiles }

function TAvailableDiffFiles.GetItems(Index: integer): TAvailableDiffFile;
begin
  Result := TAvailableDiffFile(inherited Items[Index]);
end;

procedure TAvailableDiffFiles.SetItems(Index: integer; const AValue: TAvailableDiffFile);
begin
  inherited Items[Index] := AValue;
end;

procedure TAvailableDiffFiles.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Clear;
end;

function TAvailableDiffFiles.Add(DiffFile: TAvailableDiffFile): integer;
begin
  Result := inherited Add(DiffFile);
end;

function TAvailableDiffFiles.IndexOfName(const Name: string): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].Name <> Name) do Dec(Result);
end;

end.
