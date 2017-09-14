unit opkman_repositorydetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TRepositoryDetailsFrm }

  TRepositoryDetailsFrm = class(TForm)
    bOk: TButton;
    bCancel: TButton;
    edName: TEdit;
    edAddress: TEdit;
    lbName: TLabel;
    lbAddress: TLabel;
    lbDescription: TLabel;
    lbOF2: TLabel;
    mDescription: TMemo;
    pnButtons: TPanel;
    procedure bOkClick(Sender: TObject);
    procedure edAddressChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAddress: String;
    function IsDuplicateRepository(const AAddress: String): Boolean;
  public
    property Address: String read FAddress;
  end;

var
  RepositoryDetailsFrm: TRepositoryDetailsFrm;

implementation
uses opkman_const, opkman_common, opkman_options;
{$R *.lfm}

{ TRepositoryDetailsFrm }

procedure TRepositoryDetailsFrm.FormCreate(Sender: TObject);
begin
  Caption := rsRepositoryDetailsFrm_Caption;
  lbName.Caption := rsRepositoryDetailsFrm_lbName_Caption;
  edName.Hint := rsRepositoryDetailsFrm_edName_Hint;
  lbAddress.Caption := rsRepositoryDetailsFrm_lbAddress_Caption;
  edAddress.Hint := rsRepositoryDetailsFrm_edAddress_Hint;
  lbDescription.Caption := rsRepositoryDetailsFrm_lbDescription_Caption;
  mDescription.Hint := rsRepositoryDetailsFrm_mDescription_Hint;
  bOk.Caption := rsRepositoryDetailsFrm_bOk_Caption;
  bOk.Hint := rsRepositoryDetailsFrm_bOk_Hint;
  bCancel.Caption := rsRepositoryDetailsFrm_bCancel_Caption;
  bCancel.Hint := rsRepositoryDetailsFrm_bCancel_Hint;
  FAddress := '';
end;

function TRepositoryDetailsFrm.IsDuplicateRepository(const AAddress: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Options.RemoteRepository.Count - 1 do
  begin
    if UpperCase(Options.RemoteRepository.Strings[I]) = UpperCase(AAddress) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TRepositoryDetailsFrm.bOkClick(Sender: TObject);
begin
  if Trim(edName.Text) = '' then
  begin
    MessageDlgEx(rsRepositoryDetailsFrm_Info1, mtInformation, [mbOk], Self);
    edName.SetFocus;
    Exit;
  end;
  if Trim(edAddress.Text) <> '' then
  begin
    FAddress := Trim(edAddress.Text);
    if FAddress[Length(FAddress)] <> '/' then
      FAddress := FAddress + '/';
    if IsDuplicateRepository(FAddress) then
      FAddress := '';
  end;
  ModalResult := mrOk;
end;

procedure TRepositoryDetailsFrm.edAddressChange(Sender: TObject);
begin
  edAddress.Font.Color := clDefault;
end;

end.

