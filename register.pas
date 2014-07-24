unit Register;

//========================================================================================
//
//  Register.pas
//
//
//  Calls: AppVariables
//         HUtils : CalculateRegistrationKey
//                  ErrorMessageDlgOk
//                  ValidAlphaCharacter
//                  ValidCallsignCharacter
//                  ValidDigitCharacter
//         Main : TfrmMain.DisplayTitleBar
//
//  Called By: INIStuff : ReadINIFile
//                        WriteINIFile
//             NagScreen : TdlgNagScreen.bbtRegisterClick
//
//  Ver: 1.0.0
//
//  Date: 21 Dec 2013
//
//========================================================================================

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,
  // Application Units
  AppVariables, HUtils;

type

   TdlgRegister = class(TForm)
    bbtOk: TBitBtn;
    bbtCancel: TBitBtn;
    bbtHelp: TBitBtn;
    edtName: TEdit;
    edtCallsign: TEdit;
    edtRegKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure bbtCancelClick(Sender: TObject);
    procedure bbtOkClick(Sender: TObject);
    procedure edtCallsignKeyPress(Sender: TObject; var Key: char);
    procedure edtNameKeyPress(Sender: TObject; var Key: char);
    procedure edtRegKeyKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dlgRegister: TdlgRegister;

implementation

{$R *.lfm}

uses
  Main;

const
  cbytMaxNameLength = 20;
  cbytMinNameLength = 2;
  cbytMaxCallsignLength = 10;
  cbytMinCallsignLength = 3;
  cbytRegKeyLength = 8;

  cstrNameEntry =         '       You must enter a Valid Name.';
  cstrValidName =         '      A Valid Name consists of two to 20' +
                          #13 +
                          '     Alphabetic characters and a space.';
  cstrCallsignEntry =     '   If entered, your Callsign must be valid.';
  cstrValidCallsign =     '      A Valid Callsign consists of three to 11' +
                          #13 +
                          '  Alphabetic and Numeric characters and the' +
                          #13 +
                          '               / character.';
  cstrRegKeyEntry =      '  You must enter a Valid Registration Key.';
  cstrValidRegKey =      '   A Valid Registration Key consists of eight' +
                         #13 +
                         '             Digits [0..9].';
  cstrUnmatchedRegKey =  'The Registration Key entered is not a correct' +
                         #13 +
                         ' match for the Name and/or Callsign entered.';
  cstrCancelButtonHint = 'Cancel the Registration Process.';
  cstrOKButtonHint = 'Complete the Registration Process.';

//========================================================================================
//          FORM ROUTINES
//========================================================================================
procedure TdlgRegister.FormShow(Sender: TObject);
begin
  dlgRegister.Caption := 'TMV7A Registration';
  edtName.Hint := cstrValidName;
  edtName.MaxLength := cbytMaxNameLength;
  edtName.Text := gvstrRegisteredName;
  edtCallsign.Hint := cstrValidCallsign;
  edtCallsign.MaxLength := cbytMaxCallsignLength;
  edtCallsign.Text := gvstrRegisteredCall;
  edtRegKey.Hint := cstrValidRegKey;
  edtRegKey.MaxLength := cbytRegKeyLength;
  edtRegKey.Text := gvstrRegistrationKey;
  bbtCancel.Hint := cstrCancelButtonHint;
  bbtOK.Hint := cstrOKButtonHint;
  edtName.SetFocus;
end;

//========================================================================================
//          BUTTON ROUTINES
//========================================================================================
procedure TdlgRegister.bbtOkClick(Sender: TObject);

var
  vstrRegString : string;

begin

  // We must have valid data in the Name and optionally Callsign fields as well as the
  // Registration Key field. If there is a Callsign, then it will be used to calculate
  // The Registration Key Value. If there is no Callsign, the Name will be used. The
  // Calculated value will be compared to the value entered into the Registration Key
  // field to determine Registration Validation.

  // First we check the validity of the fields. Any non-valid data entries will
  // cause an error message to be displayed and the form will remain open.

  // We require at least one of the Name or Callsign fields to calculate the
  // Registration Key

  // The name must be Alphabetic with 2 to 20 characters
  if (Length(edtName.Text) < cbytMinNameLength) then
  begin
    ErrorMessageDlgOk('Invalid Name', cstrNameEntry +
                                      #13 +
                                      #13 +
                                      cstrValidName);
    ModalResult := mrNone;
    edtName.SetFocus;
    Exit;
  end;// if (Length(edtName.Text) < cbytMinNameLength)

  // If present, the Callsign must be 3 to 10 Callsign characters (A..Z, 0..9 and /)
  if (Length(edtCallsign.Text) > 0) then
  begin
    if (Length(edtCallsign.Text) < cbytMinCallsignLength) then
    begin
      ErrorMessageDlgOk('Invalid Callsign', cstrCallsignEntry +
                                        #13 +
                                        #13 +
                                        cstrValidCallsign);
      ModalResult := mrNone;
      edtCallsign.SetFocus;
      Exit;
    end;// if (Length(edtCallsign.Text) < cbytMinCallsignLength)
  end;// if (Length(edtCallsign.Text) > 0

  // The Registration Key must be 8 digits (0..9)
  if Length(edtRegKey.Text) < 8 then
  begin
    ErrorMessageDlgOk('Invalid Registration Key', cstrRegKeyEntry +
                                      #13 +
                                      #13 +
                                      cstrValidRegKey);
    ModalResult := mrNone;
    edtRegKey.SetFocus;
    Exit;
  end;// if Length(edtRegKey.Text) = 0

  // Now we check the Registration key entered against the value calculated from
  // the Callsign or Name. If the calculated value is the same as the entered value
  // all is good and we consider the application "Registered", set the Registration
  // variables and exit the dialogue.
  //
  // If they are not the same, we display an error message and consider the
  // application "Not Registered", clear the Registration variables, display an error
  // message and remain in the dialogue.

  if Length(edtCallsign.Text) > 0 then
    vstrRegString := edtCallsign.Text[1] +  edtCallsign.Text[Length(edtCallsign.Text)]
  else
    vstrRegString := edtName.Text[1] +  edtName.Text[Length(edtName.Text)];

  if CalculateRegistrationKey(vstrRegString) = edtRegKey.Text then
  begin
    // Registration was Successful
    gvblnRegistered := True;
    gvstrRegisteredName := edtName.Text;
    gvstrRegisteredCall := edtCallsign.text;
    gvstrRegistrationKey := edtRegKey.Text;
    frmMain.DisplayTitleBar;
  end
  else
  begin
    // OOoopppsss
    ErrorMessageDlgOk('Incorrect Registration Key', cstrRegKeyEntry +
                                      #13 +
                                      #13 +
                                      cstrUnmatchedRegKey);
    ModalResult := mrNone;
    edtRegKey.SetFocus;
  end;// if CalculateRegistrationKey('aa') = edtRegKey.Text

end;// procedure TdlgRegister.bbtOkClick

//========================================================================================
procedure TdlgRegister.bbtCancelClick(Sender: TObject);
begin
  if ConfirmationMessageDlg('Confirm Cancellation',
       'Confirm you wish to cancel the Registration.') = mrNo then
  begin
    ModalResult := mrNone;
    edtName.SetFocus;
  end;// if ConfirmationMessageDlg
end;// procedure TdlgRegister.bbtCancelClick

//========================================================================================
//          KEYPRESS ROUTINES
//========================================================================================
procedure TdlgRegister.edtNameKeyPress(Sender: TObject; var Key: char);
begin
  Key := ValidAlphaCharacter(Key);  //showmessage(IntToStr(Ord(Key)));
end;// procedure TdlgRegister.Edit2KeyPress

//========================================================================================
procedure TdlgRegister.edtCallsignKeyPress(Sender: TObject; var Key: char);
begin
  Key := ValidCallsignCharacter(Key);
end;// procedure TdlgRegister.edtCallsignKeyPress

//========================================================================================
procedure TdlgRegister.edtRegKeyKeyPress(Sender: TObject; var Key: char);
begin
  Key := ValidDigitCharacter(Key);
end;// procedure TdlgRegister.edtRegKeyKeyPress

//========================================================================================
end.// unit Register;

