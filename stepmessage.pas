unit StepMessage;

{$mode delphi}

//========================================================================================
//
//  StepMEssage.pas
//
//  Calls: DataEntry
//
//  Called By: DataEntry : TfrmDataEntry.edtRXFrequencyExit
//
//  Ver: 1.0.0
//
//  Date: 21 Aug 2014
//
//========================================================================================


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TfrmStepMessage }

  TfrmStepMessage = class(TForm)
    bbtCorrect: TBitBtn;
    bbtAccept: TBitBtn;
    edtCorrectedRXFreq: TEdit;
    edtCurrentRXFreq: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblStepMessage: TLabel;
    procedure bbtCorrectClick(Sender: TObject);
    procedure bbtAcceptClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmStepMessage: TfrmStepMessage;

implementation

{$R *.lfm}

{ TfrmStepMessage }

uses
    DataEntry;

const
  cintVHFStepValue : array [0..7] of longint = (23600, 18880, 11800, 9440, 7867, 5900,
                                               4720, 2360);
  cintUHFStepValue : array [0..7] of longint = (34799, 27839, 17399, 13919, 11599, 8699,
                                               6959, 3479);

//========================================================================================
//          FORM ROUTINES
//========================================================================================
procedure TfrmStepMessage.FormActivate(Sender: TObject);
var
  vintFR1 : longint;
  vintOF1  : Integer;
  vstrTStr : string;
  vintStepValue : longint;
  vbytTemp : byte;
  vblnDone : boolean;

begin

  frmSTepMessage.lblStepMEssage.Caption := 'A non-standard Step value selected for this band.' +
                                           #13 +
                                           '   Select the <Correct> button to correct the' +
                                           #13 +
                                           '   RX Frequency to the standard Step frequency,' +
                                           #13 +
                                                'or select the <Accept> button to accept the' +
                                           #13 +
                                           '             frequency as entered.';
{  // First we get the slected Step and convert it to Hz
  vstrTStr := frmDataEntry.cbxStep.Text;
  vintStepValue := Trunc(StrToFloat(vstrTStr) * 1000);

  // Now we convert the entered RX Frequency to Hz
  vstrTStr := frmDataEntry.edtRXFrequency.Text;
  vintFR1 := Trunc(StrToFloat(vstrTStr) * 1000000);

  // Now calculate the new 'corrected' frequency by using the selected Step to create a
  // Step Offset value that will be multiplied against the Step value to create a
  // corrected frequency.
  vintOF1 := vintFR1 DIV vintStepValue;
  vintFR1 := vintStepValue * vintOF1;

  // Now we convert the corrected frequency to a string and remove trailing zeros
  vstrTStr := Copy(IntToStr(vintFR1),1,3) +
              '.' +
              Copy(IntToStr(vintFR1),4,Length(vstrTStr));

  vblnDone := False;
  repeat
    vbytTemp := Length(vstrTStr);
    if vbytTemp > 7 then
    begin
      if vstrTStr[vbytTemp] = '0' then
        vstrTStr := Copy(vstrTStr,1,vbytTemp-1)
      else
        vblnDone := True;// if vstrTStr[vbytTemp] = '0'
    end
    else
      vblnDone := True;// if vbytTemp > 7 then

  until vblnDone; }

  // and place both frequencies in the appropriate edit boxes.
  edtCorrectedRXFreq.Text := DataEntry.gvstrCorrectedFrequency;
  edtCurrentRXFreq.Text := frmDataEntry.edtRXFrequency.Text;

end;// procedure TfrmStepMessage.FormActivate

//========================================================================================
//          BUTTON ROUTINES
//========================================================================================
procedure TfrmStepMessage.bbtCorrectClick(Sender: TObject);
begin
  frmDataEntry.edtRXFrequency.Text := edtCorrectedRXFreq.Text;
end;// procedure TfrmStepMessage.bbtCorrectClick

//========================================================================================
procedure TfrmStepMessage.bbtAcceptClick(Sender: TObject);
begin
  frmDataEntry.edtRXFrequency.Text := edtCurrentRXFreq.Text;
end;// procedure TfrmStepMessage.bbtAcceptClick

//========================================================================================
end.// unit StepMessage

