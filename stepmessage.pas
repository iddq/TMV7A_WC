unit StepMessage;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TfrmStepMessage }

  TfrmStepMessage = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    lblStepMessage: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
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

procedure TfrmStepMessage.FormActivate(Sender: TObject);
begin

  frmSTepMEssage.lblStepMEssage.Caption := 'A non-standard Step value selected for this band.' +
                                           #13 +
                                           '   Select the <Correct> button to correct the' +
                                           #13 +
                                           '   RX Frequency to the standard Step frequency,' +
                                           #13 +
                                                'or select the <Accept> button to accept the' +
                                           #13 +
                                           '             frequency as entered.';

end;

procedure TfrmStepMessage.BitBtn1Click(Sender: TObject);
begin

end;

procedure TfrmStepMessage.BitBtn2Click(Sender: TObject);
begin

end;

end.

