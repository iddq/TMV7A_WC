unit Configure;

{$mode delphi}

//========================================================================================
//
//  Configure.pas
//
//  Calls: AppConstants
//         AppVariables
//         ColourSchemes
//         HUtils
//
//  Called By: Init : Initialize
//             Main : TfrmMain.mnuConfigureClick
//
//  Ver: 1.0.0
//
//  Date: 31 Aug 2014
//
//========================================================================================


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Spin,
  //  Application units
  AppConstants, AppVariables, ColourSchemes, HUtils;

type

  { TfrmConfigure }

  TfrmConfigure = class(TForm)
    bbtSave: TBitBtn;
    bbtCancel: TBitBtn;
    bbtReset: TBitBtn;
    cbxDefVHFStep: TComboBox;
    cbxDefUHFStep: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblFrequency: TLabel;
    lblText1: TLabel;
    Panel1: TPanel;
    rbt5MHz: TRadioButton;
    rbt16MHz: TRadioButton;
    rbtColourScheme1: TRadioButton;
    rbtColourScheme10: TRadioButton;
    rbtColourScheme2: TRadioButton;
    rbtColourScheme3: TRadioButton;
    rbtColourScheme4: TRadioButton;
    rbtColourScheme5: TRadioButton;
    rbtColourScheme6: TRadioButton;
    rbtColourScheme7: TRadioButton;
    rbtColourScheme8: TRadioButton;
    rbtColourScheme9: TRadioButton;
    rbt1200: TRadioButton;
    rbt9600: TRadioButton;
    sedCOMPort: TSpinEdit;
    procedure bbtCancelClick(Sender: TObject);
    procedure bbtSaveClick(Sender: TObject);
    procedure bbtResetClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbtColourScheme10Change(Sender: TObject);
    procedure rbtColourScheme1Change(Sender: TObject);
    procedure rbtColourScheme2Change(Sender: TObject);
    procedure rbtColourScheme3Change(Sender: TObject);
    procedure rbtColourScheme4Change(Sender: TObject);
    procedure rbtColourScheme5Change(Sender: TObject);
    procedure rbtColourScheme6Change(Sender: TObject);
    procedure rbtColourScheme7Change(Sender: TObject);
    procedure rbtColourScheme8Change(Sender: TObject);
    procedure rbtColourScheme9Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmConfigure: TfrmConfigure;

implementation

{$R *.lfm}

{ TfrmConfigure }

var
  vbytOrigComPort : Byte;
  vintOrigBaudRate : Integer;
  vstrOrigUHFOffset : String;
  vstrOrigUHFStep : String;
  vstrOrigVHFStep : string;
  vstrOrigColourScheme : String;

//========================================================================================
//          SUPPORT ROUTINES
//========================================================================================
procedure ShowColourScheme;
begin

  if frmConfigure.rbtColourScheme1.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor1;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor1;
    frmConfigure.lblText1.Font.Color := vclrForeColor1;
  end
  else if frmConfigure.rbtColourScheme2.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor2;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor2;
    frmConfigure.lblText1.Font.Color := vclrForeColor2;
  end
  else if frmConfigure.rbtColourScheme3.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor3;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor3;
    frmConfigure.lblText1.Font.Color := vclrForeColor3;
  end
  else if frmConfigure.rbtColourScheme4.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor4;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor4;
    frmConfigure.lblText1.Font.Color := vclrForeColor4;
  end
  else if frmConfigure.rbtColourScheme5.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor5;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor5;
    frmConfigure.lblText1.Font.Color := vclrForeColor5;
  end
  else if frmConfigure.rbtColourScheme6.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor6;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor6;
    frmConfigure.lblText1.Font.Color := vclrForeColor6;
  end
  else if frmConfigure.rbtColourScheme7.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor7;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor7;
    frmConfigure.lblText1.Font.Color := vclrForeColor7;
  end
  else if frmConfigure.rbtColourScheme8.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor8;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor8;
    frmConfigure.lblText1.Font.Color := vclrForeColor8;
  end
  else if frmConfigure.rbtColourScheme9.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor9;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor9;
    frmConfigure.lblText1.Font.Color := vclrForeColor9;
  end
  else if frmConfigure.rbtColourScheme10.Checked then
  begin
    frmConfigure.Panel1.Color := vclrBackColor10;
    frmConfigure.lblFrequency.Font.Color := vclrForeColor10;
    frmConfigure.lblText1.Font.Color := vclrForeColor10;
  end;// if frmConfigure.rbtColourScheme1.Checked

end;// procedure ShowColourScheme

//========================================================================================
procedure InitializeForm;
begin

  //====================
  //  COM Port
  //====================
  frmConfigure.sedCOMPort.Value := vbytOrigComPort;

  if vintOrigBaudRate = gcint1200Baud then
    frmConfigure.rbt1200.Checked := True
  else
    frmConfigure.rbt9600.Checked := True;

  //====================
  // UHF Offset
  //====================
  if vstrOrigUHFOffset = gcstrUHFShiftOffsetA then
    frmConfigure.rbt5MHz.Checked := True
  else
    frmConfigure.rbt16MHz.Checked := True;

  //====================
  //  Frequency Steps
  //====================
  frmConfigure.cbxDefVHFStep.ItemIndex := StrToInt(gcstrConfigVHFStep);
  frmConfigure.cbxDefUHFStep.ItemIndex := StrToInt(gcstrConfigUHFStep);

  //====================
  //     Colour Scheme
  //====================
  case StrToInt(vstrOrigColourScheme) of
    1 : frmConfigure.rbtColourScheme1.Checked := True;
    2 : frmConfigure.rbtColourScheme2.Checked := True;
    3 : frmConfigure.rbtColourScheme3.Checked := True;
    4 : frmConfigure.rbtColourScheme4.Checked := True;
    5 : frmConfigure.rbtColourScheme5.Checked := True;
    6 : frmConfigure.rbtColourScheme6.Checked := True;
    7 : frmConfigure.rbtColourScheme7.Checked := True;
    8 : frmConfigure.rbtColourScheme8.Checked := True;
    9 : frmConfigure.rbtColourScheme9.Checked := True;
    10 : frmConfigure.rbtColourScheme10.Checked := True;
  end;// case StrToInt(vstrOrigColourScheme)

  ShowColourScheme;

end;// procedure InitializeForm

//========================================================================================
//          FORM ROUTINES
//========================================================================================
procedure TfrmConfigure.FormCreate(Sender: TObject);
//
// This module sets the Global Configration variables to their default Global values.
// Although this happens every time the program is run, it is only on pRogram installation
// when there is no .ini file that gthey are used. Once an .ini file has been created, the
// values from the .ini file are used.
var
  vbytTemp : Byte;
begin
{
  // Set the data fields to the default offset values
//  gvstrCOMPort := 'COM' + gcstrDefCOMPort;
  gvstrCOMPort := gcstrDefCOMPort;
  gvintBaudRate := gcintDefBaudRate;

  gvstrUHFOffset := gcstrUHFShiftOffsetA;
 }
  for vbytTemp := 0 to gcbytMaxStepIndex do
    cbxDefVHFStep.Items.Add(gvstrStepArray[vbytTemp]);
//  gvstrVHFStep := gcstrConfigVHFStep;

  for vbytTemp := 0 to gcbytMaxStepIndex do
    cbxDefUHFStep.Items.Add(gvstrStepArray[vbytTemp]);
//  gvstrUHFStep := gcstrConfigUHFStep;

//  gvstrCurrentColourScheme := gcstrDefaultColourScheme;

end;// procedure TfrmConfigure.FormCreate

//========================================================================================
procedure TfrmConfigure.FormActivate(Sender: TObject);
//
// This module sets the Global Configration variables to the values that have been loaded
// by the .ini file, overiding the default values loaded in fromCreate.
begin

  //====================
  // Save the current data
  //====================

  //====================
  //     COM Port
  //====================
//  vbytOrigComPort := StrToInt(Copy(gvstrCOMPort,4, Length(gvstrCOMPort)));
  vbytOrigComPort := StrToInt(gvstrCOMPort);

  if gvintBaudRate = 0 then
    vintOrigBaudRate := gcintDefBaudRate;
  vintOrigBaudRate := gvintBaudRate;

  //====================
  //     UHF Offset
  //====================
  vstrOrigUHFOffset := gcstrUHFShiftOffsetA;

  //====================
  //     Frequency Step
  //====================
  vstrOrigUHFStep := gcstrConfigUHFStep;
  vstrOrigVHFStep := gcstrConfigVHFStep;

  // Colour Scheme
  vstrOrigColourScheme := gvstrCurrentColourScheme;

  // Now setup the form
  InitializeForm;

end;// procedure TfrmConfigure.FormActivate

//========================================================================================
//          BUTTON ROUTINES
//========================================================================================
procedure TfrmConfigure.bbtResetClick(Sender: TObject);
// Re-load the form data fields that were stored when the form was activated, effectively
// cancelling any changes made, and keeps the form open.
begin
  if ConfirmationMessageDlg('Confirm', 'Reset') = mrNo then
  begin
    ModalResult := mrNone;
    Exit;
  end;// if ConfirmationMessageDlg('Confirm', 'Reset') = mrCancel then
  InitializeForm;
  ModalResult := mrNone;
end;// procedure TfrmConfigure.bbtResetClick

//========================================================================================
procedure TfrmConfigure.bbtCancelClick(Sender: TObject);
// This routine simply closes the form without making any changes to the Global
// Configuration variables.
begin
  if ConfirmationMessageDlg('Confirm', 'Cancel') = mrNo then
  begin
    ModalResult := mrNone;
    Exit;
  end;// if ConfirmationMessageDlg('Confirm', 'Reset') = mrCancel then
  // ModalResult := mrCancel
end;// procedure TfrmConfigure.bbtCancelClick

//========================================================================================
procedure TfrmConfigure.bbtSaveClick(Sender: TObject);
// This rutine saves the changes made to the Global Configuration variables
// and then closes the form.
begin

  // Check and Info message here to shut down and re-start if either the COM Port
  // or Baud Rate have been changed.

  ShowMessage('Shut down and restart the program' +
              #13 +
              'if you have changed either the COM Port' +
              #13 +
              'or Baud Rate.');

  //====================
  // COM Port
  //====================
//  gvstrCOMPort := 'COM' + sedCOMPort.Text;
  gvstrCOMPort :=sedCOMPort.Text;

  if rbt1200.Checked then
    gvintBaudRate := gcint1200Baud
  else
    gvintBaudRate := gcint9600Baud;

  //====================
  // UHF Offset
  //====================
  if rbt5MHz.Checked then
    gvstrUHFOffset := gcstrUHFShiftOffsetA
  else
    gvstrUHFOffset := gcstrUHFShiftOffsetE;

  //====================
  //     Step Values
  //====================
  gvstrVHFStep := IntToStr(cbxDefVHFStep.ItemIndex);
  gvstrUHFStep := IntToStr(cbxDefUHFStep.ItemIndex);

  //====================
  //     Colour Scheme
  //====================
  if frmConfigure.rbtColourScheme1.Checked then
    gvstrCurrentColourScheme := '1'
  else if frmConfigure.rbtColourScheme2.Checked then
    gvstrCurrentColourScheme := '2'
  else if frmConfigure.rbtColourScheme3.Checked then
    gvstrCurrentColourScheme := '3'
  else if frmConfigure.rbtColourScheme4.Checked then
    gvstrCurrentColourScheme := '4'
  else if frmConfigure.rbtColourScheme5.Checked then
    gvstrCurrentColourScheme := '5'
  else if frmConfigure.rbtColourScheme6.Checked then
    gvstrCurrentColourScheme := '6'
  else if frmConfigure.rbtColourScheme7.Checked then
    gvstrCurrentColourScheme := '7'
  else if frmConfigure.rbtColourScheme8.Checked then
    gvstrCurrentColourScheme := '8'
  else if frmConfigure.rbtColourScheme9.Checked then
    gvstrCurrentColourScheme := '9'
  else if frmConfigure.rbtColourScheme10.Checked then
    gvstrCurrentColourScheme := '10';

  SetColourScheme(gvstrCurrentColourScheme);

end;// procedure TfrmConfigure.btnSaveClick

//========================================================================================
//          ON CHANGE ROUTINES
//========================================================================================
procedure TfrmConfigure.rbtColourScheme1Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme1Change(Sender: TObject);

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme2Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme2Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme3Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme3Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme4Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme4Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme5Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme5Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme6Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme6Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme7Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme7Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme8Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme8Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme9Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme9Change

//----------------------------------------------------------------------------------------
procedure TfrmConfigure.rbtColourScheme10Change(Sender: TObject);
begin
  ShowColourScheme;
end;// procedure TfrmConfigure.rbtColourScheme10Change

//========================================================================================

end.// unit Configure;

