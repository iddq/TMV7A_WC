unit COMPort;

{$mode objfpc}{$H+}

//========================================================================================
//
//  COMPort.pas
//
//  Calls: Main
//         SerialStuff : OpenPort
//         Variables
//
//  Called By: BCCommand : TogglePTTBand
//                         GetBCStatus
//             Main : TfrmMain.mnuConfigCOMPortClick
//             PSCOMMAND : TogglePowerOnOff
//             SerialStuff : OpenPort
//
//  Ver: 1.0.0
//
//  Date: 31 Aug 2014
//
//========================================================================================

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons, ExtCtrls,
  // Application units
  AppVariables;

type

  { TfrmCOMPort }

  TfrmCOMPort = class(TForm)
    bbtOk: TBitBtn;
    Label1: TLabel;
    speCOMPort: TSpinEdit;
    procedure bbtOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmCOMPort: TfrmCOMPort;

implementation

{$R *.lfm}

uses
  Main, SerialStuff;

{ TfrmCOMPort }

//========================================================================================
//          FORM ROUTINES
//========================================================================================
procedure TfrmCOMPort.FormShow(Sender: TObject);
begin
    //  Set Spinedity control to the current COM Port
    if gvstrCOMPort = '' then
      speCOMPort.Value := 1
    else
//      speCOMPort.Value := StrToInt(Copy(gvstrCOMPort,4, Length(gvstrCOMPort)));
      speCOMPort.Value := StrToInt(gvstrCOMPort);
end;// procedure TfrmCOMPort.FormShow

//========================================================================================
//          BUTTON ROUTINES
//========================================================================================
procedure TfrmCOMPort.bbtOkClick(Sender: TObject);
begin
    gvstrCOMPort := 'COM' + speCOMPort.Text;
end;// procedure TfrmCOMPort.bbtOkClick

//========================================================================================
end.// unit COMPort

