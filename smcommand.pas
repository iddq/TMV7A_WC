unit SMCommand;

{$mode objfpc}{$H+}

//=========================================================================================
//
//  SMCommand.pas
//
//  This unit gets and processes the S Meter values for both UHF and VHF VFOs.
//
//  Calls: AppConstants
//         SerialStuff : SendCommand
//
//  Called By: Init : Initialize
//             ResponseParser : ParseResponse
//
//  Ver: 1.0.0
//
//  Date: 5 Jul 2013
//
//=========================================================================================

interface

uses
  Classes, Dialogs, SysUtils,
  // Application units
  AppCOnstants, SerialStuff;

procedure GetVHF_SMValue;
procedure GetUHF_SMValue;
procedure SMResponseHandler(vstrKeywordRcvd, vstrParameters : string);

implementation

uses
  Main;

procedure SMResponseHandler (vstrKeywordRcvd, vstrParameters : string);

var
  vstrBand : string;
  vstrValue : string;

begin

  // First we parse the reponse parameters (P1,P2) where P1 id the Band (0 = VHF, 1 = UHF)
  // and P2 is the S Meter value (0..7).
  vstrBand := Copy(vstrParameters, 1,1);
  vstrValue := Copy(vstrParameters, 3,2);

  if vstrBand = gcstrUHF then
  begin
    if StrToInt(vstrValue) > 0 then
    begin
      frmMain.pbrUHFSmeter.position := StrToInt(vstrValue);
      frmMain.pbrUHFSmeter.Visible := True;
    end
    else
    begin
      frmMain.pbrUHFSmeter.position := 0;
      frmMain.pbrUHFSmeter.Visible := False;
    end;// if vstrValue > 0
  end
  else
  begin
    if StrToInt(vstrValue) > 0 then
    begin
      frmMain.pbrVHFSmeter.position := StrToInt(vstrValue);
      frmMain.pbrVHFSmeter.Visible := True;
    end
    else
    begin
      frmMain.pbrVHFSmeter.position := 0;
      frmMain.pbrVHFSmeter.Visible := False;
    end;// if vstrValue > 0
  end;// if vstrBand = gcstrUHF

end;// procedure SMResponseHandler

//========================================================================================

procedure GetUHF_SMValue;
begin
  SendCommand('SM',gcstrUHF);
end;// procedure GetUHF_SMValue;

//========================================================================================

procedure GetVHF_SMValue;
begin
  SendCommand('SM',gcstrVHF);
end;// procedure GetVHF_SMValue;

//========================================================================================

end.// unit SMCommand;

