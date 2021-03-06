unit BCCommand;

{$mode objfpc}{$H+}

//========================================================================================
//
//  BCCommand.pas
//
// Description: Reads and Sets the band Control status according to the following truth
// table. Note that only PTT BAND CHANGES are processed in this version.
//
//                 STATE                 Control      PTT
//                   VHF both              0           0
//                   UHF both              1           1
//                   VHF Control UHF PTT   0           1
//                   UHF Control VHF PTT   1           0
//
//  Calls: AppConstants
//         LCDDisplay : DisplayBCStatus
//         SerialStuff : SendCommand
//         Variables
//
//  Called By: Fav : SetFAVChannel
//             Init : Initialize
//             Main.TfrmMain.btnBandClick
//             Mem_VHF : TfrmMEM.bbtSelectClick
//             ResponseParser : ParseResponse
//
//  Ver: 1.0.0
//
//  Date: 21 Jun 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, SysUtils,
  // Application Units
  AppConstants, LCDDisplay, SerialStuff, AppVariables;

procedure BCResponseHandler (vstrKeywordRcvd, vstrParameters : string);
procedure TogglePTTBand;
procedure GetBCStatus;
procedure SetVHFBand;
procedure SetUHFBand;

implementation

//========================================================================================

procedure BCResponseHandler (vstrKeywordRcvd, vstrParameters : string);

begin

  // Right now, all we are processing is the Band change, which is the second parameter.
  gvstrPTTBand := Copy (vstrParameters, 3, 1);
  gvstrCtrlBand := Copy (vstrParameters, 1, 1);
  DisplayBCStatus;

end;// procedure BCResponseHandler (vstrKeywordRcvd, vstrParameters : string)

//========================================================================================

procedure TogglePTTBand;
begin

  if gvstrPTTBand = gcstrVHF then
  begin
     SendCommand('BC','0,' + gcstrUHF);
  end
  else
  begin
    SendCommand('BC','0,' + gcstrVHF);
   end;// if gvstrPTTBand = gcstrVHF

end;//procedure TogglePTTBand

//========================================================================================

procedure GetBCStatus;
begin
  SendCommand('BC','');
end;// procedure GetBCStatus;

//========================================================================================
procedure SetVHFBand;
begin
  SendCommand('BC', '0,' + gcstrVHFVFO );
end;// procedure SetVHFBand;

//========================================================================================
procedure SetUHFBand;
begin
  SendCommand('BC', '0,' + gcstrUHFVFO );
end;// procedure SetUHFBand;

//========================================================================================
end.// unit BCCommand

