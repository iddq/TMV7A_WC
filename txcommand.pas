unit TXCommand;

{$mode objfpc}{$H+}

//========================================================================================
//
//  TXCommand.pas
//
//
//  Calls: AppCOnstants
//         AppVariables
//         LCDDisplay : DisplayUHFTXStatus
//                      DisplayVHFTXStatus
//
//  Called By: ResponseHandler : ParseResponse
//
//  Ver: 1.0.0
//
//  Date: 29 Nov 2013
//
//========================================================================================

interface

uses
  Classes, SysUtils,
  // Application Units
  AppConstants, AppVariables, LCDDisplay;

procedure TXResponseHandler(vstrKeywordRcvd, vstrParameters : string);

implementation

procedure TXResponseHandler(vstrKeywordRcvd, vstrParameters : string);
begin
  if gvstrPTTBand = gcstrUHF then
  begin
    DisplayUHFBusyStatus('ON AIR');
  end
  else
  begin
    DisplayVHFBusyStatus('ON AIR');
  end;
  gvblnTXStatus := True;
;end;// procedure TXResponseHandler

//========================================================================================
end.// unit TXCommand;

