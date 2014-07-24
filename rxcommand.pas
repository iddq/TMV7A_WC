unit RXCommand;

{$mode objfpc}{$H+}

//========================================================================================
//
//  RXCommand.pas
//
//
//  Calls: AppConstants
//         AppVariables
//         LCDDisplay : DisplayUHFTXStatus
//                      DisplayVHFTXStatus
//
//  Called By: ResponseHandler : ParseResponse
//
//  Ver: 1.0.0
//
//  Date: 6 Apr 2014
//
//========================================================================================

interface

uses
  Classes, SysUtils,
  // Application Units
  AppConstants, AppVariables, LCDDisplay;

procedure RXResponseHandler(vstrKeywordRcvd, vstrParameters : string);

implementation

procedure RXResponseHandler(vstrKeywordRcvd, vstrParameters : string);

begin
  if gvstrPTTBand = gcstrUHF then
      DisplayUHFBusyStatus('')
  else
      DisplayVHFBusyStatus('');
      gvblnTXStatus := False;
end;// RXResponseHandler

//========================================================================================
end.// unit RXCommand;

