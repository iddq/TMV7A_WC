unit PSCommand;

{$mode objfpc}{$H+}

//========================================================================================
//
//  PSCommand.pas
//
// Description:
//
//  Calls: AppConstants
//         AppVariables
//         Init : TMV7_Init
//         LCDDisplay : LCDOn
//                      LCDOff
//         Reverse : UHFReverseOff
//                   VHFReverseOff
//         SerialStuff : SendCOmmand
//
//  Called By: Main : TfrmMain.btnPowerClick
//                    ResponseParser : ParseResponse
//
//  Ver: 1.0.0
//
//  Date: 6 Apr 2014
//
//========================================================================================

interface

uses
  Classes, Dialogs, SysUtils,
  // Application Units
  AppConstants, AppVariables, Init, LCDDisplay, Reverse, SerialStuff;

procedure PSResponseHandler (vstrKeywordRcvd, vstrParameters : string);
procedure TogglePowerOnOff;

implementation

uses
  Main;

//========================================================================================

procedure PSResponseHandler (vstrKeywordRcvd, vstrParameters : string);

begin

  if vstrParameters = gcstrOn then
  begin
    gvblnPowerState := gcblnOn;
    LCDOn;
  end
  else
  begin
    gvblnPowerState := gcblnOff;
    LCDOff;
  end;// if vstrParameters = gcstrOn

end;// procedure PSResponseHandler

//========================================================================================

procedure TogglePowerOnOff;

{var
  Info : TSearchRec; }

begin

  if gvblnPowerState = gcblnOn then
    // Turn the Power Off
  begin
    if gvstrUHFReverseState = gcstrOn then
    begin
      gvstrUHFRXFrequency := gvstrUHFOrigRXFrequency;
      gvstrUHFShift := gvstrUHFOrigShift;
      gvstrUHFTone := gvstrUHFOrigTone;
      gvstrUHFCTCSS := gvstrUHFOrigCTCSS;
      UHFReverseOff;
    end;

    if gvstrVHFReverseState = gcstrOn then
    begin
      gvstrVHFRXFrequency := gvstrVHFOrigRXFrequency;
      gvstrVHFShift := gvstrVHFOrigShift;
      gvstrVHFTone := gvstrVHFOrigTone;
      gvstrVHFCTCSS := gvstrVHFOrigCTCSS;
      VHFReverseOff;
    end;
    SendCommand('PS', gcstrOff);
  end
  else
  begin
    // Turn the Power On
    SendCommand('PS', gcstrOn);// if gvblnPowerState = gcstrOn
    TMV7_Init;
  end;// if gvblnPowerState = gcstrOn

end;// procedure TogglePowerOnOff;

//========================================================================================

end.//unit PSCommand;

