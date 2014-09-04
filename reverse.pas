unit Reverse;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Reverse.pas
//
//  This unit provides Reverse functionality for the currently selected channels.
//
//  The default value for Reverse is OFF.
//
//  The Reverse button is a "Toggle". When pressed, state of the currently selected PTT
//  channel is changed.
//
//  Calls: Final : Finalize
//                 LCDDisplay : DisplayUHFReverseStatus
//                      DisplayVHFReverseStatus
//                 Main
//                 PSCommand : TogglePowerOnOff
//
//  Called By: Main : TfrmMain.btnReverseClick
//
//  Ver: 1.0.0
//
//  Date: 9 Dec 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, Graphics, SysUtils,
  // Application  Units
  AppConstants, AppVariables, LCDDisplay;

procedure ToggleReverse;
procedure UHFReverseOff;
procedure VHFReverseOff;

//========================================================================================
implementation

uses
  BufCommand, Main;

procedure UHFReverseOff;
begin
  // Change the Button colours
  frmMain.bbtReverse.Font.Color := clBlack;
  frmMain.bbtReverse.Font.Style := [];
  gvstrUHFReverseState := gcstrOff;
  DisplayUHFRXFrequency;
  DisplayUHFReverseStatus;
  DisplayUHFShiftStatus;
  DisplayUHFCTStatus;
end;// procedure UHFReverse

//========================================================================================
procedure VHFReverseOff;
begin
  // Change the Button colours
  frmMain.bbtReverse.Font.Color := clBlack;
  frmMain.bbtReverse.Font.Style := [];
  gvstrVHFReverseState := gcstrOff;
  DisplayVHFRXFrequency;
  DisplayVHFReverseStatus;
  DisplayVHFShiftStatus;
  DisplayVHFCTStatus;
end;// procedure VHFReverse

//========================================================================================
function RevRXFrequency : string;

var
  vsngTFrequency : single;
  vsngTOffset : single;


begin

  // The Shift offset depends on the PTTBand and the Shift (Plus or Minus.
  // Simplex never gets here.
  if gvstrPTTBand = gcstrVHF then
  begin
    // Save the Original RXFrequency, Shift and Tone
    gvstrVHFOrigRXFrequency := gvstrVHFRXFrequency;
    gvstrVHFOrigShift := gvstrVHFShift;
    gvstrVHFOrigTone := gvstrVHFTone;
    gvstrVHFOrigCTCSS := gvstrVHFCTCSS;
    // First we have to Reverse the Shift
    if gvstrVHFOrigShift = gcstrShiftPlus then
      gvstrVHFShift := gcstrShiftMinus
    else
      gvstrVHFShift := gcstrShiftPlus;
    // Now turn off the Tone
   gvstrVHFTone := gcstrOff;
   gvstrVHFCTCSS := gcstrOff;

    // Now we calculate a Reversed RXFrequency based on the Original Shift
    vsngTFrequency := StrToFloat( gvstrVHFRXFrequency );
    vsngTOffset := StrToFloat(gcstrVHFShiftOffset);
    if gvstrVHFOrigShift = gcstrShiftPlus then
    begin
     vsngTFrequency := vsngTFrequency + 600000;
    end
    else
    begin
      vsngTFrequency := vsngTFrequency - 600000;
    end;// if gvstrVHFShift = gcstrShiftPlus
  end
  else
  begin
    // Save the Original RXFrequency and Shift
    gvstrUHFOrigRXFrequency := gvstrUHFRXFrequency;
    gvstrUHFOrigShift := gvstrUHFShift;
    gvstrUHFOrigTone := gvstrUHFTone;
    gvstrUHFOrigCTCSS := gvstrUHFCTCSS;
    // First we have to Reverse the Shift
    if gvstrUHFShift = gcstrShiftPlus then
      gvstrUHFShift := gcstrShiftMinus
    else
      gvstrUHFShift := gcstrShiftPlus;
    // Now turn off the Tone
    gvstrUHFTone := gcstrOff;
    gvstrUHFCTCSS := gcstrOff;

    // Now we calculate a Reversed RXFrequency based on the Original Shift
    vsngTFrequency := StrToFloat( gvstrUHFRXFrequency );
    vsngTOffset := StrToFloat(gcstrUHFShiftOffsetA);
    if gvstrUHFOrigShift = gcstrShiftPlus then
    begin
     vsngTFrequency := vsngTFrequency + 5000000;
    end
    else
    begin
      vsngTFrequency := vsngTFrequency - 5000000;
    end;// if gvstrVHFShift = gcstrShiftPlus

  end;// if gvstrPTTBand = gcstrVHF

  Result := '00' + FloatToStr(vsngTFrequency);

end;// function RevRXFrequency

//========================================================================================
procedure ToggleReverse;
begin


  // Check to see which Band Channel is selected
  if gvstrPTTBand = gcstrVHF then
  begin

    // Check to see if this is a simplex channel. If it is, there is no point in going
    // to Reverse
    if gvstrVHFShift = gcstrShiftSimplex then
    begin
      ShowMessage( 'Simplex Channel' );
      Exit;
    end;// if gvstrVHFShift = gcstrShiftSimplex

     // Current Band is VHF now we toggle VHFReverse
    if gvstrVHFReverseState = gcstrOff then
    begin
       // Calculate the new RX Frequency using the configured shift and set the Buffer
      gvstrVHFRXFrequency := RevRXFrequency;
      SetBuffer(gcstrVHF);
      // Change the Button colours
      frmMain.bbtReverse.Font.Color := clRed;
      frmMain.bbtReverse.Font.Style := [fsBold];
      gvstrVHFReverseState := gcstrOn;
      DisplayVHFReverseStatus;
      DisplayVHFShiftStatus;
      DisplayVHFCTStatus;
    end
    else
    begin
      // Restore the Original RX Frequency, Shift, Tone and set the Buffer
      gvstrVHFRXFrequency := gvstrVHFOrigRXFrequency;
      gvstrVHFShift := gvstrVHFOrigShift;
      gvstrVHFTone := gvstrVHFOrigTone;
      gvstrVHFCTCSS := gvstrVHFOrigCTCSS;
      SetBuffer(gcstrVHF);
      VHFReverseOff;
    end;// if gvstrUHFReverseState = gcstrOff

  end
  else
  begin

    // Check to see if this is a simplex channel. If it is, there is no point in going
    // to Reverse
    if gvstrUHFShift = gcstrShiftSimplex then
    begin
      ShowMessage( 'Simplex Channel' );
      Exit;
    end;// if gvstrUHFShift = gcstrShiftSimplex

    // Current Band is UHF now we toggle UHFReverse
    if gvstrUHFReverseState = gcstrOff then
    begin
      // Calculate the new RX Frequency using the configured shift and set the Buffer
      gvstrUHFRXFrequency := RevRXFrequency;
      SetBuffer(gcstrUHF);
      // Change the Button colours
      frmMain.bbtReverse.Font.Color := clRed;
      frmMain.bbtReverse.Font.Style := [fsBold];
      gvstrUHFReverseState := gcstrOn;
      DisplayUHFReverseStatus;
      DisplayUHFShiftStatus;
      DisplayUHFCTStatus;
    end
    else
    begin
      // Restore the Original RX Frequency, Shift, Tone and set the Buffer
      gvstrUHFRXFrequency := gvstrUHFOrigRXFrequency;
      gvstrUHFShift := gvstrUHFOrigShift;
      gvstrUHFTone := gvstrUHFOrigTone;
      gvstrUHFCTCSS := gvstrUHFOrigCTCSS;
      SetBuffer(gcstrUHF);
      UHFReverseOff;
    end;// if gvstrUHFReverseState = gcstrOff

  end;// if gvstrPTTBand = gcstrVHF

end;// procedure ToggleReverse

//========================================================================================
end.// Reverse;

