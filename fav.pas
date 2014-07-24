unit Fav;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Fav.pas
//
//  Calls: AppConstants
//         AppVariables
//         BCCommand : SetVHFBand
//                     SetUHFBand
//         BufCommand : SetBuffer
//         LCDDisplay : UpdateLCDDisplay
//         Utilities : DisplayUHFBuffer
//                     DisplayVHFBuffer
//  Called By: Main : TfrmMain.bbtFav01MouseUp
//
//  Ver: 1.0.0
//
//  Date: 9 Dec 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, SysUtils,
  // Application Units
  AppConstants, AppVariables, BCCommand, BufCommand, LCDDisplay, Utilities;

procedure SetFAVChannel ( vbytChannelNr : Byte );

implementation

procedure SetFAVChannel ( vbytChannelNr : Byte );
begin

  // vbytChannelNr is the index into the gvstrFAVChannelDataArray table.
  // First we make sure that we have a valid data record at this position by ensuring
  // the Channel Name contains data (Mandatory field).
  if Length ( gvstrFAVChannelDataArray[ vbytChannelNr, gcbytChannelNameField ] ) <
              gcbytMinChannelNameLength then
  begin
    showmessage('No Entry');
    Exit;
  end;// if Length ( gvstrFAVChannelDataArray

  // Here we have a valid data record so we load the appropriate buffer based on the
  // VFO field
  if gvstrFAVChannelDataArray[ vbytChannelNr, gcbytVFOField ] = gcstrVHF then
  begin
    gvstrVHFDataSource := 'FAV';
    gvstrVHFRXFrequency := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytRXFrequencyField ];
    gvstrVHFStep := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytStepField ];
    gvstrVHFShift := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytShiftField ];
    gvstrVHFReverse := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytReverseField ];
    gvstrVHFTone := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytToneField ];
    gvstrVHFCTCSS := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytCTCSSField ];
    gvstrVHFDTSS := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytDTSSField ];
    gvstrVHFToneNr := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytToneNrField ];
    gvstrVHFDTSSCode := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytDTSSCodeField ];
    gvstrVHFCTCSSNr := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytCTCSSNrField ];
    gvstrVHFOffset := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytShiftOffsetField ];
    gvstrVHFScan := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytScanField ];
    gvstrVHFRFPower := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytRFPowerField ];
    gvstrVHFChannelName := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytChannelNameField ];
    gvstrVHFChannelComments := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytCommentsField ];
    gvstrVHFChannelNr := IntToStr(vbytChannelNr);

//***    DisplayVHFBuffer;

    SetBuffer(gcstrVHFVFO);
    SetVHFBand;

  end
  else
  begin
    gvstrUHFDataSource := 'FAV';
    gvstrUHFRXFrequency := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytRXFrequencyField ];
    gvstrUHFStep := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytStepField ];
    gvstrUHFShift := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytShiftField ];
    gvstrUHFReverse := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytReverseField ];
    gvstrUHFTone := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytToneField ];
    gvstrUHFCTCSS := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytCTCSSField ];
    gvstrUHFDTSS := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytDTSSField ];
    gvstrUHFToneNr := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytToneNrField ];
    gvstrUHFDTSSCode := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytDTSSCodeField ];
    gvstrUHFCTCSSNr := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytCTCSSNrField ];
    gvstrUHFOffset := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytShiftOffsetField ];
    gvstrUHFScan := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytScanField ];
    gvstrUHFRFPower := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytRFPowerField ];
    gvstrUHFChannelName := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytChannelNameField ];
    gvstrUHFChannelComments := gvstrFAVChannelDataArray[ vbytChannelNr, gcbytCommentsField ];
    gvstrUHFChannelNr := IntToStr(vbytChannelNr);

//***    DisplayUHFBuffer;

    SetBuffer(gcstrUHFVFO);
    SetUHFBand;
    UpdateLCDDisplay;

  end;// if gvstrFAVChannelDataArray[ vbytChannelNr, gcbytVFOField ] = gcstrVHFVFO

end;// procedure SetFAVChannel ( vbytChannelNr : Byte );

//========================================================================================

end.// unit Fav;

