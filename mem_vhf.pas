unit MEM_VHF;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Mem_VHF.pas
//
//  Calls: AppConstants
//         AppVariables
//         BCCommand : SetVHFBand
//         BUFCommand : SetBuffer
//         LCDDisplay : UpdateLCDDisplay
//         Mem
//         Utilities : GetToneFrequencyFromToneNr
//
//  Called By: MEM : TfrmMEM.Setup
//                   SetVHFChannel
//
//  Ver: 1.0.0
//
//  Date: 11 Aug 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, SysUtils,
  // Application Units
  AppConstants, AppVariables, BCCommand, BUFCommand, LCDDisplay, Utilities;

procedure LoadVHFStringGrid;
procedure SetVHFChannel;

implementation

uses
  Mem;

//========================================================================================
procedure LoadVHFStringGrid;

var
  vbytTemp : Byte;
  vstrTStr : String;

begin

  for vbytTemp := 1 to gcbytMaxVHFChannels do
  begin

    // Channel Nr
    frmMem.sgrVHF.Cells[gcbytChMemNrCol, vbytTemp] := IntToStr(vbytTemp);

    // Channel Name
    frmMem.sgrVHF.Cells[gcbytNameCol, vbytTemp] :=
                    gvstrVHFChannelDataArray[vbytTemp,gcbytChannelNameField];

    // RX FREQUENCY
    if Length (gvstrVHFChannelDataArray[vbytTemp,gcbytRXFrequencyField]) > 0 then
      frmMem.sgrVHF.Cells[gcbytRXFreqCol, vbytTemp] :=
         Copy(gvstrVHFChannelDataArray[vbytTemp,gcbytRXFrequencyField],3,3) +
         '.' +
         Copy(gvstrVHFChannelDataArray[vbytTemp,gcbytRXFrequencyField],6,3)
    else
      frmMem.sgrVHF.Cells[gcbytRXFreqCol, vbytTemp] := '';

    // SHIFT
    vstrTStr := gvstrVHFChannelDataArray[vbytTemp,gcbytShiftCol+1];
    case vstrTStr of
      gcstrShiftSimplex : frmMem.sgrVHF.Cells[gcbytShiftCol, vbytTemp] := gcstrTMV7ShiftSimplex;
      gcstrShiftPlus : frmMem.sgrVHF.Cells[gcbytShiftCol, vbytTemp] := gcstrTMV7ShiftPlus;
      gcstrShiftMinus : frmMem.sgrVHF.Cells[gcbytShiftCol, vbytTemp] := gcstrTMV7ShiftMinus;
    end;// case vstrTStr

    // Offset
    if Length(gvstrVHFChannelDataArray[vbytTemp,gcbytShiftOffsetField]) > 0 then
      if gvstrVHFChannelDataArray[vbytTemp,gcbytShiftCol+1] = gcstrShiftSimplex then
        frmMem.sgrVHF.Cells[gcbytOffsetCol, vbytTemp] := ''
      else
        frmMem.sgrVHF.Cells[gcbytOffsetCol, vbytTemp] :=
              Copy(gvstrVHFChannelDataArray[vbytTemp,gcbytShiftOffsetField],2,2) +
              '.' +
              Copy(gvstrVHFChannelDataArray[vbytTemp,gcbytShiftOffsetField],4,2)
    else
      frmMem.sgrVHF.Cells[gcbytOffsetCol, vbytTemp] := '';

    // Tone or CTCSS
    // We only load this field if there is a valid record
    if Length (gvstrVHFChannelDataArray[vbytTemp,gcbytChannelNameField]) > 0 then
    begin
      if gvstrVHFChannelDataArray[vbytTemp,gcbytToneField] = gcstrOn then
        frmMem.sgrVHF.Cells[gcbytToneCTCSSCol, vbytTemp] := gcstrTMV7Tone
      else if gvstrVHFChannelDataArray[vbytTemp,gcbytCTCSSField] = gcstrOn then
        frmMem.sgrVHF.Cells[gcbytToneCTCSSCol, vbytTemp] := gcstrTMV7CTCSS
      else frmMem.sgrVHF.Cells[gcbytToneCTCSSCol, vbytTemp] := gcstrTMV7None;
    end;// if Length (gvstrVHFChannelDataArray[vbytTemp,gcbytChannelNameField]) > 0

    // Tone Freq
    // We only load the tone Frequency if the record is valid and a tone is selected
   if Length (gvstrVHFChannelDataArray[vbytTemp,gcbytChannelNameField]) > 0 then
   begin
     // We have a valid record we now check to see if there is a Tone or CTCSS on
     case frmMem.sgrVHF.Cells[gcbytToneCTCSSCol, vbytTemp] of
       gcstrTMV7Tone : begin
                         frmMem.sgrVHF.Cells[gcbytToneCTCSSFreqCol, vbytTemp] :=
                           GetToneFrequencyFromToneNr
                          (StrToInt (gvstrVHFChannelDataArray[vbytTemp,gcbytToneNrField]));
                       end;// gcstrTMV7Tone
       gcstrTMV7CTCSS : begin
                          frmMem.sgrVHF.Cells[gcbytToneCTCSSFreqCol, vbytTemp] :=
                            GetToneFrequencyFromToneNr
                            (StrToInt (gvstrVHFChannelDataArray[vbytTemp,gcbytCTCSSNrField]));
                       end;// gcstrTMV7CTCSS
     else // gcstrTMV7None
       frmMem.sgrVHF.Cells[gcbytToneCTCSSFreqCol, vbytTemp] := '';
     end;// case frmMem.sgrVHF.Cells[cbytToneCTCSSCol, vbytTemp]

   end;// Length (gvstrVHFChannelDataArray[vbytTemp,gcbytChannelNameField]) > 0

        // RF Power
    vstrTStr := gvstrVHFChannelDataArray[vbytTemp,gcbytRFPowerField];
    case vstrTStr of
      gcstrRFPowerLow : frmMem.sgrVHF.Cells[gcbytRFPowerCol, vbytTemp] := gcstrTMV7RFPowerLow;
      gcstrRFPowerMedium : frmMem.sgrVHF.Cells[gcbytRFPowerCol, vbytTemp] := gcstrTMV7RFPowerMedium;
      gcstrRFPowerHigh : frmMem.sgrVHF.Cells[gcbytRFPowerCol, vbytTemp] := gcstrTMV7RFPowerHigh;
    end;//  case vstrTStr of

    // DTSS
    vstrTStr := gvstrVHFChannelDataArray[vbytTemp,gcbytDTSSField];
    case vstrTStr of
      gcstrOn : frmMem.sgrVHF.Cells[gcbytDTSSCol, vbytTemp] := gcstrTMV7On;
      gcstrOff : frmMem.sgrVHF.Cells[gcbytDTSSCol, vbytTemp] := gcstrTMV7Off;
    else
      frmMem.sgrVHF.Cells[gcbytRFPowerCol, vbytTemp] := '';
    end;//  case vstrTStr of

    // DTSS CODE
   if Length (gvstrVHFChannelDataArray[vbytTemp,gcbytChannelNameField]) > 0 then
   begin
      if gvstrVHFChannelDataArray[vbytTemp,gcbytDTSSField] = gcstrOn then
        frmMem.sgrVHF.Cells[gcbytDTSSCodeCol, vbytTemp] :=
                    gvstrVHFChannelDataArray[vbytTemp,gcbytDTSSCodeField]
      else
        frmMem.sgrVHF.Cells[gcbytDTSSCodeCol, vbytTemp] := '';
   end;// if Length (gvstrVHFChannelDataArray[vbytTemp,gcbytChannelNameField]) > 0

    // REVERSE
    vstrTStr := gvstrVHFChannelDataArray[vbytTemp,gcbytReverseField];
    case vstrTStr of
      gcstrOn : frmMem.sgrVHF.Cells[gcbytReverseCol, vbytTemp] := gcstrTMV7On;
      gcstrOff : frmMem.sgrVHF.Cells[gcbytReverseCol, vbytTemp] := gcstrTMV7Off;
    else
      frmMem.sgrVHF.Cells[gcbytReverseCol, vbytTemp] := '';
    end;//  case vstrTStr of

    // SCAN
    vstrTStr := gvstrVHFChannelDataArray[vbytTemp,gcbytScanField];
    case vstrTStr of
      gcstrOn : frmMem.sgrVHF.Cells[gcbytScanCol, vbytTemp] := gcstrTMV7On;
      gcstrOff : frmMem.sgrVHF.Cells[gcbytScanCol, vbytTemp] := gcstrTMV7Off;
    else
      frmMem.sgrVHF.Cells[gcbytScanCol, vbytTemp] := '';
    end;//  case vstrTStr of

    // Step
    if Length (gvstrVHFChannelDataArray[vbytTemp,gcbytStepField]) > 0 then
      vstrTStr := gvstrVHFChannelDataArray[vbytTemp,gcbytStepField]
    else vstrTStr := '';

    if Length(vstrTStr) > 0 then
      frmMem.sgrVHF.Cells[gcbytStepCol, vbytTemp] := gvstrStepArray[StrToInt(vstrTStr)]
    else
      frmMem.sgrVHF.Cells[gcbytStepCol, vbytTemp] := '';

    // COMMENTS
    frmMem.sgrVHF.Cells[gcbytCommentCol, vbytTemp] :=
                    gvstrVHFChannelDataArray[vbytTemp,gcbytCommentsField];

  end;// for vbytTemp := 1 to gcbytMaxVHFChannels do

end;// procedure TfrmMEM.LoadVHFStringGrid;

//----------------------------------------------------------------------------------------
procedure SetVHFChannel;
begin

  // vbytChannelNr is the index into the gvstrFAVChannelDataArray table.
  // First we make sure that we have a valid data record at this position by ensuring
  // the Channel Name contains data (Mandatory field).
  if gvintSelectedRow = 0 then gvintSelectedRow := 1;

  if Length ( gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytChannelNameField ] ) <
              gcbytMinChannelNameLength then
  begin
    showmessage('No Entry');
    Exit;
  end;// if Length ( gvstrFAVChannelDataArray

  // Here we have a valid data record so we load the appropriate buffer based on the
  // VFO field
  gvstrVHFDataSource := 'MEM';
  gvstrVHFRXFrequency := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytRXFrequencyField ];
  gvstrVHFStep := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytStepField ];
  gvstrVHFShift := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytShiftField ];
  gvstrVHFReverse := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytReverseField ];
  gvstrVHFTone := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytToneField ];
  gvstrVHFCTCSS := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytCTCSSField ];
  gvstrVHFDTSS := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytDTSSField ];
  gvstrVHFToneNr := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytToneNrField ];
  gvstrVHFDTSSCode := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytDTSSCodeField ];
  gvstrVHFCTCSSNr := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytCTCSSNrField ];
  gvstrVHFOffset := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytShiftOffsetField ];
  gvstrVHFScan := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytScanField ];
  gvstrVHFRFPower := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytRFPowerField ];
  gvstrVHFChannelName := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytChannelNameField ];
  gvstrVHFChannelComments := gvstrVHFChannelDataArray[ gvintSelectedRow, gcbytCommentsField ];

  //***      DisplayVHFBuffer;

  SetBuffer(gcstrVHFVFO);
  SetVHFBand;
  UpdateLCDDisplay;

end;// procedure SetVHFChannel;

//========================================================================================
end.// unit MEM_VHF;

