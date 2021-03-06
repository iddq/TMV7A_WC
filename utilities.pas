unit Utilities;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Utilities.pas
//
//  Calls: AppConstants
//         AppTypes
//         AppVariables
//
//  Called By: DataEntry : TfrmDataEntry.bbtSaveClick
//                         TfrmDataEntry.edtRXFrequencyExit
//             DataEntry_FAV : DataEntry_FAV_Init
//             DataEntry_UHFMEM : DataEntry_UHFMEM_Init
//             DataEntry_VHFMEM : DataEntry_VHFMEM_Init
//             Fav : SetFAVChannel
//             LCDDisplay : DisplayUHFCTStatus
//                          DisplayVHFCTStatus
//             Mem_VHF : LoadVHFStringGrid
//             Mem_UHF : LoadUHFStringGrid
//             TMVFiles_FAV : MakeFAVRecord
//             TMVFiles_VHF : ParseVHFRecord
//
//  Ver: 1.0.0
//
//  Date: 20 Aug 2014
//
//========================================================================================

interface

uses
  Classes, Dialogs, SysUtils,
  // Application Units
  AppConstants, AppTypes, AppVariables;

procedure DisplayDataArray(vstrArrayType : TDataRecordType; bytRecNr : Byte);
procedure DisplayUHFBuffer;
procedure DisplayVHFBuffer;
function GetStepIndex(vfltStepSize : Real) : Byte;
function GetStepSize(vbytStepIndex : Byte) : Real;
function GetToneIndexFromToneNr(vbytToneNr : Byte ) : Byte;
function GetToneFrequencyFromToneNr(vbytToneNr : Byte ) : string;
function GetToneNrFromFrequency(vstrFrequency : string) : Byte;
function GetToneNrFromIndex(vbytToneIndex : Byte) : Byte;
function ValidUHFFrequency(vstrUHFFrequency : string) : Boolean;
function ValidVHFFrequency(vstrVHFFrequency : string) : Boolean;

implementation

//========================================================================================
//     STEP ARRAY ROUTINES
//========================================================================================
function GetStepSize(vbytStepIndex : Byte) : Real;
begin

end;// function GetStepSize

//----------------------------------------------------------------------------------------
function GetStepIndex(vfltStepSize : Real) : Byte;

var
  vbytTemp : Byte;

begin

  for vbytTemp := 0 to 9 do
  begin
    if FloatToStr(vfltStepSize) = gvstrStepArray[vbytTemp] then
    begin
      Result := vbytTemp;
      Exit;
    end;// if FloatToStr(vfltStepSize) = gvstrStepArray[vbytTemp]
  end;// for vbytTemp := 0 to 9 do

end;// GetStepIndex

//========================================================================================
//     TONE ARRAY ROUTINES
//========================================================================================
function GetToneIndexFromToneNr(vbytToneNr : Byte ) : Byte;
// This function returns the correct Index into the Tone Combobox string table for the
// Tone Number used by the TMV7 passed as vbytToneNr.
//
//  Tone Nr         Index
//     1              0
//     2 (not used)
//     3..39          1..38
begin

  Case vbytToneNr of
    1 : Result := 0;
   // 2 (not used)
    3..39 : Result := vbytToneNr - 2; // 1 to 37
  end;
end;// function GetToneIndex(vbytToneNr : Byte ) : Byte;

//----------------------------------------------------------------------------------------
function GetToneNrFromIndex(vbytToneIndex : Byte) : Byte;
// This function returns the correct Tone Number used by the TMV7 for the Tone Combobx
// string table Index passed as vbytToneIndex.
//
//  Index            Tone Nr
//    0                1
//                     2 Not Used
//    1..37            3..39
begin

  case vbytToneIndex of
    0 : Result := vbytToneIndex + 1;
    1..37 : Result := vbytToneIndex + 2;
  end;

end;// function GetToneNrFromIndex(vbytToneIndex) : Byte;

//----------------------------------------------------------------------------------------
function GetToneFrequencyFromToneNr(vbytToneNr : Byte ) : string;
begin
  Result := gvstrToneArray[GetToneIndexFromToneNr(vbytToneNr)]
end;// function GetToneFrequency(vbytToneNr)

//----------------------------------------------------------------------------------------
function GetToneNrFromFrequency(vstrFrequency : string) : Byte;

var
  vbytTemp : Byte;

begin

  // vfltFrequency contains the Tone Frequency as a Real. We have to search the Tone Array to
  // determine the array index and then convert that into the correct tone number.
  //
  //  vbytTemp            Tone Nr
  //    0                1
  //                     2 Not Used
  //    1..37            3..39
  for vbytTemp := 0 to gcbytMaxToneIndex do
  begin
    if vstrFrequency = gvstrToneArray[vbytTemp] then
    begin
      if vbytTemp = 0 then
        Result := vbytTemp + 1
      else Result := vbytTemp + 2;
      Exit;
    end;// if vstrTFreq = gvstrToneArray[vbytTemp]
  end;// for vbytTemp := 0 to gcbytMaxToneIndex do

end;// function GetToneNrFromFrequency

//========================================================================================
//     VALIDATION ROUTINES
//========================================================================================
function ValidVHFFrequency(vstrVHFFrequency : string) : Boolean;

var
  vsngFrequency : Single;

begin

  Result := True;

//  if Length(vstrVHFFrequency) <> 7 then
  if Length(vstrVHFFrequency) < 7 then
  begin
    Result := False;
    Exit;
  end;

  vsngFrequency := StrToFloat(vstrVHFFrequency);

  if (vsngFrequency < gcsngMinVHFFrequency) or
     (vsngFrequency > gcsngMaxVHFFrequency) then
       Result := False;

end;// function ValidVHFFrequency : Boolean;

//----------------------------------------------------------------------------------------
function ValidUHFFrequency(vstrUHFFrequency : string) : Boolean;

var
  vsngFrequency : Single;

begin

  Result := True;

//  if Length(vstrUHFFrequency) <> 7 then
  if Length(vstrUHFFrequency) < 7 then
  begin
    Result := False;
    Exit;
  end;

  vsngFrequency := StrToFloat(vstrUHFFrequency);

  if (vsngFrequency < gcsngMinUHFFrequency) or
     (vsngFrequency > gcsngMaxUHFFrequency) then
       Result := False;

end;// function ValidUHFFrequency : Boolean;

//========================================================================================
procedure DisplayDataArray(vstrArrayType : TDataRecordType; bytRecNr : Byte);
begin

  case vstrArrayType of
    drtVHFMEM :
      ShowMessage('VHF MEMORY array - Record - ' + IntToStr(bytRecNr) +
      #13 +
//      'gcbytChannelNrField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytChannelNrField] +
//      #13 +
      'gcbytVFOField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytVFOField] +
      #13 +
      'gcbytRXFrequencyField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytRXFrequencyField] +
      #13 +
      'gcbytStepField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytStepField] +
      #13 +
      'gcbytShiftField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytShiftField] +
      #13 +
      'gcbytReverseField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytReverseField] +
      #13 +
      'gcbytToneField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytToneField] +
      #13 +
      'gcbytCTCSSField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytCTCSSField] +
      #13 +
      'gcbytDTSSField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytDTSSField] +
      #13 +
      'gcbytToneNrField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytToneNrField] +
      #13 +
      'gcbytDTSSCodeField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytDTSSCodeField] +
      #13 +
      'gcbytCTCSSNrField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytCTCSSNrField] +
      #13 +
      'gcbytShiftOffsetField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytShiftOffsetField] +
      #13 +
      'gcbytScanField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytScanField] +
      #13 +
      'gcbytRFPowerField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytRFPowerField] +
      #13 +
      'gcbytChannelNameField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytChannelNameField] +
      #13 +
      'gcbytCommentsField = ' + gvstrVHFChannelDataArray[bytRecNr, gcbytCommentsField]
      );

    drtUHFMEM :
      ShowMessage('UHF MEMORY array - Record - ' + IntToStr(bytRecNr) +
      #13 +
 //     'gcbytChannelNrField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytChannelNrField] +
 //     #13 +
      'gcbytVFOField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytVFOField] +
      #13 +
      'gcbytRXFrequencyField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytRXFrequencyField] +
      #13 +
      'gcbytStepField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytStepField] +
      #13 +
      'gcbytShiftField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytShiftField] +
      #13 +
      'gcbytReverseField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytReverseField] +
      #13 +
      'gcbytToneField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytToneField] +
      #13 +
      'gcbytCTCSSField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytCTCSSField] +
      #13 +
      'gcbytDTSSField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytDTSSField] +
      #13 +
      'gcbytToneNrField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytToneNrField] +
      #13 +
      'gcbytDTSSCodeField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytDTSSCodeField] +
      #13 +
      'gcbytCTCSSNrField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytCTCSSNrField] +
      #13 +
      'gcbytShiftOffsetField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytShiftOffsetField] +
      #13 +
      'gcbytScanField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytScanField] +
      #13 +
      'gcbytRFPowerField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytRFPowerField] +
      #13 +
      'gcbytChannelNameField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytChannelNameField] +
      #13 +
      'gcbytCommentsField = ' + gvstrUHFChannelDataArray[bytRecNr, gcbytCommentsField]
      );

    drtFAV :
      ShowMessage('FAV MEMORY array - Record - ' + IntToStr(bytRecNr) +
      #13 +
//      'gcbytChannelNrField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytChannelNrField] +
//      #13 +
      'gvstrVFOField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytVFOField] +
      #13 +
      'gcbytRXFrequencyField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytRXFrequencyField] +
      #13 +
      'gcbytStepField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytStepField] +
      #13 +
      'gcbytShiftField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytShiftField] +
      #13 +
      'gcbytReverseField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytReverseField] +
      #13 +
      'gcbytToneField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytToneField] +
      #13 +
      'gcbytCTCSSField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytCTCSSField] +
      #13 +
      'gcbytDTSSField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytDTSSField] +
      #13 +
      'gcbytToneNrField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytToneNrField] +
      #13 +
      'gcbytDTSSCodeField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytDTSSCodeField] +
      #13 +
      'gcbytCTCSSNrField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytCTCSSNrField] +
      #13 +
      'gcbytShiftOffsetField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytShiftOffsetField] +
      #13 +
      'gcbytScanField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytScanField] +
      #13 +
      'gcbytRFPowerField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytRFPowerField] +
      #13 +
      'gcbytChannelNameField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytChannelNameField] +
      #13 +
      'gcbytCommentsField = ' + gvstrFAVChannelDataArray[bytRecNr, gcbytCommentsField]
       );
    drtDTMF :
      ShowMessage('DTMF array - Record - ' + IntToStr(bytRecNr)
      );
  end;// case vstrArrayType of

end;// procedure DisplayDataArray

//----------------------------------------------------------------------------------------

procedure DisplayUHFBuffer;
begin

  ShowMessage('UHF Buffer' +
               #13 +
               'gvstrUHFDataSource  = ' + gvstrUHFDataSource +
               #13 +
//               'gvstrUHFChannelNr = ' + gvstrUHFChannelNr +
//               #13 +
               'gvstrUHFRXFrequency = ' + gvstrUHFRXFrequency +
               #13 +
               'gvstrUHFStep = ' + gvstrUHFStep +
               #13 +
               'gvstrUHFShift = ' + gvstrUHFShift +
               #13 +
               'gvstrUHFReverse = ' + gvstrUHFReverse +
               #13 +
               'gvstrUHFTone = ' + gvstrUHFTone +
               #13 +
               'gvstrUHFCTCSS = ' + gvstrUHFCTCSS +
               #13 +
               'gvstrUHFDTSS = ' + gvstrUHFDTSS +
               #13 +
               'gvstrUHFToneNr = ' + gvstrUHFToneNr +
               #13 +
               'gvstrUHFDTSSCode = ' + gvstrUHFDTSSCode +
               #13 +
               'gvstrUHFCTCSSNr = ' + gvstrUHFCTCSSNr +
               #13 +
               'gvstrUHFOffset = ' + gvstrUHFOffset +
               #13 +
               'gvstrUHFScan = ' + gvstrUHFScan +
               #13 +
               'gvstrUHFSplitFrequency = ' + gvstrUHFSplitFrequency +
               #13 +
               'gvstrUHFSplitStep = ' + gvstrUHFSplitStep +
               #13 +
               'gvstrUHFRFPower = ' + gvstrUHFRFPower +
               #13 +
               'gvstrUHFChannelName = ' + gvstrUHFChannelName +
               #13 +
               'gvstrUHFChannelComments = ' + gvstrUHFChannelComments +
               #13 +
               'gvstrUHFAudioLevel = ' + gvstrUHFAudioLevel +
               #13 +
               'gvstrUHFSquelchLevel = ' + gvstrUHFSquelchLevel
               );

end;// procedure DisplayUHFBuffer;

//----------------------------------------------------------------------------------------
procedure DisplayVHFBuffer;
begin

    ShowMessage('VHF Buffer' +
               #13 +
               'gvstrVHFDataSource = ' + gvstrVHFDataSource +
               #13 +
   //            'gvstrVHFChannelNr = ' + gvstrVHFChannelNr +
   //            #13 +
               'gvstrVHFRXFrequency = ' + gvstrVHFRXFrequency +
               #13 +
               'gvstrVHFStep = ' + gvstrVHFStep +
               #13 +
               'gvstrVHFShift = ' + gvstrVHFShift +
               #13 +
               'gvstrVHFReverse = ' + gvstrVHFReverse +
               #13 +
               'gvstrVHFTone = ' + gvstrVHFTone +
               #13 +
               'gvstrVHFCTCSS = ' + gvstrVHFCTCSS +
               #13 +
               'gvstrVHFDTSS = ' + gvstrVHFDTSS +
               #13 +
               'gvstrVHFToneNr = ' + gvstrVHFToneNr +
               #13 +
               'gvstrVHFDTSSCode = ' + gvstrVHFDTSSCode +
               #13 +
               'gvstrVHFCTCSSNr = ' + gvstrVHFCTCSSNr +
               #13 +
               'gvstrVHFOffset = ' + gvstrVHFOffset +
               #13 +
               'gvstrVHFScan = ' + gvstrVHFScan +
               #13 +
               'gvstrVHFSplitFrequency = ' + gvstrVHFSplitFrequency +
               #13 +
               'gvstrVHFSplitStep = ' + gvstrVHFSplitStep  +
               #13 +
               'gvstrVHFRFPower = ' + gvstrUHFRFPower +
               #13 +
               'gvstrVHFChannelName = ' + gvstrVHFChannelName +
               #13 +
               'gvstrVHFChannelComments = ' + gvstrVHFChannelComments +
               #13 +
               'gvstrVHFAudioLevel = ' + gvstrVHFAudioLevel +
               #13 +
               'gvstrVHFSquelchLevel = ' + gvstrVHFSquelchLevel
               );

end;// procedure DisplayVHFBuffer;

//========================================================================================
end.// unit Utilities;

