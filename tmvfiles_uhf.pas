unit TMVFiles_UHF;

{$mode objfpc}{$H+}

//========================================================================================
//
//  TMVFiles_UHF.pas
//
//  Calls: AppConstants
//         AppVariables
//         Utilities : GetStepIndex
//
//  Called By: TMVFiles : OpenTMVFile
//
//  Ver: 1.0.0
//
//  Date: 9 Aug 2013
//
//========================================================================================

interface

uses
  Classes, SysUtils,
  // Application Units
  AppConstants, AppVariables, Utilities;

function MakeUHFRecord(vbytRecord : Byte) : string;
procedure ParseUHFRecord(vbytRecNr : Byte; vstrRecord : string);

implementation

//========================================================================================
function MakeUHFRecord(vbytRecord : Byte) : string;

var
  vstrTRecord : string;

begin

  // Record Nr
  vstrTRecord := IntToStr(vbytRecord) + ',';

  // VFO
  if gvstrUHFChannelDataArray[vbytRecord, gcbytVFOField] = gcstrUHF then
    vstrTRecord := vstrTRecord + gcstrTMV7VFO_UHF + ','
  else if gvstrUHFChannelDataArray[vbytRecord, gcbytVFOField] = gcstrVHF then
    vstrTRecord := vstrTRecord + gcstrTMV7VFO_VHF + ','
  else
    vstrTRecord := vstrTRecord + '' + ',';

  // RX Frequency
  if Length(gvstrUHFChannelDataArray[vbytRecord, gcbytRXFrequencyField]) > 0 then
    vstrTRecord := vstrTRecord +
                   Copy(gvstrUHFChannelDataArray[vbytRecord, gcbytRXFrequencyField], 3, 3) +
                   '.' +
                   Copy(gvstrUHFChannelDataArray[vbytRecord, gcbytRXFrequencyField], 6, 3) +
                   ','
  else
    vstrTRecord := vstrTRecord + '' + ',';

  // Step
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytStepField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    vstrTRecord := vstrTRecord +
                 gvstrStepArray[StrToInt(gvstrUHFChannelDataArray[vbytRecord,
                 gcbytStepField])] + ',';

  // Shift
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytShiftField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    case gvstrUHFChannelDataArray[vbytRecord, gcbytShiftField] of
      gcstrShiftSimplex :
        vstrTRecord := vstrTRecord + gcstrTMV7ShiftSimplex + ',';
      gcstrShiftPlus :
        vstrTRecord := vstrTRecord + gcstrTMV7ShiftPlus + ',';
      gcstrShiftMinus :
        vstrTRecord := vstrTRecord + gcstrTMV7ShiftMinus + ',';
    end;// case gvstrUHFChannelDataArray[vbytRecord, gcbytShiftField]

  // Reverse
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytReverseField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    if gvstrUHFChannelDataArray[vbytRecord, gcbytReverseField] = gcstrOff then
        vstrTRecord := vstrTRecord + gcstrTMV7Off + ','
    else
      vstrTRecord := vstrTRecord + gcstrTMV7On + ',';

  //  TONE - CTCSS
  // This is sort of comlicated. We have three radio boxes which give us the correct
  // status of the TMV7 Tone functions as well as a list of tones in the combo box
  //
  // First we determine the Tone Function Status
  If (Length(gvstrUHFChannelDataArray[vbytRecord, gcbytToneField]) = 0) and
     (Length(gvstrUHFChannelDataArray[vbytRecord, gcbytCTCSSField]) = 0)
  then
  begin
    // There is no Tone Function selected so we null out the Status Field
    vstrTRecord := vstrTRecord + '' + ',';
    // and the Tone Frequency field
    vstrTRecord := vstrTRecord + '' + ',';
  end
  else
  begin
    // We have a Tone Function selected so we determine both the Function as well
    // as the Tone Freq
    if  ((gvstrUHFChannelDataArray[vbytRecord, gcbytToneField]) = gcstrOff) and
        ((gvstrUHFChannelDataArray[vbytRecord, gcbytCTCSSField]) = gcstrOff) then
    begin
      // Both Tone Function and Frequency are turned off
      vstrTRecord := vstrTRecord + gcstrTMV7None + ',';
      vstrTRecord := vstrTRecord + '' + ',';
    end
    else if ((gvstrUHFChannelDataArray[vbytRecord, gcbytToneField]) = gcstrOn) then
    begin
      // The Tone Function and Frequency are turned on
      vstrTRecord := vstrTRecord + gcstrTMV7Tone + ',';
      vstrTRecord := vstrTRecord +
           GetToneFrequencyFromToneNr(StrToInt(gvstrUHFChannelDataArray[vbytRecord,
           gcbytToneNrField]))+ ',';
    end
    else
    begin
      // The CTCSS Function and Frequency are tuirned on
      vstrTRecord := vstrTRecord + gcstrTMV7CTCSS + ',';
      vstrTRecord := vstrTRecord +
           GetToneFrequencyFromToneNr(StrToInt(gvstrUHFChannelDataArray[vbytRecord,
           gcbytCTCSSNrField])) + ',';
    end;// if  ((gvstrUHFChannelDataArray[vbytRecord, gcbytToneField])

  end;// If (Length(gvstrVHFChannelDataArray[vbytRecord, gcbytToneField]) = 0)

  // DTSS Function and Code
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytDTSSField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ',' + '' + ','
  else
    if gvstrUHFChannelDataArray[vbytRecord, gcbytDTSSField] = gcstrOff then
    begin
        vstrTRecord := vstrTRecord + gcstrTMV7Off + ',';
        vstrTRecord := vstrTRecord + '' + ',';
    end
    else
    begin
      vstrTRecord := vstrTRecord + gcstrTMV7On + ',';
      vstrTRecord := vstrTRecord + gvstrUHFChannelDataArray[vbytRecord,
      gcbytDTSSCodeField] + ','
    end;// if gvstrUHFChannelDataArray[vbytRecord, gcbytDTSSField]

  // Shift Offset
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytShiftOffsetField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    vstrTRecord := vstrTRecord +
    Copy(gvstrUHFChannelDataArray[vbytRecord, gcbytShiftOffsetField], 2, 2) +
    '.' +
    Copy(gvstrUHFChannelDataArray[vbytRecord, gcbytShiftOffsetField], 4, 2) + ',';

  // Scan
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytScanField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    if gvstrUHFChannelDataArray[vbytRecord, gcbytScanField] = gcstrOff then
        vstrTRecord := vstrTRecord + gcstrTMV7Off + ','
    else
      vstrTRecord := vstrTRecord + gcstrTMV7On + ',';

  // RF Power
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytRFPowerField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    case gvstrUHFChannelDataArray[vbytRecord, gcbytRFPowerField] of
      gcstrRFPowerLow :
        vstrTRecord := vstrTRecord + gcstrTMV7RFPowerLow + ',';
      gcstrRFPowerMedium :
        vstrTRecord := vstrTRecord + gcstrTMV7RFPowerMedium + ',';
      gcstrRFPowerHigh :
        vstrTRecord := vstrTRecord + gcstrTMV7RFPowerHigh + ',';
    end;// case gvstrUHFChannelDataArray[vbytRecord, gcbytRFPowerField]

  // Channel Name
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytChannelNameField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    vstrTRecord := vstrTRecord + gvstrUHFChannelDataArray[vbytRecord,
                   gcbytChannelNameField] + ',';

  // Comments
  If Length(gvstrUHFChannelDataArray[vbytRecord, gcbytCommentsField]) = 0 then
    vstrTRecord := vstrTRecord + ''
  else
    vstrTRecord := vstrTRecord + gvstrUHFChannelDataArray[vbytRecord, gcbytCommentsField];

  Result := vstrTRecord;

end;// function MakeUHFRecord

//========================================================================================
procedure ParseUHFRecord(vbytRecNr : Byte; vstrRecord : string);

var
  vbytCommaPos : Byte;
  vstrTStr : string;
  vbytTbyt : Byte;
  vstrTToneNr : string;

begin

  // Bypass the Record Nr
  vbytCommaPos := Pos(',', vstrRecord );
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // VFO
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case  vstrTstr of
    '' :     gvstrUHFChannelDataArray[vbytRecNr, gcbytVFOField] := '';
    gcstrTMV7VFO_UHF : gvstrUHFChannelDataArray[vbytRecNr, gcbytVFOField] := gcstrUHFVFO;
    else
      gvstrUHFChannelDataArray[vbytRecNr, gcbytVFOField] :=gcstrVHFVFO;
  end;// case of vstrTstr
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // RX Frequency
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  if Length(vstrTStr) > 0 then
    vstrTStr := '00' + Copy(vstrRecord, 1, 3) + Copy(vstrRecord, 5, 3) + '000';
  gvstrUHFChannelDataArray[vbytRecNr, gcbytRXFrequencyField] := vstrTStr;
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Step Size
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  if Length(vstrTStr) > 0 then
  begin
    // We look for a decimal to determine if we have to conver to Real
    if Pos('.', vstrTStr) > 0 then
      vbytTByt := GetStepIndex(StrToFloat(vstrTStr))
    else
      vbytTByt := GetStepIndex(StrToFloat(vstrTStr + '.0'));
    gvstrUHFChannelDataArray[vbytRecNr, gcbytStepField] := IntToStr(vbytTByt);
  end
  else
    gvstrUHFChannelDataArray[vbytRecNr, gcbytStepField] := '';
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Shift
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrUHFChannelDataArray[vbytRecNr, gcbytShiftField] := '';
    gcstrTMV7ShiftSimplex :
      gvstrUHFChannelDataArray[vbytRecNr, gcbytShiftField] := gcstrShiftSimplex;
    gcstrTMV7ShiftPlus :
      gvstrUHFChannelDataArray[vbytRecNr, gcbytShiftField] := gcstrShiftPlus;
    gcstrTMV7ShiftMinus :
      gvstrUHFChannelDataArray[vbytRecNr, gcbytShiftField] := gcstrShiftMinus;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Reverse
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrUHFChannelDataArray[vbytRecNr, gcbytReverseField] := '';
    gcstrTMV7Off : gvstrUHFChannelDataArray[vbytRecNr, gcbytReverseField] := gcstrOff;
    else
      gvstrUHFChannelDataArray[vbytRecNr, gcbytReverseField] := gcstrOn;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Tone Function - This takes care of both Tone and CTCSS On/Off fields
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : begin
           gvstrUHFChannelDataArray[vbytRecNr, gcbytToneField] := '';
           gvstrUHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := '';
         end;// '', cstrNone
    gcstrTMV7None : begin
                 gvstrUHFChannelDataArray[vbytRecNr, gcbytToneField] := gcstrOff;
                 gvstrUHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := gcstrOff;
               end;// '', cstrNone
    gcstrTMV7Tone : begin
                 gvstrUHFChannelDataArray[vbytRecNr, gcbytToneField] := gcstrOn;
                 gvstrUHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := gcstrOff;
               end;// cstrTone
    else
      begin
        gvstrUHFChannelDataArray[vbytRecNr, gcbytToneField] := gcstrOff;
        gvstrUHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := gcstrOn;
      end;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Tone Frequency - This takes care of both Tone and CTCSS Frequency fields
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  if Length(vstrTStr) > 0 then
  begin
    // There is a Frequency in the record. That mneans that either Tone or CTCSS have
    // been selected. vstrTStr contains the Tone Frequency as a string
    if gvstrUHFChannelDataArray[vbytRecNr, gcbytToneField] = gcstrOn then
    begin
      // Tone has been selected so we have to populate the Tone Nr field and Default the
      // CTCSS Nr field
      vbytTByt := GetToneNrFromFrequency(vstrTStr);
      vstrTToneNr := IntToStr(vbytTByt);
      if Length(vstrTToneNr) = 1 then
        vstrTToneNr := '0' + vstrTToneNr;
      gvstrUHFChannelDataArray[vbytRecNr, gcbytToneNrField] := vstrTToneNr;
      gvstrUHFChannelDataArray[vbytRecNr, gcbytCTCSSNrField] := '01';
    end
    else
    begin
      // CTCSS has been selected so we have to populate the CTCSS Nr field and Default the
      // Tone Nr field
      vbytTByt := GetToneNrFromFrequency(vstrTStr);
      vstrTToneNr := IntToStr(vbytTByt);
      if Length(vstrTToneNr) = 1 then
        vstrTToneNr := '0' + vstrTToneNr;
      gvstrUHFChannelDataArray[vbytRecNr, gcbytCTCSSNrField] := vstrTToneNr;
      gvstrUHFChannelDataArray[vbytRecNr, gcbytToneNrField] := '01';
    end;//éé if gvstrFAVChannelDataArray[vbytRecNr, gcbytToneField] = gcstrOn
  end
  else
  begin
    // There is no Tone Frequency in the record so we clear the Tone Nr fields
    gvstrUHFChannelDataArray[vbytRecNr, gcbytToneNrField] := '01';
    gvstrUHFChannelDataArray[vbytRecNr, gcbytCTCSSNrField] := '01';
  end;//  if Length(vstrTStr) > 0
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // DTSS On/Off
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrUHFChannelDataArray[vbytRecNr, gcbytDTSSField] := '';
    gcstrTMV7Off : gvstrUHFChannelDataArray[vbytRecNr, gcbytDTSSField] :=
                     gcstrOff;
    else
      gvstrUHFChannelDataArray[vbytRecNr, gcbytDTSSField] := gcstrOn;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // DTSS Code
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrUHFChannelDataArray[vbytRecNr, gcbytDTSSCodeField] := '000';
    else
     gvstrUHFChannelDataArray[vbytRecNr, gcbytDTSSCodeField] := vstrTStr;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Shift Offset
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrUHFChannelDataArray[vbytRecNr, gcbytShiftOffsetField] := '';
    else
      gvstrUHFChannelDataArray[vbytRecNr, gcbytShiftOffsetField] := '0' +
          Copy(vstrTStr, 1, 2) + Copy(vstrTStr, 4, 2) + '0000';
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Scan On/Off
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrUHFChannelDataArray[vbytRecNr, gcbytScanField] := '';
    gcstrTMV7Off : gvstrUHFChannelDataArray[vbytRecNr, gcbytScanField] := gcstrOff;
    else
      gvstrUHFChannelDataArray[vbytRecNr, gcbytScanField] := gcstrOn;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // RF Power
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrUHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := '';
    gcstrTMV7RFPowerLow :
      gvstrUHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := gcstrRFPowerLow;
    gcstrTMV7RFPowerMedium :
      gvstrUHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := gcstrRFPowerMedium;
    gcstrTMV7RFPowerHigh :
      gvstrUHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := gcstrRFPowerHigh;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Channel Name
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  gvstrUHFChannelDataArray[vbytRecNr, gcbytChannelNameField] := vstrTStr;
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Comments
  gvstrUHFChannelDataArray[vbytRecNr, gcbytCommentsField] := vstrRecord;

end;// procedure ParseUHFRecord;

//========================================================================================
end.// unit TMVFiles_UHF;

