unit TMVFiles_VHF;

{$mode objfpc}{$H+}

//========================================================================================
//
//  TMVFiles_VHF.pas
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

procedure ParseVHFRecord(vbytRecNr : Byte; vstrRecord : string);
function MakeVHFRecord(vbytRecord : Byte) : string;

implementation

//========================================================================================
function MakeVHFRecord(vbytRecord : Byte) : string;

var
  vstrTRecord : string;

begin

  // Record Nr
  vstrTRecord := IntToStr(vbytRecord) + ',';

  // VFO
  if gvstrVHFChannelDataArray[vbytRecord, gcbytVFOField] = gcstrVHF then
    vstrTRecord := vstrTRecord + gcstrTMV7VFO_VHF + ','
  else if gvstrVHFChannelDataArray[vbytRecord, gcbytVFOField] = gcstrUHF then
    vstrTRecord := vstrTRecord + gcstrTMV7VFO_UHF + ','
  else
    vstrTRecord := vstrTRecord + '' + ',';

  // RX Frequency
  if Length(gvstrVHFChannelDataArray[vbytRecord, gcbytRXFrequencyField]) > 0 then
    vstrTRecord := vstrTRecord +
                   Copy(gvstrVHFChannelDataArray[vbytRecord, gcbytRXFrequencyField], 3, 3) +
                   '.' +
                   Copy(gvstrVHFChannelDataArray[vbytRecord, gcbytRXFrequencyField], 6, 3) +
                   ','
  else
    vstrTRecord := vstrTRecord + '' + ',';

  // Step
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytStepField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    vstrTRecord := vstrTRecord +
                 gvstrStepArray[StrToInt(gvstrVHFChannelDataArray[vbytRecord,
                 gcbytStepField])] + ',';

  // Shift
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytShiftField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    case gvstrVHFChannelDataArray[vbytRecord, gcbytShiftField] of
      gcstrShiftSimplex :
        vstrTRecord := vstrTRecord + gcstrTMV7ShiftSimplex + ',';
      gcstrShiftPlus :
        vstrTRecord := vstrTRecord + gcstrTMV7ShiftPlus + ',';
      gcstrShiftMinus :
        vstrTRecord := vstrTRecord + gcstrTMV7ShiftMinus + ',';
    end;// case gvstrVHFChannelDataArray[vbytRecord, gcbytShiftField]

  // Reverse
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytReverseField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    if gvstrVHFChannelDataArray[vbytRecord, gcbytReverseField] = gcstrOff then
        vstrTRecord := vstrTRecord + gcstrTMV7Off + ','
    else
      vstrTRecord := vstrTRecord + gcstrTMV7On + ',';

  //  TONE - CTCSS
  // This is sort of comlicated. We have three radio boxes which give us the correct
  // status of the TMV7 Tone functions as well as a list of tones in the combo box
  //
  // First we determine the Tone Function Status
  If (Length(gvstrVHFChannelDataArray[vbytRecord, gcbytToneField]) = 0) and
     (Length(gvstrVHFChannelDataArray[vbytRecord, gcbytCTCSSField]) = 0)
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
    if  ((gvstrVHFChannelDataArray[vbytRecord, gcbytToneField]) = gcstrOff) and
        ((gvstrVHFChannelDataArray[vbytRecord, gcbytCTCSSField]) = gcstrOff) then
    begin
      // Both Tone Function and Frequency are turned off
      vstrTRecord := vstrTRecord + gcstrTMV7None + ',';
      vstrTRecord := vstrTRecord + '' + ',';
    end
    else if ((gvstrVHFChannelDataArray[vbytRecord, gcbytToneField]) = gcstrOn) then
    begin
      // The Tone Function and Frequency are turned on
      vstrTRecord := vstrTRecord + gcstrTMV7Tone + ',';
      vstrTRecord := vstrTRecord +
           GetToneFrequencyFromToneNr(StrToInt(gvstrVHFChannelDataArray[vbytRecord,
           gcbytToneNrField]))+ ',';
    end
    else
    begin
      // The CTCSS Function and Frequency are tuirned on
      vstrTRecord := vstrTRecord + gcstrTMV7CTCSS + ',';
      vstrTRecord := vstrTRecord +
           GetToneFrequencyFromToneNr(StrToInt(gvstrVHFChannelDataArray[vbytRecord,
           gcbytCTCSSNrField])) + ',';
    end;// if  ((gvstrVHFChannelDataArray[vbytRecord, gcbytToneField])

  end;// If (Length(gvstrVHFChannelDataArray[vbytRecord, gcbytToneField]) = 0)

  // DTSS Function and Code
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytDTSSField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ',' + '' + ','
  else
    if gvstrVHFChannelDataArray[vbytRecord, gcbytDTSSField] = gcstrOff then
    begin
        vstrTRecord := vstrTRecord + gcstrTMV7Off + ',';
        vstrTRecord := vstrTRecord + '' + ',';
    end
    else
    begin
      vstrTRecord := vstrTRecord + gcstrTMV7On + ',';
      vstrTRecord := vstrTRecord + gvstrVHFChannelDataArray[vbytRecord,
      gcbytDTSSCodeField] + ','
    end;// if gvstrVHFChannelDataArray[vbytRecord, gcbytDTSSField]

  // Shift Offset
 If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytShiftOffsetField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    vstrTRecord := vstrTRecord +
    Copy(gvstrVHFChannelDataArray[vbytRecord, gcbytShiftOffsetField], 2, 2) +
    '.' +
    Copy(gvstrVHFChannelDataArray[vbytRecord, gcbytShiftOffsetField], 4, 2) + ',';

  // Scan
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytScanField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    if gvstrVHFChannelDataArray[vbytRecord, gcbytScanField] = gcstrOff then
        vstrTRecord := vstrTRecord + gcstrTMV7Off + ','
    else
      vstrTRecord := vstrTRecord + gcstrTMV7On + ',';

  // RF Power
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytRFPowerField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    case gvstrVHFChannelDataArray[vbytRecord, gcbytRFPowerField] of
      gcstrRFPowerLow :
        vstrTRecord := vstrTRecord + gcstrTMV7RFPowerLow + ',';
      gcstrRFPowerMedium :
        vstrTRecord := vstrTRecord + gcstrTMV7RFPowerMedium + ',';
       gcstrRFPowerHigh :
        vstrTRecord := vstrTRecord + gcstrTMV7RFPowerHigh + ',';
     end;// case gvstrVHFChannelDataArray[vbytRecord, gcbytRFPowerField]

  // Channel Name
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytChannelNameField]) = 0 then
    vstrTRecord := vstrTRecord + '' + ','
  else
    vstrTRecord := vstrTRecord + gvstrVHFChannelDataArray[vbytRecord,
                   gcbytChannelNameField] + ',';

  // Comments
  If Length(gvstrVHFChannelDataArray[vbytRecord, gcbytCommentsField]) = 0 then
    vstrTRecord := vstrTRecord + ''
  else
    vstrTRecord := vstrTRecord + gvstrVHFChannelDataArray[vbytRecord, gcbytCommentsField];

  Result := vstrTRecord;

end;// function MakeVHFRecord

//========================================================================================
procedure ParseVHFRecord(vbytRecNr : Byte; vstrRecord : string);

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
    '' :     gvstrVHFChannelDataArray[vbytRecNr, gcbytVFOField] := '';
    gcstrTMV7VFO_VHF : gvstrVHFChannelDataArray[vbytRecNr, gcbytVFOField] := gcstrVHFVFO;
    else
      gvstrVHFChannelDataArray[vbytRecNr, gcbytVFOField] :=gcstrUHFVFO;
  end;// case of vstrTstr
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // RX Frequency
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  if Length(vstrTStr) > 0 then
    vstrTStr := '00' + Copy(vstrRecord, 1, 3) + Copy(vstrRecord, 5, 3) + '000';
  gvstrVHFChannelDataArray[vbytRecNr, gcbytRXFrequencyField] := vstrTStr;
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
    gvstrVHFChannelDataArray[vbytRecNr, gcbytStepField] := IntToStr(vbytTByt);
  end
  else
    gvstrVHFChannelDataArray[vbytRecNr, gcbytStepField] := '';
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Shift
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrVHFChannelDataArray[vbytRecNr, gcbytShiftField] := '';
    gcstrTMV7ShiftSimplex :
      gvstrVHFChannelDataArray[vbytRecNr, gcbytShiftField] := gcstrShiftSimplex;
    gcstrTMV7ShiftPlus :
      gvstrVHFChannelDataArray[vbytRecNr, gcbytShiftField] := gcstrShiftPlus;
    gcstrTMV7ShiftMinus :
      gvstrVHFChannelDataArray[vbytRecNr, gcbytShiftField] := gcstrShiftMinus;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Reverse
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrVHFChannelDataArray[vbytRecNr, gcbytReverseField] := '';
    gcstrTMV7Off : gvstrVHFChannelDataArray[vbytRecNr, gcbytReverseField] := gcstrOff;
    else
      gvstrVHFChannelDataArray[vbytRecNr, gcbytReverseField] := gcstrOn;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Tone Function - This takes care of both Tone and CTCSS On/Off fields
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : begin
           gvstrVHFChannelDataArray[vbytRecNr, gcbytToneField] := '';
           gvstrVHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := '';
         end;// '', cstrNone
    gcstrTMV7None : begin
                 gvstrVHFChannelDataArray[vbytRecNr, gcbytToneField] := gcstrOff;
                 gvstrVHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := gcstrOff;
               end;// '', cstrNone
    gcstrTMV7Tone : begin
                 gvstrVHFChannelDataArray[vbytRecNr, gcbytToneField] := gcstrOn;
                 gvstrVHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := gcstrOff;
               end;// cstrTone
    else
      begin
        gvstrVHFChannelDataArray[vbytRecNr, gcbytToneField] := gcstrOff;
        gvstrVHFChannelDataArray[vbytRecNr, gcbytCTCSSField] := gcstrOn;
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
    if gvstrVHFChannelDataArray[vbytRecNr, gcbytToneField] = gcstrOn then
    begin
      // Tone has been selected so we have to populate the Tone Nr field and Default the
      // CTCSS Nr field
      vbytTByt := GetToneNrFromFrequency(vstrTStr);
      vstrTToneNr := IntToStr(vbytTByt);
      if Length(vstrTToneNr) = 1 then
        vstrTToneNr := '0' + vstrTToneNr;
      gvstrVHFChannelDataArray[vbytRecNr, gcbytToneNrField] := vstrTToneNr;
      gvstrVHFChannelDataArray[vbytRecNr, gcbytCTCSSNrField] := '01';
    end
    else
    begin
      // CTCSS has been selected so we have to populate the CTCSS Nr field and Default the
      // Tone Nr field
      vbytTByt := GetToneNrFromFrequency(vstrTStr);
      vstrTToneNr := IntToStr(vbytTByt);
      if Length(vstrTToneNr) = 1 then
        vstrTToneNr := '0' + vstrTToneNr;
      gvstrVHFChannelDataArray[vbytRecNr, gcbytCTCSSNrField] := vstrTToneNr;
      gvstrVHFChannelDataArray[vbytRecNr, gcbytToneNrField] := '01';
    end;//éé if gvstrFAVChannelDataArray[vbytRecNr, gcbytToneField] = gcstrOn

  end
  else
  begin
    // There is no Tone Frequency in the record so we clear the Tone Nr fields
    gvstrVHFChannelDataArray[vbytRecNr, gcbytToneNrField] := '01';
    gvstrVHFChannelDataArray[vbytRecNr, gcbytCTCSSNrField] := '01';
  end;//  if Length(vstrTStr) > 0
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // DTSS On/Off
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrVHFChannelDataArray[vbytRecNr, gcbytDTSSField] := '';
    gcstrTMV7Off : gvstrVHFChannelDataArray[vbytRecNr, gcbytDTSSField] :=
                     gcstrOff;
    else
      gvstrVHFChannelDataArray[vbytRecNr, gcbytDTSSField] := gcstrOn;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // DTSS Code
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrVHFChannelDataArray[vbytRecNr, gcbytDTSSCodeField] := '000';
    else
     gvstrVHFChannelDataArray[vbytRecNr, gcbytDTSSCodeField] := vstrTStr;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

    // Shift Offset
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrVHFChannelDataArray[vbytRecNr, gcbytShiftOffsetField] := '';
    else
      gvstrVHFChannelDataArray[vbytRecNr, gcbytShiftOffsetField] := '0' +
          Copy(vstrTStr, 1, 2) + Copy(vstrTStr, 4, 2) + '0000';
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Scan On/Off
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrVHFChannelDataArray[vbytRecNr, gcbytScanField] := '';
    gcstrTMV7Off : gvstrVHFChannelDataArray[vbytRecNr, gcbytScanField] := gcstrOff;
    else
      gvstrVHFChannelDataArray[vbytRecNr, gcbytScanField] := gcstrOn;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // RF Power
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  case vstrTStr of
    '' : gvstrVHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := '';
    gcstrTMV7RFPowerLow :
      gvstrVHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := gcstrRFPowerLow;
    gcstrTMV7RFPowerMedium :
      gvstrVHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := gcstrRFPowerMedium;
    gcstrTMV7RFPowerHigh :
      gvstrVHFChannelDataArray[vbytRecNr, gcbytRFPowerField] := gcstrRFPowerHigh;
  end;// case vstrTStr of
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Channel Name
  vbytCommaPos := Pos(',', vstrRecord );
  vstrTStr := Copy(vstrRecord, 1, vbytCommaPos-1);
  gvstrVHFChannelDataArray[vbytRecNr, gcbytChannelNameField] := vstrTStr;
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

  // Comments
  gvstrVHFChannelDataArray[vbytRecNr, gcbytCommentsField] := vstrRecord;

end;// procedure ParseVHFRecord;

//========================================================================================
end.// unit TMVFiles_VHF;

