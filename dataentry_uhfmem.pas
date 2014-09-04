unit DataEntry_UHFMem;

{$mode objfpc}{$H+}

//========================================================================================
//
//  DataEntry_UHFMEM.pas
//
//  Calls: AppConstants
//         AppTypes
//         AppVariables
//         DataEntry
//         Mem_UHF : LoadUHFStringGrid
//         Utilities : GetToneIndexFromToneNr
//
//  Called By: DataEntry : TfrmDataEntry.FormActivate
//                         TfrmDataEntry.bbtResetClick
//                         TfrmDataEntry.bbtSaveClick
//
//  Ver: 1.0.0
//
//  Date: 1 Sep 2014
//
//========================================================================================

interface

uses
  Classes, Dialogs, SysUtils,
  // Application Units
  AppConstants, AppTypes, AppVariables, MEM_UHF, Utilities;

procedure DataEntry_UHFMEM_Init;
procedure DataEntry_UHFMEM_Save;

implementation

uses
  DataEntry;

//========================================================================================
procedure DataEntry_UHFMEM_Init;

var
  vbytToneNr : Byte;

begin

    frmDataEntry.Caption := 'UHF ' + frmDataEntry.cstrMemFormTitle;
    frmDataEntry.edtSource.Text := Format('UHF%.2d',[frmDataEntry.vbytChannelNumber]);

    //=====================
    // Set the RX Frequency
    //=====================

    if Length(gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
              gcbytRXFrequencyField]) > 0 then
              frmDataEntry.edtRXFrequency.Text :=
      Copy(gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
        gcbytRXFrequencyField], 3, 3) +
      '.' +
      Copy(gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
        gcbytRXFrequencyField], 6, 3)
    else
      frmDataEntry.edtRXFrequency.Text := '';

    //==========================
    // Set the Band Radio buttons
    //==========================

    if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytVFOField] = gcstrUHF then
      frmDataEntry.rbtUHF.Checked := True
    else if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytVFOField] = gcstrVHF then
      frmDataEntry.rbtVHF.Checked := True
    else
      frmDataEntry.rbtUHF.Checked := True;

  {  if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytVFOField] = gcstrUHF then
      frmDataEntry.rbtUHF.Checked := True
    else
      frmDataEntry.rbtVHF.Checked := True; }

    //===========================
    // Set the Shift Radio buttons
    //===========================

    if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytShiftField] = gcstrShiftPlus then
      frmDataEntry.rbtPlus.Checked := True
    else if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytShiftField] = gcstrShiftMinus then
      frmDataEntry.rbtMinus.Checked := True
    else
      frmDataEntry.rbtSimplex.Checked := True;

    //======================================
    // Set the Shift offset and calulate and Display the TX Frequency
    //======================================

     if Length(frmDataEntry.edtRXFrequency.Text) > 0 then
     begin
       frmDataEntry.edtTXFrequency.Text := frmDataEntry.CalculateTXFrequency;
       frmDataEntry.SetShiftOffset;
     end
     else
       frmDataEntry.edtTXFrequency.Text := '';

     //====================================================
     // Set the Step Combobox based on the band
     //====================================================
     if Length(gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
               gcbytRXFrequencyField]) > 0 then
       frmDataEntry.cbxStep.ItemIndex := StrToInt(gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                    gcbytStepField])
     else
     if frmDataEntry.rbtUHF.Checked then
       frmDataEntry.cbxStep.ItemIndex := StrToInt(gcstrConfigUHFStep)
     else
       frmDataEntry.cbxStep.ItemIndex := StrToInt(gcstrConfigVHFStep);

     //====================================================
    // Set the Tone Checkboxes and Tone Frequency Combobox
    //====================================================
    if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytToneField] = gcstrOn then
      frmDataEntry.rbtTone.Checked := True
    else if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytCTCSSField] = gcstrOn then
      frmDataEntry.rbtCTCSS.Checked := True
    else
      frmDataEntry.rbtNoTones.Checked := True;

    //===========================================================
    // Determine the correct Index and set the Tone Freq Combobox
    //===========================================================

    if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytToneNrField] = '' then vbytToneNr := 1
    else vbytToneNr := StrToInt(gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytToneNrField]);
    frmDataEntry.cbxTones.ItemIndex := GetToneIndexFromToneNr( vbytToneNr);

    // Now Clear the Text field and disable the list box if there is no Tone function
    //  selected.
    if frmDataEntry.rbtNoTones.Checked then
    begin
      frmDataEntry.cbxTones.Text := '';
      frmDataEntry.cbxTones.Enabled := False;
    end;

    //======================================
    //  Set the DTSS Checkbox and Code field
    //======================================
    if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytDTSSField] = gcstrOn then
    begin
      frmDataEntry.chkDTSS.Checked := True;
      frmDataEntry.EnableDTSSCode;
    end
    else
    begin
      frmDataEntry.chkDTSS.Checked := False;
      frmDataEntry.DisableDTSSCode;
    end;

    frmDataEntry.SetDTSSCode;

    //======================
    // Set the Scan Checkbox
    //======================

    if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytScanField] = gcstrOn then
      frmDataEntry.chkScan.Checked := True
    else
      frmDataEntry.chkScan.Checked := False;

    //===============================
    // Set the RF Power Radio buttons
    //===============================

    if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytRFPowerField] = gcstrRFPowerLow then
       frmDataEntry.rbtRFPowerLow.Checked := True
    else if gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytShiftField] = gcstrRFPowerMedium then
      frmDataEntry.rbtRFPowerMedium.Checked := True
    else
      frmDataEntry.rbtRFPowerHigh.Checked := True;

    //=====================
    // Set the Channel Name
    //=====================

    frmDataEntry.edtChannelName.Text := gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                        gcbytChannelNameField];

    //=================
    // Set the Comments
    //=================

    frmDataEntry.edtComments.Text := gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytCommentsField];

end;// procedure DataEntry_UHFMEM_Init

//========================================================================================
procedure DataEntry_UHFMEM_Save;

var
  vstrTStr : string;

begin

  //==================================================================
  // We populate the Favourite Array using the Favourite Button number
  // as the primary key and the Field number as the sceondary key.
  //==================================================================

  // VFO/Band
  if frmdataEntry.rbtUHF.Checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytVFOField] := gcstrUHF
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytVFOField] := gcstrVHF;

  // RX Frequency
  vstrTStr := '00' +
              Copy(frmDataEntry.edtRXFrequency.Text,1,3) +
              Copy(frmDataEntry.edtRXFrequency.Text,5,3) +
              '000';
  gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRXFrequencyField] :=
    vstrTStr;

  // Step Size
  if frmdataEntry.rbtUHF.Checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytStepField] :=
      gcstrConfigUHFStep
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytStepField] :=
      gcstrConfigVHFStep;

  // Shift Indicator
  if frmDataEntry.rbtSimplex.Checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytShiftField] :=
      gcstrShiftSimplex
  else if frmDataEntry.rbtPlus.Checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytShiftField] :=
      gcstrShiftPlus
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytShiftField] :=
      gcstrShiftMinus;

  // Reverse switch
  // It is not configurable. It may only be toggled by the GUI button
  // so we default it to Off
  gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytReverseField] :=
    gcstrOff;

  // Tone and CTCSS switch
  // Although there are two data fields, they are mutually exclusive so we handle
  // them together. They may both be Off, but only one of them may be on at a time
  if frmDataEntry.rbtNoTones.checked then
  begin
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneField] :=
      gcstrOff;
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSField] :=
      gcstrOff;
  end
  else if frmDataEntry.rbtTone.checked then
  begin
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneField] :=
      gcstrOn;
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSField] :=
      gcstrOff;
  end
  else
  begin
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneField] :=
      gcstrOff;
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSField] :=
      gcstrOn;
  end;// if frmDataEntry.rbtNoTones.checked

  // DTSS switch
  if frmDataEntry.chkDTSS.checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSField] :=
      gcstrOn
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSField] :=
      gcstrOff;

  // Tone Number
  if frmDataEntry.rbtTone.checked then
  begin
    vstrTStr := IntToStr(GetToneNrFromIndex(frmDataEntry.cbxTones.ItemIndex));
    if StrToInt(vstrTStr) > 10 then
      gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneNrField] := vstrTStr
    else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneNrField] := '0' +
                                                                               vstrTStr;
  end
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneNrField] := '01';

     // DTSS Code
  if frmDataEntry.chkDTSS.checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSCodeField] :=
      frmDataEntry.edtDTSSCode.Text
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSCodeField] := '000';

  // CTCSS Nr
  if frmDataEntry.rbtCTCSS.checked then
  begin
    vstrTStr := IntToStr(GetToneNrFromIndex(frmDataEntry.cbxTones.ItemIndex));
    if StrToInt(vstrTStr) > 10 then
      gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSNrField] := vstrTStr
    else
      gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSNrField] := '0' +
                                                                                  vstrTStr;
  end
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSNrField] := '01';

  // Shift Offset
  if frmDataEntry.rbtSimplex.checked then
  gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                             gcbytShiftOffsetField] := '000000000'
  else
  begin
    if frmDataEntry.rbtVHF.Checked then
      gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytShiftOffsetField] := '000600000'
    else
      gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytShiftOffsetField] := '005000000';
  end;// if frmDataEntry.rbtSimplex.checked

  // Scan switch
  if frmdataEntry.chkScan.Checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytScanField] := gcstrOn
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytScanField] := gcstrOff;

  // RF Power
  if frmDataEntry.rbtRFPowerLow.Checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRFPowerField] :=
      gcstrRFPowerLow
  else if frmDataEntry.rbtRFPowerMedium.Checked then
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRFPowerField] :=
      gcstrRFPowerMedium
  else
    gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRFPowerField] :=
      gcstrRFPowerHigh;

  // Now we save the Channel Name
  gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytChannelNameField] :=
    frmDataEntry.edtChannelName.Text;

   // Now the Comments
   gvstrUHFChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCommentsField] :=
     frmDataEntry.edtComments.Text;

//   DisplayDataArray(drtUHFMEM, frmDataEntry.vbytChannelNumber);
   LoadUHFStringGrid;

end;// procedure DataEntry_UHFMEM_Save;

//========================================================================================

end.// unit DataEntry_UHFMem;

