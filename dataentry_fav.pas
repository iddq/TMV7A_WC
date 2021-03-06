unit DataEntry_FAV;

{$mode objfpc}{$H+}

//========================================================================================
//
//  DataEntry_FAV.pas
//
//  Calls: AppConstants
//         AppTypes
//         AppVariables
//         DataEntry : frmDataEntry.CalculateTXFrequency
//                     frmDataEntry.DisableDTSSCode
//                     frmDataEntry.EnableDTSSCode
//                     frmDataEntry.SetDTSSCode
//                     frmDataEntry.SetShiftOffset
//         Main
//         Utilities : GetToneIndexFromToneNr
//                     DisplayDataArray
//
//  Called By: DataEntry : TfrmDataEntry.bbtClearClick
//                         TfrmDataEntry.bbtResetClick
//                         TfrmDataEntry.FormActivate
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
  AppCOnstants, AppTypes, AppVariables, Utilities;

procedure DataEntry_FAV_Init;
procedure DataEntry_FAV_Save;

implementation

uses
  DataEntry, Main;

//========================================================================================
procedure DataEntry_FAV_Init;

var
  vbytToneNr : Byte;

begin

    frmDataEntry.Caption := frmDataEntry.cstrFavFormTitle;
    frmDataEntry.edtSource.Text := Format('FAV%d',[frmDataEntry.vbytChannelNumber]);

    //=====================
    // Set the RX Frequency
    //=====================

    if Length(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
              gcbytRXFrequencyField]) > 0 then
              frmDataEntry.edtRXFrequency.Text :=
      Copy(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
        gcbytRXFrequencyField], 3, 3) +
      '.' +
      Copy(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
        gcbytRXFrequencyField], 6, 3)
    else
      frmDataEntry.edtRXFrequency.Text := '';

    //==========================
    // Set the Band Radio buttons
    //==========================
 {   if Length(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
              gcbytRXFrequencyField]) > 0 then
      frmDataEntry.cbxStep.ItemIndex := StrToInt(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytStepField])
    else
    if frmDataEntry.rbtUHF.Checked then
      frmDataEntry.cbxStep.ItemIndex := StrToInt(gcstrUHFStep)
    else
      frmDataEntry.cbxStep.ItemIndex := StrToInt(gcstrVHFStep);  }

    if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytVFOField] = gcstrUHF then
      frmDataEntry.rbtUHF.Checked := True
    else if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytVFOField] = gcstrVHF then
      frmDataEntry.rbtVHF.Checked := True
    else
      frmDataEntry.rbtVHF.Checked := True;

    //===========================
    // Set the Shift Radio buttons
    //===========================

    if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytShiftField] = gcstrShiftPlus then
      frmDataEntry.rbtPlus.Checked := True
    else if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
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
     if Length(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
               gcbytRXFrequencyField]) > 0 then
       frmDataEntry.cbxStep.ItemIndex := StrToInt(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                    gcbytStepField])
     else
     if frmDataEntry.rbtUHF.Checked then
       frmDataEntry.cbxStep.ItemIndex := StrToInt(gcstrConfigUHFStep)
     else
       frmDataEntry.cbxStep.ItemIndex := StrToInt(gcstrConfigVHFStep);

    //====================================================
    // Set the Tone Checkboxes and Tone Frequency Combobox
    //====================================================
    if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytToneField] = gcstrOn then
      frmDataEntry.rbtTone.Checked := True
    else if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytCTCSSField] = gcstrOn then
      frmDataEntry.rbtCTCSS.Checked := True
    else
      frmDataEntry.rbtNoTones.Checked := True;

    //===========================================================
    // Determine the correct Index and set the Tone Freq Combobox
    //===========================================================

    if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytToneNrField] = '' then vbytToneNr := 1
    else vbytToneNr := StrToInt(gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytToneNrField]);
    frmDataEntry.cbxTones.ItemIndex := GetToneIndexFromToneNr( vbytToneNr);

    // Now Clear the Text field if there is no Tone function selected.
    if frmDataEntry.rbtNoTones.Checked then
      frmDataEntry.cbxTones.Text := '';

    //======================================
    //  Set the DTSS Checkbox and Code field
    //======================================
    if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
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

    if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                   gcbytScanField] = gcstrOn then
      frmDataEntry.chkScan.Checked := True
    else
      frmDataEntry.chkScan.Checked := False;

    //===============================
    // Set the RF Power Radio buttons
    //===============================

    if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytRFPowerField] = gcstrRFPowerLow then
       frmDataEntry.rbtRFPowerLow.Checked := True
    else if gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytShiftField] = gcstrRFPowerMedium then
      frmDataEntry.rbtRFPowerMedium.Checked := True
    else
      frmDataEntry.rbtRFPowerHigh.Checked := True;

    //=====================
    // Set the Channel Name
    //=====================

    frmDataEntry.edtChannelName.Text := gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                        gcbytChannelNameField];

    //=================
    // Set the Comments
    //=================

    frmDataEntry.edtComments.Text := gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                                     gcbytCommentsField];

end;// procedure FAVFormInit;  }

//========================================================================================
procedure DataEntry_FAV_Save;

var
  vstrTStr : string;

begin

  //==================================================================
  // We populate the Favourite Array using the Favourite Button number
  // as the primary key and the Field number as the sceondary key.
  //==================================================================

  // VFO/Band
  if frmdataEntry.rbtVHF.Checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytVFOField] := gcstrVHF
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytVFOField] := gcstrUHF;

  // RX Frequency
  vstrTStr := '00' +
              Copy(frmDataEntry.edtRXFrequency.Text,1,3) +
              Copy(frmDataEntry.edtRXFrequency.Text,5,3) +
              '000';
  gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRXFrequencyField] :=
    vstrTStr;

  // Step Size
  if frmdataEntry.rbtVHF.Checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytStepField] :=
      gcstrConfigVHFStep
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytStepField] :=
      gcstrConfigUHFStep;

  // Shift Indicator
  if frmDataEntry.rbtSimplex.Checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytShiftField] :=
      gcstrShiftSimplex
  else if frmDataEntry.rbtPlus.Checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytShiftField] :=
      gcstrShiftPlus
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytShiftField] :=
      gcstrShiftMinus;

  // Reverse switch
  // It is not configurable. It may only be toggled by the GUI button
  // so we default it to Off
  gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytReverseField] :=
    gcstrOff;

  // Tone and CTCSS switch
  // Although there are two data fields, they are mutually exclusive so we handle
  // them together. They may both be Off, but only one of them may be on at a time
  if frmDataEntry.rbtNoTones.checked then
  begin
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneField] :=
      gcstrOff;
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSField] :=
      gcstrOff;
  end
  else if frmDataEntry.rbtTone.checked then
  begin
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneField] :=
      gcstrOn;
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSField] :=
      gcstrOff;
  end
  else
  begin
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneField] :=
      gcstrOff;
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSField] :=
      gcstrOn;
  end;// if frmDataEntry.rbtNoTones.checked

  // DTSS switch
  if frmDataEntry.chkDTSS.checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSField] :=
      gcstrOn
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSField] :=
      gcstrOff;

  // Tone Number
  if frmDataEntry.rbtTone.checked then
  begin
    vstrTStr := IntToStr(GetToneNrFromIndex(frmDataEntry.cbxTones.ItemIndex));
    if StrToInt(vstrTStr) > 10 then
      gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneNrField] := vstrTStr
    else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneNrField] := '0' +
                                                                               vstrTStr;
  end
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytToneNrField] := '01';

     // DTSS Code
  if frmDataEntry.chkDTSS.checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSCodeField] :=
      frmDataEntry.edtDTSSCode.Text
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytDTSSCodeField] := '000';

  // CTCSS Nr
  if frmDataEntry.rbtCTCSS.checked then
  begin
    vstrTStr := IntToStr(GetToneNrFromIndex(frmDataEntry.cbxTones.ItemIndex));
    if StrToInt(vstrTStr) > 10 then
      gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSNrField] := vstrTStr
    else
      gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSNrField] := '0' +
                                                                                  vstrTStr;
  end
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCTCSSNrField] := '01';

  // Shift Offset
  if frmDataEntry.rbtSimplex.checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                             gcbytShiftOffsetField] := '000000000'
  else
  begin
    if frmDataEntry.rbtVHF.Checked then
      gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytShiftOffsetField] := '000600000'
    else
      gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber,
                               gcbytShiftOffsetField] := '005000000';
  end;// if frmDataEntry.rbtSimplex.checked

  // Scan switch
  if frmdataEntry.chkScan.Checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytScanField] := gcstrOn
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytScanField] := gcstrOff;

  // RF Power
  if frmDataEntry.rbtRFPowerLow.Checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRFPowerField] :=
      gcstrRFPowerLow
  else if frmDataEntry.rbtRFPowerMedium.Checked then
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRFPowerField] :=
      gcstrRFPowerMedium
  else
    gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytRFPowerField] :=
      gcstrRFPowerHigh;

  // Now we save the Button Name and set the apprpriate button caption
  gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytChannelNameField] :=
    frmDataEntry.edtChannelName.Text;

  case frmDataEntry.vbytChannelNumber of
    1 : frmMain.bbtFAV01.Caption := frmDataEntry.edtChannelName.Text;
    2 : frmMain.bbtFAV02.Caption := frmDataEntry.edtChannelName.Text;
    3 : frmMain.bbtFAV03.Caption := frmDataEntry.edtChannelName.Text;
    4 : frmMain.bbtFAV04.Caption := frmDataEntry.edtChannelName.Text;
    5 : frmMain.bbtFAV05.Caption := frmDataEntry.edtChannelName.Text;
    6 : frmMain.bbtFAV06.Caption := frmDataEntry.edtChannelName.Text;
    7 : frmMain.bbtFAV07.Caption := frmDataEntry.edtChannelName.Text;
    8 : frmMain.bbtFAV08.Caption := frmDataEntry.edtChannelName.Text;
    9 : frmMain.bbtFAV09.Caption := frmDataEntry.edtChannelName.Text;
    10 : frmMain.bbtFAV10.Caption := frmDataEntry.edtChannelName.Text;
    11 : frmMain.bbtFAV11.Caption := frmDataEntry.edtChannelName.Text;
    12 : frmMain.bbtFAV12.Caption := frmDataEntry.edtChannelName.Text;
   end;// case vbytChannelNumber of

   // Now the Comments
   gvstrFAVChannelDataArray[frmDataEntry.vbytChannelNumber, gcbytCommentsField] :=
     frmDataEntry.edtComments.Text;

   DisplayDataArray(drtFAV, frmDataEntry.vbytChannelNumber);

end;// procedure DataEntry_FAV_Save;

//========================================================================================

end.// unit DataEntry_FAV;

