unit LCDDisplay;

{$mode objfpc}{$H+}

//========================================================================================
//
//  LCDDisplay.pas
//
//  Calls: AppConstants
//         AppVariables
//         Main
//
//  Called By: BCCommand : TogglePTTBand
//             BUFCommand : BUFResponseHandler
//             BYCommand : BYResponseHandler
//             FAV : SetFAVChannel
//             Init : Initialize
//             Mem_VHF : TfrmMEM.bbtSelectClick
//             PSCommand : PSResponseHandler
//             Reverse : Toggle Reverse
//             RXCommand : RXResponseHandler
//             TXCommand : TXResponseHandler
//
//  Ver: 1.0.0
//
//  Date: 7 Dec 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, Graphics, SysUtils,
  // Application Units
  AppConstants, AppVariables, Utilities;

procedure LCDOff;
procedure LCDOn;
procedure UpdateLCDDisplay;

procedure DisplayBCStatus;
procedure DisplayUHFBusyStatus(vstrStatus : string);
procedure DisplayVHFBusyStatus(vstrStatus : string);
procedure DisplayUHFCTStatus;
procedure DisplayVHFCTStatus;
procedure DisplayUHFChannelName;
procedure DisplayVHFChannelName;
procedure DisplayUHFChannelNr;
procedure DisplayVHFChannelNr;
procedure DisplayUHFDataSource;
procedure DisplayVHFDataSource;
procedure DisplayUHFDTSSStatus;
procedure DisplayVHFDTSSStatus;
procedure DisplayUHFRXFrequency;
procedure DisplayVHFRXFrequency;
procedure DisplayUHFReverseStatus;
procedure DisplayVHFReverseStatus;
procedure DisplayUHFRFPowerStatus;
procedure DisplayVHFRFPowerStatus;
procedure DisplayUHFRXStatus(vstrStatus : string);
procedure DisplayVHFRXStatus(vstrStatus : string);
procedure DisplayUHFShiftStatus;
procedure DisplayVHFShiftStatus;
procedure DisplayUHFTXStatus(vstrStatus : string);
procedure DisplayVHFTXStatus(vstrStatus : string);

implementation

uses
    BYCommand, Main;

{========================================================================================}
{                      LCD PANEL                                                         }
{========================================================================================}
procedure LCDOff;
begin
  frmMain.pnlLCD.Visible := False;
end;// procedure LCDOff;

//----------------------------------------------------------------------------------------
procedure LCDOn;
begin
  frmMain.pnlLCD.Visible := True;
end;// procedure LCDOn;

//----------------------------------------------------------------------------------------
procedure UpdateLCDDisplay;
begin
  DisplayBCStatus;
  GetUHFBYStatus;
  GetVHFBYStatus;
  DisplayUHFChannelName;
  DisplayVHFChannelName;
  DisplayUHFChannelNr;
  DisplayVHFChannelNr;
  DisplayUHFCTStatus;
  DisplayVHFCTStatus;
  DisplayUHFDataSource;
  DisplayVHFDataSource;
  DisplayUHFDTSSStatus;
  DisplayVHFDTSSStatus;
  DisplayUHFRXFrequency;
  DisplayVHFRXFrequency;
  DisplayUHFReverseStatus;
  DisplayVHFReverseStatus;
  DisplayUHFRFPowerStatus;
  DisplayVHFRFPowerStatus;
  DisplayUHFShiftStatus;
  DisplayVHFShiftStatus;

end;// procedure UpdateLCDDisplay;

{========================================================================================}
{                        BC STATUS                                                       }
{========================================================================================}
procedure DisplayBCStatus;

const
  cbytPTTFreqSize = 48;
  cbytInactiveFreqSize = 32;

begin

  if gvstrPTTBand = gcstrVHF then // Current Band is VHF
  begin
    frmMain.lblVHFPTT.Visible := True;
    frmMain.lblUHFPTT.Visible := False;
    frmMain.lblVHFFreq.Font.Size := cbytPTTFreqSize;
    frmMain.lblVHFFreq.Font.Bold := True;
    frmMain.lblUHFFreq.Font.Size := cbytInactiveFreqSize;
    frmMain.lblUHFFreq.Font.Bold := False;
    DisplayVHFReverseStatus;
  end
  else // it is UHF
  begin
    frmMain.lblVHFPTT.Visible := False;
    frmMain.lblUHFPTT.Visible := True;
    frmMain.lblVHFFreq.Font.Size := cbytInactiveFreqSize;
    frmMain.lblVHFFreq.Font.Bold := False;
    frmMain.lblUHFFreq.Font.Size := cbytPTTFreqSize;
    frmMain.lblUHFFreq.Font.Bold := True;
    DisplayUHFReverseStatus;
  end;// if gvstrPTTBand = gcstrVHF

end;// procedure DisplayBCStatus

{========================================================================================}
{                             BUSY STATUS                                                }
{========================================================================================}
procedure DisplayUHFBusyStatus(vstrStatus : string);
begin
  frmMain.lblUHFOnAirBusy.Caption := vstrStatus;
end;// procedure DisplayUHFBusyStatus

//----------------------------------------------------------------------------------------
procedure DisplayVHFBusyStatus(vstrStatus : string);
begin
  frmMain.lblVHFOnAirBusy.Caption := vstrStatus;
end;// procedure DisplayVHFBusyStatus

{========================================================================================}
{                            CHANNEL NAME                                                }
{========================================================================================}
procedure DisplayUHFChannelName;
begin
  frmMain.lblUHFChannelName.Caption := gvstrUHFChannelName;
end;// procedure DisplayUHFChannelName;

//----------------------------------------------------------------------------------------
procedure DisplayVHFChannelName;
begin
  frmMain.lblVHFChannelName.Caption := gvstrVHFChannelName;
end;// procedure DisplayVHFChannelName;

{========================================================================================}
{                           CHANNEL NR                                                   }
{========================================================================================}
procedure DisplayUHFChannelNr;
begin
  frmMain.lblUHFChannelNr.Caption := gvstrUHFChannelNr;
end;// procedure DisplayUHFChannelNr;

//----------------------------------------------------------------------------------------
procedure DisplayVHFChannelNr;
begin
  frmMain.lblVHFChannelNr.Caption := gvstrVHFChannelNr;
end;// procedure DisplayVHFChannelNr;

{========================================================================================}
{                                 CT STATUS                                              }
{========================================================================================}
procedure DisplayUHFCTStatus;
 // This routine handles the status of both the UHF Tone and CTCSS functions. They are
// mutually exclusive so only one conition can be present. Either CTCSS, Tone or None.
VAR
  vwrdCTFreq : Word;
  vbytCode : Byte;

begin

  if gvstrUHFCTCSS = gcstrOn then
  begin
    frmMain.lblUHFTCT.Caption := 'CT';
    frmMain.lblUHFTCT.visible := True;
    frmMain.lblUHFTCTFreq.Caption := GetToneFrequencyFromToneNr(StrToInt(gvstrUHFCTCSSNr));
    frmMain.lblUHFTCTFreq.visible := True;
  end
  else if gvstrUHFTone = gcstrOn then
  begin
    frmMain.lblUHFTCT.Caption := 'T';
    frmMain.lblUHFTCT.visible := True;
    frmMain.lblUHFTCTFreq.Caption := GetToneFrequencyFromToneNr(StrToInt(gvstrUHFToneNr));
    frmMain.lblUHFTCTFreq.visible := True;
  end
  else
  begin
    frmMain.lblUHFTCT.visible := False;
    frmMain.lblUHFTCTFreq.visible := False;
  end;// if gvstrUHFTone = gcstrOn

end;// procedure DisplayUHFCTCStatus

//----------------------------------------------------------------------------------------
procedure DisplayVHFCTStatus;
// This routine handles the status of both the VHF Tone and CTCSS functions. They are
// mutually exclusive so only one conition can be present. Either CTCSS, Tone or None.
VAR
 vwrdCTFreq : Word;
 vbytCode : Byte;

begin

 if gvstrVHFCTCSS = gcstrOn then
 begin
   frmMain.lblVHFTCT.Caption := 'CT';
   frmMain.lblVHFTCT.visible := True;
   frmMain.lblVHFTCTFreq.Caption := GetToneFrequencyFromToneNr(StrToInt(gvstrVHFCTCSSNr));
   frmMain.lblVHFTCTFreq.visible := True;
 end
 else if gvstrVHFTone = gcstrOn then
 begin
   frmMain.lblVHFTCT.Caption := 'T';
   frmMain.lblVHFTCT.visible := True;
   frmMain.lblVHFTCTFreq.Caption := GetToneFrequencyFromToneNr(StrToInt(gvstrVHFToneNr));
   frmMain.lblVHFTCTFreq.visible := True;
 end
 else
 begin
   frmMain.lblVHFTCT.visible := False;
   frmMain.lblVHFTCTFreq.visible := False;
 end;// if gvstrVHFTone = gcstrOn

end;// procedure DisplayVHFCTStatus

{========================================================================================}
{                           DATA SOURCE                                                  }
{========================================================================================}
procedure DisplayUHFDataSource;
begin
  frmMain.lblUHFDataSource.Caption := gvstrUHFDataSource;
end;// procedure DisplayUHFDataSource;

//----------------------------------------------------------------------------------------
procedure DisplayVHFDataSource;
begin
  frmMain.lblVHFDataSource.Caption := gvstrVHFDataSource;
end;// procedure DisplayVHFDataSource;

{========================================================================================}
{                    DTSS STATUS                                                         }
{========================================================================================}
procedure DisplayUHFDTSSStatus;
begin

  if gvstrUHFDTSS = gcstrOn then
  begin
    frmMain.lblUHFDTSSCode.caption := gvstrUHFDTSSCode;
    frmMain.lblUHFDTSS.visible := True;
  end
  else
  begin
    frmMain.lblUHFDTSS.visible := False;
    frmMain.lblUHFDTSSCode.visible := False;
  end;// if gvstrUHFDTSS = gcstrOn

end;// procedure DisplayUHFDTSSStatus;

//----------------------------------------------------------------------------------------
procedure DisplayVHFDTSSStatus;
begin

    if gvstrVHFDTSS = gcstrOn then
    begin
      frmMain.lblVHFDTSSCode.caption := gvstrVHFDTSSCode;
      frmMain.lblVHFDTSS.visible := True;
    end
    else
    begin
      frmMain.lblVHFDTSS.visible := False;
      frmMain.lblVHFDTSSCode.visible := False;
    end;// if gvstrVHFDTSS = gcstrOn

end;// procedure DisplayVHFDTSSStatus;

{========================================================================================}
{                  FREQUENCY                                                             }
{========================================================================================}
procedure DisplayUHFRXFrequency;
begin
  frmMain.lblUHFFreq.Caption := Copy(gvstrUHFRXFrequency,3,3) +
                                '.' +
                                Copy(gvstrUHFRXFrequency,6,3);
end;// procedure DisplayUHFRXFrequency;

//----------------------------------------------------------------------------------------
procedure DisplayVHFRXFrequency;
begin
  frmMain.lblVHFFreq.Caption := Copy(gvstrVHFRXFrequency,3,3) +
                                '.' +
                                Copy(gvstrVHFRXFrequency,6,3);
end;// procedure DisplayVHFRXFrequency;

{========================================================================================}
{                   REVERSE STATUS                                                       }
{========================================================================================}
procedure DisplayUHFReverseStatus;
begin

  if gvstrUHFReverseState = gcstrOn then
  begin
    frmMain.lblUHFReverse.visible := True;
    frmMain.bbtReverse.Font.Color := clRed;
    frmMain.bbtReverse.Font.Style := [fsBold];
  end
  else
  begin
    frmMain.lblUHFReverse.visible := False;
    frmMain.bbtReverse.Font.Color := clBlack;
    frmMain.bbtReverse.Font.Style := [];
  end;// if gvstrUHFReverseState = gcstrOn

end;// procedure DisplayUHFReverseStatus

{----------------------------------------------------------------------------------------}
procedure DisplayVHFReverseStatus;
begin

    if gvstrVHFReverseState = gcstrOn then
    begin
      frmMain.lblVHFReverse.visible := True;
      frmMain.bbtReverse.Font.Color := clRed;
      frmMain.bbtReverse.Font.Style := [fsBold];
    end
    else
    begin
      frmMain.lblVHFReverse.visible := False;
      frmMain.bbtReverse.Font.Color := clBlack;
      frmMain.bbtReverse.Font.Style := [];
    end;// if gvstrVHFReverseState = gcstrOn

end;// procedure DisplayVHFReverseStatus

{========================================================================================}
{                    RF POWER STATUS                                                     }
{========================================================================================}
procedure DisplayUHFRFPowerStatus;
begin
  case gvstrUHFRFPower of
    gcstrRFPowerLow : frmMain.lblUHFRFPwr.Caption := 'L';
    gcstrRFPowerMedium : frmMain.lblUHFRFPwr.Caption := 'M';
    gcstrRFPowerHigh : frmMain.lblUHFRFPwr.Caption := 'H';
  end;// case gvstrUHFRFPower of
end;// procedure DisplayUHFRFPowerStatus;

//----------------------------------------------------------------------------------------
procedure DisplayVHFRFPowerStatus;
begin
  case gvstrVHFRFPower of
    gcstrRFPowerLow : frmMain.lblVHFRFPwr.Caption := 'L';
    gcstrRFPowerMedium : frmMain.lblVHFRFPwr.Caption := 'M';
    gcstrRFPowerHigh : frmMain.lblVHFRFPwr.Caption := 'H';
  end;// case gvstrVHFRFPower of
end;// procedure DisplayVHFRFPowerStatus;

{========================================================================================}
{                             RX STATUS                                                }
{========================================================================================}
procedure DisplayUHFRXStatus(vstrStatus : string);
begin
  frmMain.lblUHFOnAirBusy.Caption := vstrStatus;
end;// procedure DisplayUHFRXStatus

//----------------------------------------------------------------------------------------
procedure DisplayVHFRXStatus(vstrStatus : string);
begin
  frmMain.lblVHFOnAirBusy.Caption := vstrStatus;
end;// procedure DisplayVHFRXStatus

{========================================================================================}
{                SHIFT STATUS                                                            }
{========================================================================================}
procedure DisplayUHFShiftStatus;
begin

    if gvstrUHFShift = gcstrShiftSimplex then
      frmMain.lblUHFshift.Caption := ''
    else if gvstrUHFShift = gcstrShiftPlus then
      frmMain.lblUHFshift.Caption := '+'
    else
      frmMain.lblUHFshift.Caption := '-';

end;// procedure DisplayUHFShiftStatus;

//----------------------------------------------------------------------------------------
procedure DisplayVHFShiftStatus;
begin

  if gvstrVHFShift = gcstrShiftSimplex then
    frmMain.lblVHFshift.Caption := ''
  else if gvstrVHFShift = gcstrShiftPlus then
    frmMain.lblVHFshift.Caption := '+'
  else
    frmMain.lblVHFshift.Caption := '-';

end;// procedure DisplayVHFShiftStatus;

{========================================================================================}
{                             TX STATUS                                                }
{========================================================================================}
procedure DisplayUHFTXStatus(vstrStatus : string);
begin
  frmMain.lblUHFOnAirBusy.Caption := vstrStatus;
end;// procedure DisplayUHFTXStatus

//----------------------------------------------------------------------------------------
procedure DisplayVHFTXStatus(vstrStatus : string);
begin
  frmMain.lblVHFOnAirBusy.Caption := vstrStatus;
end;// procedure DisplayVHFTXStatus

{========================================================================================}
end.// unit LCDDisplay

