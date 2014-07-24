unit PCCommand;

{$mode objfpc}{$H+}

//========================================================================================
//
//  PCCommand.pas
//
//  Description: This routine sets the RF Power level for the selected band. This will
//               set the RF Power temporarily. The RF Power will return to the configured
//               power level whenever a channel is changed or a VFO frequency is selected.
//               For a permenant change the channel must be re-configured.
//
//               Command format is PC Band,PwrLvl where band is gcstrUHF or gcstrVHF
//               and PwrLvl is gcstrRFPowerLow, gcstrRFPowerMedium or gcstrRFPowerHigh.
//
//  Calls: AppConstants
//         AppVariables
//         LCDDisplay : DisplayUHFRFPowerStatus
//         SerialStuiff : SendCommand
//
//  Called By: Main : TfrmMain.btnRFPowerClick
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
  AppCOnstants, AppVariables, LCDDisplay, SerialStuff;

procedure ToggleRFPower;

implementation

procedure ToggleRFPower;
begin
  case gvstrPTTBand of
    gcstrUHF : begin
                 case gvstrUHFRFPower of
                   gcstrRFPowerLow : gvstrUHFRFPower := gcstrRFPowerMedium;
                   gcstrRFPowerMedium : gvstrUHFRFPower := gcstrRFPowerHigh;
                   gcstrRFPowerHigh : gvstrUHFRFPower := gcstrRFPowerLow;
                 end;// case gvstrUHFRFPower
                 SendCommand('PC',gcstrUHF + ',' + gvstrUHFRFPower);
                 DisplayUHFRFPowerStatus;
               end;// gcstrUHF
    gcstrVHF : Begin
                 case gvstrVHFRFPower of
                   gcstrRFPowerLow : gvstrVHFRFPower := gcstrRFPowerMedium;
                   gcstrRFPowerMedium : gvstrVHFRFPower := gcstrRFPowerHigh;
                   gcstrRFPowerHigh : gvstrVHFRFPower := gcstrRFPowerLow;
                 end;// case gvstrVHFRFPower
                 SendCommand('PC',gcstrVHF + ',' + gvstrVHFRFPower);
                 DisplayVHFRFPowerStatus;
               end;// gcstrVHF
  end;// case gvstrPTTBand
end;// procedure Toggle RFPower;

end.// unit PCCommand

