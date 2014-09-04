unit ColourSchemes;

{$mode objfpc}{$H+}

//========================================================================================
//
//  ColourSchemes.pas
//
//  Calls: AppVariables
//         Main
//
//  Called By: Configure : ShowColourScheme
//             Init : Initialize
//             Main : TfrmMain.mnuConfigColourScheme1Click
//
//  Ver: 1.0.0
//
//  Date: 27 Aug 2014
//
//========================================================================================

interface

uses
  Classes, Dialogs, Graphics, SysUtils,
  // Application Units
  AppVariables;

procedure SetColourScheme (vstrColourScheme : String);

const
  vclrBackColor1 = $00FF8000;
  vclrForeColor1 = clWhite;
  vclrBackColor2 = clWhite;
  vclrForeColor2 = $00FF8000;

  vclrBackColor3 = $00FF8000;
  vclrForeColor3 = clYellow;
  vclrBackColor4 = clYellow;
  vclrForeColor4 = $00FF8000;

  vclrBackColor5 = clBlack;
  vclrForeColor5 = $005EAEFF;
  vclrBackColor6 = $005EAEFF;
  vclrForeColor6 = clBlack;

  vclrBackColor7 = clBlack;
  vclrForeColor7 = $0000B900;
  vclrBackColor8 = $0000B900;
  vclrForeColor8 = clBlack;

  vclrBackColor9 = clBlack;
  vclrForeColor9 = clWhite;
  vclrBackColor10 = clWhite;
  vclrForeColor10 = clBlack;

implementation

uses
  Main;

var
  vclrBackColor : TColor;
  vclrForeColor : TColor;

procedure SetColourScheme (vstrColourScheme : String);
begin

  gvstrCurrentColourScheme := vstrColourScheme;

{  frmMain.mnuConfigColourScheme1.Checked := False;
  frmMain.mnuConfigColourScheme2.Checked := False;
  frmMain.mnuConfigColourScheme3.Checked := False;
  frmMain.mnuConfigColourScheme4.Checked := False;
  frmMain.mnuConfigColourScheme5.Checked := False;
  frmMain.mnuConfigColourScheme6.Checked := False;
  frmMain.mnuConfigColourScheme7.Checked := False;
  frmMain.mnuConfigColourScheme8.Checked := False;
  frmMain.mnuConfigColourScheme9.Checked := False;
  frmMain.mnuConfigColourScheme10.Checked := False; }

  case vstrColourScheme of
    '1' : Begin
          vclrBackColor := vclrBackColor1;
          vclrForeColor := vclrForeColor1;
//          frmMain.mnuConfigColourScheme1.Checked := True;
        end;
    '2' : Begin
          vclrBackColor := vclrBackColor2;
          vclrForeColor := vclrForeColor2;
//          frmMain.mnuConfigColourScheme2.Checked := True;
        end;
    '3' : Begin
          vclrBackColor := vclrBackColor3;
          vclrForeColor := vclrForeColor3;
//          frmMain.mnuConfigColourScheme3.Checked := True;
        end;
    '4' : Begin
          vclrBackColor := vclrBackColor4;
          vclrForeColor := vclrForeColor4;
//          frmMain.mnuConfigColourScheme4.Checked := True;
        end;
    '5' : Begin
          vclrBackColor := vclrBackColor5;
          vclrForeColor := vclrForeColor5;
//          frmMain.mnuConfigColourScheme5.Checked := True;
        end;
    '6' : Begin
          vclrBackColor := vclrBackColor6;
          vclrForeColor := vclrForeColor6;
//          frmMain.mnuConfigColourScheme6.Checked := True;
        end;
    '7' : Begin
          vclrBackColor := vclrBackColor7;
          vclrForeColor := vclrForeColor7;
//          frmMain.mnuConfigColourScheme7.Checked := True;
        end;
    '8' : Begin
          vclrBackColor := vclrBackColor8;
          vclrForeColor := vclrForeColor8;
//          frmMain.mnuConfigColourScheme8.Checked := True;
        end;
    '9' : Begin
          vclrBackColor := vclrBackColor9;
          vclrForeColor := vclrForeColor9;
//          frmMain.mnuConfigColourScheme9.Checked := True;
        end;
    '10' : Begin
          vclrBackColor := vclrBackColor10;
          vclrForeColor := vclrForeColor10;
//          frmMain.mnuConfigColourScheme10.Checked := True;
        end;
  end;// case vstrColourScheme

  // Set up the colours now
  // LCD Panel
  frmMain.pnlLCD.color := vclrBackColor;
  // PTT Status
  frmMain.lblVHFPTT.color := vclrForeColor;
  frmMain.lblVHFPTT.font.color := vclrBackColor;
  frmMain.lblUHFPTT.color := vclrForeColor;
  frmMain.lblUHFPTT.font.color := vclrBackColor;
  // BY Status
  frmmain.lblUHFOnAirBusy.Font.Color := vclrForeColor;
  frmmain.lblVHFOnAirBusy.Font.Color := vclrForeColor;
  // Channel Name
  frmmain.lblUHFChannelName.Font.Color := vclrForeColor;
  frmmain.lblVHFChannelName.Font.Color := vclrForeColor;
  // Channel Number
  frmmain.lblUHFChannelNr.Font.Color := vclrForeColor;
  frmmain.lblVHFChannelNr.Font.Color := vclrForeColor;
  // TCT Status
  frmMain.lblUHFTCT.Font.Color := vclrForeColor;
  frmMain.lblVHFTCT.Font.Color := vclrForeColor;
  // Tone Freq
  frmMain.lblUHFTCTFreq.Font.Color := vclrForeColor;
  frmMain.lblVHFTCTFreq.Font.Color := vclrForeColor;
  // Data Source
  frmmain.lblUHFDataSource.Font.Color :=vclrForeColor;
  frmmain.lblVHFDataSource.Font.Color :=vclrForeColor;
  // DTSS Status
  frmMain.lblUHFDTSS.Font.Color := vclrForeColor;
  frmMain.lblVHFDTSS.Font.Color := vclrForeColor;
  // DTSS Code Status
  frmMain.lblUHFDTSSCode.Font.Color := vclrForeColor;
  frmMain.lblVHFDTSSCode.Font.Color := vclrForeColor;
  // Frequency
  frmMain.lblUHFFreq.Font.Color := vclrForeColor;
  frmMain.lblVHFFreq.Font.Color := vclrForeColor;
  // Reverse
  frmMain.lblUHFReverse.Font.Color := vclrForeColor;
  frmMain.lblVHFReverse.Font.Color := vclrForeColor;
  // RF Power
  frmMain.lblUHFRFPwr.Font.Color := vclrForeColor;
  frmMain.lblVHFRFPwr.Font.Color := vclrForeColor;
  // Shift
  frmmain.lblUHFShift.Font.Color :=vclrForeColor;
  frmmain.lblVHFShift.Font.Color :=vclrForeColor;

end;

end.// unit ColourSchemes;

