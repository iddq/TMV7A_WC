unit MEM;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Mem.pas
//
//  Calls: AppConstants
//         AppVariables
//         DataEntry
//         MEM_VHF : LoadVHFStringGrid
//                   SetVHFChannel
//         MEM_UHF : LoadJUHFStringGrid
//                   SetUHFChannel
//         MEM_DTMF : LoadDTMFStringGrid
//                   SetDTMFCode
//         Utilities : GetToneFrequencyFromToneNr
//
//  Called By: Main : TfrmMain.mnuMemVHFClick
//                    TfrmMain.mnuMemUHFClick
//                    TfrmMain.mnuMemDTMFClick
//
//  Ver: 1.0.0
//
//  Date: 11 Aug 2013
//
//========================================================================================

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Grids, Buttons, StdCtrls, Messages,
  // Application Units
  AppConstants, AppTypes, AppVariables, BCCommand, BUFCommand, DataEntry, LCDDisplay,
  MEM_DTMF, MEM_UHF, MEM_VHF, Utilities;

type

  TfrmMEM = class(TForm)
    bbtSelect: TBitBtn;
    bbtClose: TBitBtn;
    bbtEdit: TBitBtn;
    sgrVHF: TStringGrid;
    sgrUHF: TStringGrid;
    sgrDTMF: TStringGrid;
    procedure bbtCloseClick(Sender: TObject);
    procedure bbtEditClick(Sender: TObject);
    procedure bbtSelectClick(Sender: TObject);
    procedure Setup(vdrtDataRecType : TDataRecordType);
    procedure sgrUHFDblClick(Sender: TObject);
    procedure sgrUHFMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgrVHFDblClick(Sender: TObject);
    procedure sgrVHFMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    vdrtRecType : TDataRecordType;
  public
    { public declarations }
  end;

var
  frmMEM: TfrmMEM;

implementation

{$R *.lfm}

const

  cbytVHFColCOunt = 14;
  cbytUHFColCOunt = 14;
  cbytDTMFColCount = 2;

  cbytDTMFCodeCol = 1;

//========================================================================================
//          SUPPORT ROUTINES
//========================================================================================
procedure SetVHFUHFHeaders;

const
  cbytCHNrColWidth = 47;
  cstrCHNrColHdr = 'CH Nr';
  cbytNameColWidth = 160;
  cstrNameColHdr = '             CH Name';
  cbytRXFreqColWidth = 65;
  cstrRXFreqColHdr = ' RX Freq';
  cbytShiftColWidth = 58;
  cstrShiftColHdr = '   Shift';
  cbytOffsetColWidth = 58;
  cstrOffsetColHdr = 'Offset';
  cbytToneCTCSSColWidth = 50;
  cstrToneCTCSSColHdr = ' Tone';
  cbytToneFreqColWidth = 50;
  cstrToneFreqColHdr = ' Freq';
  cbytRFPwrColWidth = 65;
  cstrRFPwrColHdr = 'RF Pwr';
  cstrDTSSColWidth = 50;
  cstrDTSSDColHdr = ' DTSS';
  cstrDTSSCodeColWidth = 75;
  cstrDTSSDCodeColHdr = 'DTSS Code';
  cstrReverseColWidth = 60;
  cstrReverseColHdr = 'Reverse';
  cstrScanColWidth = 40;
  cstrScanColHdr = 'Scan';
  cstrStepColWidth = 40;
  cstrStepColHdr = 'Step';
  cstrCommentsColWidth = 375;
  cstrCommentsColHdr = '                                Comments';

begin

  // Channel Nr
  frmMem.sgrVHF.ColWidths[gcbytChMemNrCol] := cbytChNrColWidth;
  frmMEM.sgrVHF.Cells[gcbytChMemNrCol,0] := cstrCHNrColHdr;
  frmMem.sgrUHF.ColWidths[gcbytChMemNrCol] := cbytChNrColWidth;
  frmMEM.sgrUHF.Cells[gcbytChMemNrCol,0] := cstrCHNrColHdr;
  // Ch Name
  frmMem.sgrVHF.ColWidths[gcbytNameCol] := cbytNameColWidth;
  frmMEM.sgrVHF.Cells[gcbytNameCol,0] := cstrNameColHdr;
  frmMem.sgrUHF.ColWidths[gcbytNameCol] := cbytNameColWidth;
  frmMEM.sgrUHF.Cells[gcbytNameCol,0] := cstrNameColHdr;
  // RX Frequency
  frmMem.sgrVHF.ColWidths[gcbytRXFreqCol] := cbytRXFreqColWidth;
  frmMEM.sgrVHF.Cells[gcbytRXFreqCol,0] := cstrRXFreqColHdr;
  frmMem.sgrUHF.ColWidths[gcbytRXFreqCol] := cbytRXFreqColWidth;
  frmMEM.sgrUHF.Cells[gcbytRXFreqCol,0] := cstrRXFreqColHdr;
  // Shift
  frmMem.sgrVHF.ColWidths[gcbytShiftCol] := cbytShiftColWidth;
  frmMEM.sgrVHF.Cells[gcbytShiftCol,0] := cstrShiftColHdr;
  frmMem.sgrUHF.ColWidths[gcbytShiftCol] := cbytShiftColWidth;
  frmMEM.sgrUHF.Cells[gcbytShiftCol,0] := cstrShiftColHdr;
  // Offset
  frmMem.sgrVHF.ColWidths[gcbytOffsetCol] := cbytOffsetColWidth;
  frmMEM.sgrVHF.Cells[gcbytOffsetCol,0] := cstrOffsetColHdr;
  frmMem.sgrUHF.ColWidths[gcbytOffsetCol] := cbytOffsetColWidth;
  frmMEM.sgrUHF.Cells[gcbytOffsetCol,0] := cstrOffsetColHdr;
  // Tone CTCSS
  frmMem.sgrVHF.ColWidths[gcbytToneCTCSSCol] := cbytToneCTCSSColWidth;
  frmMEM.sgrVHF.Cells[gcbytToneCTCSSCol,0] := cstrToneCTCSSColHdr;
  frmMem.sgrUHF.ColWidths[gcbytToneCTCSSCol] := cbytToneCTCSSColWidth;
  frmMEM.sgrUHF.Cells[gcbytToneCTCSSCol,0] := cstrToneCTCSSColHdr;
  // Tone Freq
  frmMem.sgrVHF.ColWidths[gcbytToneCTCSSFreqCol] := cbytToneFreqColWidth;
  frmMEM.sgrVHF.Cells[gcbytToneCTCSSFreqCol,0] := cstrToneFreqColHdr;
  frmMem.sgrUHF.ColWidths[gcbytToneCTCSSFreqCol] := cbytToneFreqColWidth;
  frmMEM.sgrUHF.Cells[gcbytToneCTCSSFreqCol,0] := cstrToneFreqColHdr;
  // RF Powerr
  frmMem.sgrVHF.ColWidths[gcbytRFPowerCol] := cbytRFPwrColWidth;
  frmMEM.sgrVHF.Cells[gcbytRFPowerCol,0] := cstrRFPwrColHdr;
  frmMem.sgrUHF.ColWidths[gcbytRFPowerCol] := cbytRFPwrColWidth;
  frmMEM.sgrUHF.Cells[gcbytRFPowerCol,0] := cstrRFPwrColHdr;
  // DTSS
  frmMem.sgrVHF.ColWidths[gcbytDTSSCol] := cstrDTSSColWidth;
  frmMEM.sgrVHF.Cells[gcbytDTSSCol,0] := cstrDTSSDColHdr;
  frmMem.sgrUHF.ColWidths[gcbytDTSSCol] := cstrDTSSColWidth;
  frmMEM.sgrUHF.Cells[gcbytDTSSCol,0] := cstrDTSSDColHdr;
  // DTSS CODE
  frmMem.sgrVHF.ColWidths[gcbytDTSSCodeCol] := cstrDTSSCodeColWidth;
  frmMEM.sgrVHF.Cells[gcbytDTSSCodeCol,0] := cstrDTSSDCodeColHdr;
  frmMem.sgrUHF.ColWidths[gcbytDTSSCodeCol] := cstrDTSSCodeColWidth;
  frmMEM.sgrUHF.Cells[gcbytDTSSCodeCol,0] := cstrDTSSDCodeColHdr;
  // Reverse
  frmMem.sgrVHF.ColWidths[gcbytReverseCol] := cstrReverseColWidth;
  frmMEM.sgrVHF.Cells[gcbytReverseCol,0] := cstrReverseColHdr;
  frmMem.sgrUHF.ColWidths[gcbytReverseCol] := cstrReverseColWidth;
  frmMEM.sgrUHF.Cells[gcbytReverseCol,0] := cstrReverseColHdr;
  // SCAN
  frmMem.sgrVHF.ColWidths[gcbytScanCol] := cstrScanColWidth;
  frmMEM.sgrVHF.Cells[gcbytScanCol,0] := cstrScanColHdr;
  frmMem.sgrUHF.ColWidths[gcbytScanCol] := cstrScanColWidth;
  frmMEM.sgrUHF.Cells[gcbytScanCol,0] := cstrScanColHdr;
  // STEP
  frmMem.sgrVHF.ColWidths[gcbytStepCol] := cstrStepColWidth;
  frmMEM.sgrVHF.Cells[gcbytStepCol,0] := cstrStepColHdr;
  frmMem.sgrUHF.ColWidths[gcbytStepCol] := cstrStepColWidth;
  frmMEM.sgrUHF.Cells[gcbytStepCol,0] := cstrStepColHdr;
  // COMMENTS
  frmMem.sgrVHF.ColWidths[gcbytCommentCol] := cstrCommentsColWidth;
  frmMEM.sgrVHF.Cells[gcbytCommentCol,0] := cstrCommentsColHdr;
  frmMem.sgrUHF.ColWidths[gcbytCommentCol] := cstrCommentsColWidth;
  frmMEM.sgrUHF.Cells[gcbytCommentCol,0] := cstrCommentsColHdr;

end;// procedure SetVHFUHFHeaders;

//----------------------------------------------------------------------------------------
procedure SetDTMFHeaders;

const
  cbytChNrColWidth = 57;
  cbytNameColWidth = 80;

begin
  frmMem.sgrDTMF.ColWidths[gcbytChMemNrCol] := cbytChNrColWidth;
  frmMEM.sgrDTMF.Cells[gcbytChMemNrCol,0] := 'Mem Nr';
  frmMem.sgrDTMF.ColWidths[cbytDTMFCodeCol] := cbytNameColWidth;
  frmMEM.sgrDTMF.Cells[cbytDTMFCodeCol,0] := 'DTMF Code';

end;// procedure SetDTMFHeaders

//========================================================================================
//          FORM ROUTINES
//========================================================================================
procedure TfrmMEM.Setup(vdrtDataRecType : TDataRecordType);

Const
  cbytVHFUHFTop = 10;
  cbytVHFUHFLeft = 10;
  cbytVHFUHFWidth = 675;
  cbytVHFUHFHeight = 325;
  cbytDTMFTop = 60;
  cbytDTMFLeft = 260;
  cbytDTMFWidth = 157;
  cbytDTMFHeight = 246;

var
  vbytTemp : Byte;

begin

  sgrVHF.Visible := False;
  sgrUHF.Visible := False;
  sgrDTMF.Visible := False;

  vdrtRecType := vdrtDataRecType;

  case vdrtDataRecType of
    drtVHFMEM : begin
                 Caption := 'VHF Data';
                 sgrVHF.Top := cbytVHFUHFTop;
                 sgrVHF.Left := cbytVHFUHFLeft;
                 sgrVHF.Width := cbytVHFUHFWidth;
                 sgrVHF.Height := cbytVHFUHFHeight;
                 sgrVHF.ColCount := cbytVHFColCount;
                 sgrVHF.RowCount := gcbytMaxVHFChannels+1;
                 SetVHFUHFHeaders;
                 LoadVHFStringGrid;
                 sgrVHF.Row:=1;
                 sgrVHF.Visible := True;
               end;
    drtUHFMEM : begin
                 Caption := 'UHF Data';
                 sgrUHF.Top := cbytVHFUHFTop;
                 sgrUHF.Left := cbytVHFUHFLeft;
                 sgrUHF.Width := cbytVHFUHFWidth;
                 sgrUHF.Height := cbytVHFUHFHeight;
                 sgrUHF.ColCount := cbytUHFColCOunt;
                 sgrUHF.RowCount := gcbytMaxUHFChannels+1;
                 SetVHFUHFHeaders;
                 LoadUHFStringGrid;
                 sgrUHF.Row:=1;
                 sgrUHF.Visible := True;
               end;
    drtDTMF : begin
                  Caption := 'DTMF Data';
                  sgrDTMF.Top := cbytDTMFTop;
                  sgrDTMF.Left := cbytDTMFLeft;
                  sgrDTMF.Width := cbytDTMFWidth;
                  sgrDTMF.Height := cbytDTMFHeight;
                  sgrDTMF.ColCOunt := cbytDTMFColCOunt;
                  sgrDTMF.RowCount := gcbytMaxDTMFCodes+1;
                  SetDTMFHeaders;
                  LoadDTMFStringGrid;
                  sgrDTMF.Visible := True;
                end;
    end;// case vdrtDataRecType

  ShowModal;

end;// procedure TfrmMEM.Setup

//========================================================================================
//          BUTTON ROUTINES
//========================================================================================
procedure TfrmMEM.bbtEditClick(Sender: TObject);
begin

  case vdrtRecType of
    drtVHFMEM : frmDataEntry.vdetDataEntryType := detVHFMEM;
    drtUHFMEM : frmDataEntry.vdetDataEntryType := detUHFMEM;
 //   drtDTMF :
  end;

  if gvintSelectedRow = 0 then gvintSelectedRow := 1;
  frmDataEntry.vbytChannelNumber := gvintSelectedRow;
  frmDataEntry.ShowModal;

end;// procedure TfrmMEM.bbtEditClick

//----------------------------------------------------------------------------------------
procedure TfrmMEM.bbtSelectClick(Sender: TObject);
begin
  SetVHFChannel;
  ModalResult := mrClose;
end;// procedure TfrmMEM.bbtSelectClick

//----------------------------------------------------------------------------------------
procedure TfrmMEM.bbtCloseClick(Sender: TObject);
begin

end;// procedure TfrmMEM.bbtCloseClick

//========================================================================================
//          MOUSE ROUTINES
//========================================================================================
procedure TfrmMEM.sgrUHFMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  vintCol, vintRow : Longint;

begin

    sgrUHF.MouseToCell(X, Y, vintCol, vintRow);
    if vintRow > 0 then
    begin
      gvintSelectedRow := vintRow;
    end;// if vintRow > 0

end;// procedure TfrmMEM.sgrUHFMouseUp

//----------------------------------------------------------------------------------------
procedure TfrmMEM.sgrUHFDblClick(Sender: TObject);
begin
  SetUHFChannel;
  ModalResult := mrClose;
end;// procedure TfrmMEM.sgrUHFDblClick

//----------------------------------------------------------------------------------------
procedure TfrmMEM.sgrVHFMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  vintCol, vintRow : Longint;

begin

  sgrVHF.MouseToCell(X, Y, vintCol, vintRow);
  if vintRow > 0 then
  begin
    gvintSelectedRow := vintRow;
  end;// if vintRow > 0

end;// procedure TfrmMEM.sgrVHFMouseUp

//----------------------------------------------------------------------------------------
procedure TfrmMEM.sgrVHFDblClick(Sender: TObject);
begin
    SetVHFChannel;
    ModalResult := mrClose;
end;

//========================================================================================
end.// unit MEM

