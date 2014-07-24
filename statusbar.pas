unit StatusBar;

{$mode objfpc}{$H+}

//========================================================================================
//
//  StatusBar.pas
//
//  Calls: Main
//         AppConstants
//         AppVariables
//
//  Called By: Main : TfrmMain.FormShow
//                    TfrmMain.tmrTimeStatusTimer
//             TMVFiles : OpenTMVFile
//
//  Ver: 1.0.0
//
//  Date: 11 Dec 2013
//
//========================================================================================

interface

uses
  Classes, SysUtils, Dialogs,
  // Application units
  AppConstants, AppVariables;

procedure DisplayCOMPortStatus;
procedure DisplayDateStatus;
procedure DisplayTimeStatus;
procedure DisplayTMVFileStatus;
procedure DisplayCommentStatus;
procedure DisplayTMV7Status;

const
  gcbytDateStatusPanel = 0;
  gcbytTimeStatusPanel = 1;
  gcbytCOMPortStatusPanel = 2;
  gcbytTMVFileStatusPanel = 3;
  gcbytCommentStatusPanel = 4;

implementation

uses
  // Application units
  Main;

//========================================================================================
//           SUPPORT ROUTINES
//========================================================================================
procedure TMV7PanelStatus ( vblnOnLine : Boolean );
begin
  if vblnOnLine then
  begin
    // Menus
    frmMain.mnuVFOVHF.Enabled := True;
    frmMain.mnuVFOUHF.Enabled := True;
    // Tool Buttons
    // Controls
    frmMain.uekVHFVolume.Enabled := True;
    frmMain.uekVHFSquelch.Enabled := True;
    frmMain.uekUHFVolume.Enabled := True;
    frmMain.uekUHFSquelch.Enabled := True;
    frmMain.updVHF.Enabled := True;
    frmMain.updUHF.Enabled := True;
    frmMain.btnBand.Enabled := True;
    frmMain.bbtReverse.Enabled := True;
    frmMain.bbtMute.Enabled := True;
    frmMain.btnRFPower.Enabled := True;
    frmMain.btnPower.Enabled := True;
  end
  else
  begin
    // Menus
    frmMain.mnuVFOVHF.Enabled := False;
    frmMain.mnuVFOUHF.Enabled := False;
    // Tool Buttons
    // Controls
    frmMain.uekVHFVolume.Enabled := False;
    frmMain.uekVHFSquelch.Enabled := False;
    frmMain.uekUHFVolume.Enabled := False;
    frmMain.uekUHFSquelch.Enabled := False;
    frmMain.updVHF.Enabled := False;
    frmMain.updUHF.Enabled := False;
    frmMain.btnBand.Enabled := False;
    frmMain.bbtReverse.Enabled := False;
    frmMain.bbtMute.Enabled := False;
    frmMain.btnRFPower.Enabled := False;
    frmMain.btnPower.Enabled := False;
  end;// if vblnOnLine
end;// procedure TMV7PanelStatus

//========================================================================================
//          STATUS ROUTINES
//========================================================================================
procedure DisplayCOMPortStatus;
begin

  if gvstrCOMPort = gcstrDefCOMPort then
    frmMain.StatusBar1.Panels[gcbytCOMPortStatusPanel].text := 'None'
  else
  begin
    if frmMain.sdpoSerial1.Active then
    begin
      frmMain.StatusBar1.Panels[gcbytCOMPortStatusPanel].text := gvstrCOMPort + ' - Open';
      frmMain.mnuConfigSelectPort.Enabled := False;
      frmMain.mnuConfigOpenPort.Enabled := False;
      frmMain.mnuConfigClosePort.Enabled := True;;
    end
    else
    begin
      frmMain.StatusBar1.Panels[gcbytCOMPortStatusPanel].text := gvstrCOMPort + ' - Closed';
      frmMain.mnuConfigSelectPort.Enabled := True;
      frmMain.mnuConfigOpenPort.Enabled := True;
      frmMain.mnuConfigClosePort.Enabled := False;;
    end;// if frmMain.sdpoSerial1.Active
  end;// if gvstrCOMPort = gcstrDefCOMPort

end;// procedure DisplayCOMPortStatus;

//========================================================================================

procedure DisplayDateStatus;
begin
  frmMain.StatusBar1.Panels[gcbytDateStatusPanel].text := FormatDateTime ( 'dd mmm yyyy', Now );
end;// procedure DisplayDateStatus;

//========================================================================================

procedure DisplayTimeStatus;
begin
  frmMain.StatusBar1.Panels[gcbytTimeStatusPanel].text := FormatDateTime ( 'hhmm:ss', Now );
end;//procedure DisplayTimeStatus;

//========================================================================================
procedure DisplayTMVFileStatus;
begin
  frmMain.StatusBar1.Panels[gcbytTMVFileStatusPanel].text :=
                                                   ExtractFileName(gvstrTMVFileName);
end;// procedure DisplayTMVFileStatus;

//========================================================================================
//========================================================================================
procedure DisplayCommentStatus;

var
  vstrTStr : string;
begin

  if gvstrPTTBand = gcstrVHF then
    vstrTStr := gvstrVHFChannelComments
  else
    vstrTStr := gvstrVHFChannelComments;

  frmMain.StatusBar1.Panels[gcbytCommentStatusPanel].text := vstrTStr;

end;// procedure DisplayCommentFileStatus;

//========================================================================================
procedure DisplayTMV7Status;
begin
  if gvblnTMV7OnLine then
  begin
    frmMain.stxOffLine.Visible := False;
    TMV7PanelStatus(True);
  end
  else
  begin
    frmMain.stxOffLine.Visible := True;
    TMV7PanelStatus(False);
  end;// if gvblnTMV7OnLine
end;//procedure DisplayTMV7Status;

//========================================================================================
end.// unit StatusBar

