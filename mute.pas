unit Mute;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Mute.pas
//
//  Calls: AGCommand : SetUHF_AGValue
//                     SetVHF_AGValue
//         AppConstants
//         AppVariables
//         Main
//
//  Called By: Init : Initialize
//             Main : TfrmMain.bbtMuteClick
//
//  Ver: 1.0.0
//
//  Date: 19 Jul 2013
//
//========================================================================================

interface

uses
  Classes, Graphics, SysUtils,
  // Application Unit
  AGCommand, AppConstants, AppVariables;

procedure ToggleMute;
procedure SetMuteOn;
procedure SetMuteOff;

implementation

uses
    Main;

var
  vstrOriginalUHFAudioLevel : String;
  vstrOriginalVHFAudioLevel : String;

//========================================================================================
procedure SetMuteOn;
begin

  vstrOriginalUHFAudioLevel := gvstrUHFAudioLevel;
  vstrOriginalVHFAudioLevel := gvstrVHFAudioLevel;
  gvstrUHFAudioLevel := '00';
  gvstrVHFAudioLevel := '00';
  SetUHF_AGValue(gvstrUHFAudioLevel);
  SetVHF_AGValue(gvstrVHFAudioLevel);
  frmMain.bbtMute.Font.Color := clRed;
  frmMain.bbtMute.Font.Style := [fsBold];
  frmMain.uekUHFVolume.Transparent:= False;
  frmMain.uekVHFVolume.Transparent:= False;
  gvblnMute := gcblnOn;

end;// procedure SetMuteOn;

//========================================================================================
procedure SetMuteOff;
begin

  gvstrUHFAudioLevel := vstrOriginalUHFAudioLevel;
  gvstrVHFAudioLevel := vstrOriginalVHFAudioLevel;
  SetUHF_AGValue(gvstrUHFAudioLevel);
  SetVHF_AGValue(gvstrVHFAudioLevel);
  frmMain.bbtMute.Font.Color := clBlack;
  frmMain.bbtMute.Font.Style := [];
  frmMain.uekUHFVolume.Transparent:= True;
  frmMain.uekVHFVolume.Transparent:= True;
  gvblnMute := gcblnOff;

end;// procedure SetMuteOff

//========================================================================================
procedure ToggleMute;
begin

  if gvblnMute = gcblnOn then
      SetMuteOff
  else
     SetMuteOn;

end;//procedure TogglePTTBand

//========================================================================================

end.// unit Mute

