unit INIStuff;

{$mode objfpc}{$H+}

//========================================================================================
//
//  INIStuff.pas
//
//  This unit provides all INI file functionality.
//
//  Calls: AppConstants
//         Main
//         Register
//
//  Called By: Init : Initialize
//             Final : Finalize
//
//  Ver: 1.0.0
//
//  Date: 21 Dec 2013
//
//========================================================================================

interface

uses
  Classes, SysUtils, INIFiles, Dialogs,
  // Application units
  AppConstants, AppVariables, Register;

var
  INIFile : TINIFile;

procedure ReadINIFile;
procedure WriteINIFile;

implementation

uses
  Main;

const
  cstrSectionCONFIG = 'CONFIG';
    cstrKeyCOMPort = 'COM Port';
    cstrKeyColourScheme = 'Colour Scheme';
  cstrSectionTMVFILES = 'TMV FILES';
    cstrKeyTMVFileName = 'TMVFileName';
  cstrSectionUHF = 'UHF';
    cstrKeyUHFDataSource = 'UHF Data Source';
    cstrKeyUHFRxFrequency = 'UHF RX Frequency';
    cstrKeyUHFStep = 'UHF Step';
    cstrKeyUHFShift = 'UHF Shift';
    cstrKeyUHFReverse = 'UHF Reverse';
    cstrKeyUHFTone = 'UHF Tone';
    cstrKeyUHFCTCSS = 'UHF CTCSS';
    cstrKeyUHFDTSS = 'UHF DTSS';
    cstrKeyUHFToneNr = 'UHF Tone Nr';
    cstrKeyUHFDTSSCode = 'UHF DTSS Code';
    cstrKeyUHFCTCSSNr = 'UHF CTCSS Nr';
    cstrKeyUHFOffset = 'UHF Offset';
    cstrKeyUHFScan = 'UHF Scan';
    cstrKeyUHFSplitFreq = 'UHF Split Freq';
    cstrKeyUHFSplitStep = 'UHF Split Step';
    cstrKeyUHFRFPower = 'UHF RF Power';
    cstrKeyUHFChannelName = 'UHF Channel Name';
    cstrKeyUHFComments = 'UHF Comments';
    cstrKeyUHFAudioLevel = 'UHF Audio Level';
    cstrKeyUHFSquelchLevel = 'UHF Squelch Level';
    cstrKeyUHFReverseState = 'UHF Reverse State';
    cstrKeyUHFChannelNumber = 'UHF Channel Number';
  cstrSectionVHF = 'VHF';
    cstrKeyVHFDataSource = 'VHF Data Source';
    cstrKeyVHFRxFrequency = 'VHF RX Frequency';
    cstrKeyVHFStep = 'VHF Step';
    cstrKeyVHFShift = 'VHF Shift';
    cstrKeyVHFReverse = 'VHF Reverse';
    cstrKeyVHFTone = 'VHF Tone';
    cstrKeyVHFCTCSS = 'VHF CTCSS';
    cstrKeyVHFDTSS = 'VHF DTSS';
    cstrKeyVHFToneNr = 'VHF Tone Nr';
    cstrKeyVHFDTSSCode = 'VHF DTSS Code';
    cstrKeyVHFCTCSSNr = 'VHF CTCSS Nr';
    cstrKeyVHFOffset = 'VHF Offset';
    cstrKeyVHFScan = 'VHF Scan';
    cstrKeyVHFSplitFreq = 'VHF Split Freq';
    cstrKeyVHFSplitStep = 'VHF Split Step';
    cstrKeyVHFRFPower = 'VHF RF Power';
    cstrKeyVHFChannelName = 'VHF Channel Name';
    cstrKeyVHFComments = 'VHF Comments';
    cstrKeyVHFAudioLevel = 'VHF Audio Level';
    cstrKeyVHFSquelchLevel = 'VHF Squelch Level';
    cstrKeyVHFReverseState = 'VHF Reverse State';
    cstrKeyVHFChannelNumber = 'VHF Channel Number';
  cstrSectionLCDDisplay = 'LCD DISPLAY';
    cstrKeyPTTBand = 'PTT Band';
  cstrSectionColourSchemes = 'COLOUR SCHEMES';
    cstrKeyCurrentColourScheme = 'Current COlour Scheme';
  cstrSectionRegistration = 'REGISTRATION';
    cstrKeyRegistered = 'Registered';
    cstrKeyRegisteredName = 'Registered Name';
    cstrKeyRegisteredCall = 'Registered Call';
    cstrKeyRegistrationKey = 'Registration Key';
  cstrSectionRegistrationDialog = 'REGISTRATION DIALOG';
    cstrKeyRegistrationDialogTop = 'Top';
    cstrKeyRegistrationDialogLeft = 'Left';
  cstrSectionMainForm = 'MAIN FORM';
    cstrKeyMainFormTop = 'Top';
    cstrKeyMainFormLeft = 'Left';


var
  vstrINIFileName : string;

procedure ReadINIFile;

var
  vstrTstr : string;

begin

  vstrINIFileName := gvstrAppPath + gcstrAppName + '.ini';
  INIFile := TINIFile.Create(vstrINIFileName);

  // CONFIG SECTION
  gvstrCOMPort := INIFile.Readstring (cstrSectionCONFIG, cstrKeyCOMPort, gcstrDefCOMPort);

  // TMVFILES SECTION
  gvstrTMVFileName := INIFile.ReadString (cstrSectionTMVFILES, cstrKeyTMVFileName, '');

  // UHF CHANNEL SECTION
  gvstrUHFDataSource := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFDataSource, gcstrUHFDefDataSource);
  gvstrUHFRXFrequency := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFRxFrequency, gcstrDefUHFRXFrequency);
  gvstrUHFStep := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFStep, gcstrDefUHFStep);
  gvstrUHFShift := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFShift, gcstrDefUHFShift);
  gvstrUHFReverse := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFReverse, gcstrDefUHFReverse);
  gvstrUHFTone := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFTone, gcstrDefUHFTone);
  gvstrUHFCTCSS := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFCTCSS, gcstrDefUHFCTCSS);
  gvstrUHFDTSS := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFDTSS, gcstrDefUHFDTSS);
  gvstrUHFToneNr := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFToneNr, gcstrDefUHFToneNr);
  gvstrUHFDTSSCode := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFDTSSCode, gcstrDefUHFDTSSCode);
  gvstrUHFCTCSSNr := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFCTCSSNr, gcstrDefUHFCTCSSNr);
  gvstrUHFOffset := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFOffset, gcstrDefUHFOffset);
  gvstrUHFScan := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFScan, gcstrDefUHFScan);
  gvstrUHFSplitFrequency := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFSplitFreq, gcstrDefUHFSplitFrequency);
  gvstrUHFSplitStep := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFSplitStep, gcstrDefUHFSplitStep);
  gvstrUHFRFPower := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFRFPower, gcstrDefUHFRFPower);
  gvstrUHFChannelName := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFChannelName, gcstrDefUHFChannelName);
  gvstrUHFChannelComments := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFComments, gcstrDefUHFChannelComments);
  gvstrUHFAudioLevel := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFAudioLevel, gcstrDefUHFAudioLevel);
  gvstrUHFSquelchLevel := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFSquelchLevel, gcstrDefUHFSquelchLevel);
  gvstrUHFReverseState := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFReverseState, gcstrDefUHFReverseState);
  gvstrUHFChannelNr := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyUHFChannelNumber, '');

     // VHF CHANNEL SECTION
  gvstrVHFDataSource := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFDataSource, gcstrVHFDefDataSource);
  gvstrVHFRXFrequency := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFRxFrequency, gcstrDefVHFRXFrequency);
  gvstrVHFStep := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFStep, gcstrDefVHFStep);
  gvstrVHFShift := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFShift, gcstrDefVHFShift);
  gvstrVHFReverse := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFReverse, gcstrDefVHFReverse);
  gvstrVHFTone := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyVHFTone, gcstrDefVHFTone);
  gvstrVHFCTCSS := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFCTCSS, gcstrDefVHFCTCSS);
  gvstrVHFDTSS := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFDTSS, gcstrDefVHFDTSS);
  gvstrVHFToneNr := INIFile.ReadString (cstrSectionUHF,
                        cstrKeyVHFToneNr, gcstrDefVHFToneNr);
  gvstrVHFDTSSCode := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFDTSSCode, gcstrDefVHFDTSSCode);
  gvstrVHFCTCSSNr := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFCTCSSNr, gcstrDefVHFCTCSSNr);
  gvstrVHFOffset := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFOffset, gcstrDefVHFOffset);
  gvstrVHFScan := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFScan, gcstrDefVHFScan);
  gvstrVHFSplitFrequency := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFSplitFreq, gcstrDefVHFSplitFrequency);
  gvstrVHFSplitStep := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFSplitStep, gcstrDefVHFSplitStep);
  gvstrVHFRFPower := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFRFPower, gcstrDefVHFRFPower);
  gvstrVHFChannelName := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFChannelName, gcstrDefVHFChannelName);
  gvstrVHFChannelComments := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFComments, gcstrDefVHFChannelComments);
  gvstrVHFAudioLevel := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFAudioLevel, gcstrDefVHFAudioLevel);
  gvstrVHFSquelchLevel := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFSquelchLevel, gcstrDefVHFSquelchLevel);
  gvstrVHFReverseState := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFReverseState, gcstrDefVHFReverseState);
  gvstrVHFChannelNr := INIFile.ReadString (cstrSectionVHF,
                        cstrKeyVHFChannelNumber, '');

  // LCD DISPLAY SECTION
  gvstrPTTBand := INIFile.ReadString (cstrSectionLCDDisplay,
                        cstrKeyPTTBand, gcstrVHF);

  // COLOUR SCHEMES SECTION
  gvstrCurrentColourScheme := INIFile.ReadString (cstrSectionColourSchemes,
                        cstrKeyCurrentColourScheme, gcstrDefaultColourScheme);

  // REGISTRATION SECTION
  gvblnRegistered := INIFile.ReadBool (cstrSectionRegistration,
                        cstrKeyRegistered, False);

  gvstrRegisteredName := INIFile.ReadString (cstrSectionRegistration,
                        cstrKeyRegisteredName, '');

  gvstrRegisteredCall := INIFile.ReadString (cstrSectionRegistration,
                        cstrKeyRegisteredCall, '');

  gvstrRegistrationKey := INIFile.ReadString (cstrSectionRegistration,
                        cstrKeyRegistrationKey, '');

  // REGISTRATION DIALOG
  dlgRegister.Top := INIFile.ReadInteger (cstrSectionRegistrationDialog,
                     cstrKeyRegistrationDialogTop, dlgRegister.Top);
  dlgRegister.Left := INIFile.ReadInteger (cstrSectionRegistrationDialog,
                     cstrKeyRegistrationDialogLeft, dlgRegister.Left);

  // MAIN FORM
  frmMain.Top := INIFile.ReadInteger (cstrSectionMainForm,
                     cstrKeyMainFormTop, frmMain.Top);
  frmMain.Left := INIFile.ReadInteger (cstrSectionMainForm,
                     cstrKeyMainFormLeft, frmMain.Left);

end;//procedure ReadINIFile

procedure WriteINIFile;
begin

   // CONFIG SECTION
   INIFile.WriteString (cstrSectionCONFIG, cstrKeyCOMPort, gvstrCOMPort);

   // TMVFILES SECTION
   INIFile.WriteString (cstrSectionTMVFILES, cstrKeyTMVFileName, gvstrTMVFileName);

  // UHF SECTION
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFDataSource, gvstrUHFDataSource);
//  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFChannelNr, gvstrUHFChannelNr);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFRxFrequency, gvstrUHFRXFrequency);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFStep, gvstrUHFStep);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFShift, gvstrUHFShift);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFReverse, gvstrUHFReverse);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFTone, gvstrUHFTone);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFCTCSS, gvstrUHFCTCSS);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFDTSS, gvstrUHFDTSS);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFToneNr, gvstrUHFToneNr);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFDTSSCode, gvstrUHFDTSSCode);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFCTCSSNr, gvstrUHFCTCSSNr);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFOffset, gvstrUHFOffset);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFScan, gvstrUHFScan);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFSplitFreq, gvstrUHFSplitFrequency);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFSplitStep, gvstrUHFSplitStep);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFRFPower, gvstrUHFRFPower);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFChannelName, gvstrUHFChannelName);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFComments, gvstrUHFChannelComments);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFAudioLevel, gvstrUHFAudioLevel);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFSquelchLevel, gvstrUHFSquelchLevel);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFReverseState, gvstrUHFReverseState);
  INIFile.WriteString (cstrSectionUHF, cstrKeyUHFChannelNumber, gvstrUHFChannelNr);

    // VHF SECTION
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFDataSource, gvstrVHFDataSource);
//  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFChannelNr, gvstrVHFChannelNr);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFRxFrequency, gvstrVHFRXFrequency);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFStep, gvstrVHFStep);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFShift, gvstrVHFShift);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFReverse, gvstrVHFReverse);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFTone, gvstrVHFTone);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFCTCSS, gvstrVHFCTCSS);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFDTSS, gvstrVHFDTSS);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFToneNr, gvstrVHFToneNr);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFDTSSCode, gvstrVHFDTSSCode);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFCTCSSNr, gvstrVHFCTCSSNr);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFOffset, gvstrVHFOffset);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFScan, gvstrVHFScan);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFSplitFreq, gvstrVHFSplitFrequency);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFSplitStep, gvstrVHFSplitStep);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFRFPower, gvstrVHFRFPower);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFChannelName, gvstrVHFChannelName);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFComments, gvstrVHFChannelComments);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFAudioLevel, gvstrVHFAudioLevel);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFSquelchLevel, gvstrVHFSquelchLevel);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFReverseState, gvstrVHFReverseState);
  INIFile.WriteString (cstrSectionVHF, cstrKeyVHFChannelNumber, gvstrVHFChannelNr);

  // LCD DISPLAY SECTION
  INIFile.WriteString (cstrSectionLCDDisplay, cstrKeyPTTBand, gvstrPTTBand);

  // COLOUR SCHEMES SECTION
  INIFile.WriteString (cstrSectionColourSchemes, cstrKeyCurrentColourScheme,
                                                 gvstrCurrentColourScheme);

  // REGISTRATION SECTION
  INIFile.WriteBool (cstrSectionRegistration, cstrKeyRegistered,
                                                 gvblnRegistered);

  INIFile.WriteString (cstrSectionRegistration, cstrKeyRegisteredName,
                                                 gvstrRegisteredName);

  INIFile.WriteString (cstrSectionRegistration, cstrKeyRegisteredCall,
                                                 gvstrRegisteredCall);

  INIFile.WriteString (cstrSectionRegistration, cstrKeyRegistrationKey,
                                                 gvstrRegistrationKey);

    // REGISTRATION DIALOG
  INIFile.WriteInteger (cstrSectionRegistrationDialog, cstrKeyRegistrationDialogTop,
                                                   dlgRegister.Top);
  INIFile.WriteInteger (cstrSectionRegistrationDialog, cstrKeyRegistrationDialogLeft,
                                                   dlgRegister.Left);

  // MAIN FORM
INIFile.WriteInteger (cstrSectionMainForm, cstrKeyMainFormTop,
                                                 frmMain.Top);
INIFile.WriteInteger (cstrSectionMainForm, cstrKeyMainFormLeft,
                                                 frmMain.Left);

  INIFile.UpdateFile;
  INIFile.Free;

end;// procedure WriteINIFile

end.// unit INIStuff

