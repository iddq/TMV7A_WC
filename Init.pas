unit Init;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Init.pas
//
//  Calls:  AGCommand : SetVHF_AGValue
//                      SetVHF_AGValue
//         AppConstants
//         BCCommand : DisplayBCStatus
//         BUFCommand : SetBuffer
//         BYCommand : GetUHFBYStatus
//                     GetVHFBYStatus
//         ColourSchemse : SetColourScheme
//         Configure : frmConfigure.Show
//         HUtils : InfoMessageDlgOk
//         INIStuff : ReadINIFile
//                    Variables
//         LCDDisplay : LCDOn
//                      LCDOff
//                      DisplayBCStatus
//                      DisplayVHFDataSource
//                      DisplayUHFDataSource
//         Main
//         NagScreen
//         PSCommand : TogglePowerOnOff
//         SerialStuff : OpenPort
//                       SendCommand
//         SMCommand : GetVHF_SMValue
//                     GetUHF_SMValue
//         SQCommand : SetVHF_SQValue
//                     SetUHF_SQValue
//         StatusBar : DisplayTMVFileStatus
//         TMVFiles : OpenTMVFile
//                    NewTMVFile
//         Variables
//
//  Called By: Main : TfrmMain.FormShow
//
//  Ver: 1.0.0
//
//  Date: 31 Aug 2014
//
//========================================================================================

interface

uses
  Classes, Forms, SysUtils, Dialogs, HUtils,
  // Application units
  AGCommand, AppConstants, AppVariables, BUFCommand, BYCommand, ColourSchemes, {COMPort,}
  Configure, DataEntry, INIStuff, LCDDisplay, NagScreen, SerialStuff, SMCommand,
  SQCommand, StatusBar, TMVFiles;

function Initialize : boolean;
procedure TMV7_Init;

implementation

uses
  Main;

//========================================================================================
//         INITIALIZE THE TONE FREQUENCY ARRAY
//========================================================================================

Procedure InitToneFrequencyArray;
Begin
  gvstrToneArray[0] := '67.0';
  gvstrToneArray[1] := '71.9';
  gvstrToneArray[2] := '74.4';
  gvstrToneArray[3] := '77.0';
  gvstrToneArray[4] := '79.7';
  gvstrToneArray[5] := '82.5';
  gvstrToneArray[6] := '85.4';
  gvstrToneArray[7] := '88.5';
  gvstrToneArray[8] := '91.5';
  gvstrToneArray[9] := '94.8';
  gvstrToneArray[10] := '97.4';
  gvstrToneArray[11] := '100.0';
  gvstrToneArray[12] := '103.5';
  gvstrToneArray[13] := '107.2';
  gvstrToneArray[14] := '110.9';
  gvstrToneArray[15] := '114.8';
  gvstrToneArray[16] := '118.8';
  gvstrToneArray[17] := '123.0';
  gvstrToneArray[18] := '127.3';
  gvstrToneArray[19] := '131.8';
  gvstrToneArray[20] := '136.5';
  gvstrToneArray[21] := '141.3';
  gvstrToneArray[22] := '146.2';
  gvstrToneArray[23] := '151.4';
  gvstrToneArray[24] := '156.7';
  gvstrToneArray[25] := '162.2';
  gvstrToneArray[26] := '167.9';
  gvstrToneArray[27] := '173.8';
  gvstrToneArray[28] := '179.9';
  gvstrToneArray[29] := '186.2';
  gvstrToneArray[30] := '192.8';
  gvstrToneArray[31] := '203.5';
  gvstrToneArray[32] := '210.7';
  gvstrToneArray[33] := '218.1';
  gvstrToneArray[34] := '225.7';
  gvstrToneArray[35] := '233.6';
  gvstrToneArray[36] := '241.8';
  gvstrToneArray[37] := '250.3';
End;// InitToneFrequencyArray

//========================================================================================
//        INITIALIZE STEP VALUE ARRAY
//========================================================================================

Procedure InitStepArray;
Begin
  { gvstrStepArray[0] := '5';
   gvstrStepArray[1] := '6.25';
   gvstrStepArray[2] := '10';
   gvstrStepArray[3] := '12.5';
   gvstrStepArray[4] := '15';
   gvstrStepArray[5] := '20';
   gvstrStepArray[6] := '25';
   gvstrStepArray[7] := '30';
   gvstrStepArray[8] := '50'; }

 End;// InitStepArray

//========================================================================================
//          INITIALIZE
//========================================================================================
function Initialize : boolean;

var
  vbytTemp : Byte;
  Info : TSearchRec;

begin

  // Get and save the Application Path
  GetDir(0, gvstrAppPath);
  gvstrAppPath := gvstrAppPath + '\';

  // Load the INI data variables
  ReadINIFile;

  // Check to see if the TMV7Files directory exists. If it doesn't, we create it
  if not DirectoryExists (gcstrTMV7FileDir) then
  begin
    // The directory does not exist so we assume that this is the initial program
    // installation so we display an information message and attempt to create it.
    // If it cannot be created, we display an error message, stop the Initialization
    // process and return False;
    InfoMessageDlgOk('Initial Installation', 'Creating TMV File Folder');
    if not CreateDir (gcstrTMV7FileDir)then
    begin
      MessageDlg('TMVFiles Directory creation failed.', mtError, [mbOk], 0 );
      Result := False;
      Exit;
    end;//if not CreateDir (gcstrTMV7FileDir

    // Now we do the initial program confoguration
    frmConfigure.ShowModal;

  end;//if not DirectoryExists (gcstrTMV7FileDir)

{  // Load the INI data variables
  ReadINIFile; }

  // Init Tones
  InitToneFrequencyArray;

  // Init Steps
  InitStepArray;

  // Load the Tone Combobox items
//  for vbytTemp := 0 to gcbytMaxToneIndex do
//    frmDataEntry.cbxTones.Items.Add(gvstrToneArray[vbytTemp]);

  // Read the TMV File. If a previous .tmv file was open, then we simply reopen it.
  // If no .tmv7 file was open and a .tmv file exists, then we are prompted to open it.
  // If there is no existing .tmv file we create and open a default file.
  if FileExists(gvstrTMVFileName) then
  begin
    // A .tmv file was open so we simply open and read it
    OpenTMVFile(gvstrTMVFileName);
  end
  else
  begin
    // No file was open so we first check to see if one exists
    if FindFirst(gvstrAppPath + '\' + gcstrTMV7FileDir + '\*.' +
                 gcstrTMV7FileExt, faAnyFile, info) = 0 then
    begin
      ShowMessage('Select a .tmv file.');
      // A .tmv file exists so we prompt to open it. If the Open dialog is cancelled,
      // we create a default file.
      frmMain.OpenDialog1.Title := 'Open TMV File';
      frmMain.OpenDialog1.InitialDir := gvstrAppPath + gcstrTMV7FileDir;
      frmMain.OpenDialog1.Filter := 'TMVFiles|*.tmv';
      frmMain.OpenDialog1.DefaultExt := 'tmv';

      if frmMain.OpenDialog1.Execute then
      begin
        OpenTMVFile(frmMain.OpenDialog1.Filename);
      end
      else
      begin
        if FileExists(gvstrAppPath + gcstrTMV7FileDir + '\Default.' + gcstrTMV7FileExt) then
        begin
          ShowMessage('Selection cancelled. Using Default.tmv.');
          gvstrTMVFileName := gvstrAppPath + gcstrTMV7FileDir + '\Default.' + gcstrTMV7FileExt;
        end
        else
        begin
          ShowMessage('Selection cancelled. Creating a default .tmv file.');
          gvstrTMVFileName := gvstrAppPath + gcstrTMV7FileDir + '\Default.' + gcstrTMV7FileExt;
          NewTMVFile;
        end;// if FileExists(gvstrAppPath +
       end;// if OpenDialog1.Execute
    end
    else
    begin
      // No .tmv file exists so we create a defult one.
      ShowMessage('Creating a default .tmv file.');
      gvstrTMVFileName := gvstrAppPath + gcstrTMV7FileDir + '\Default.' + gcstrTMV7FileExt;
      NewTMVFile;
    end;// if FindFirst(gvstrAppPath

    FindClose(Info);
    DisplayTMVFileStatus;

  end;// if FileExists(gvstrTMVFileName)

  // Default the application to Radio Off
  LCDOff;

  // Try to open the COM port
  if not OpenPort then
  begin
    Result := True;
    Exit;
  end;// if not OpenPort

  TMV7_Init;
  Result := True;

end;// function Initialize


//========================================================================================
//          INIT THE TRANSCEIVER
//========================================================================================
procedure TMV7_Init;

var
  Info : TSearchRec;

begin

 // Turn AI off
 SendCommand ('AI', '0');

 // Set the Buffers to the INI file data
 SetBuffer(gcstrVHF);
 SetBuffer(gcstrUHF);

 // Set up the LCD Display
 SetColourScheme (gvstrCurrentColourScheme);

 // Get Busy Status
 GetUHFBYStatus;
 GetVHFBYStatus;

 // Get PTT Band Status
 DisplayBCStatus;

 // Get S Meter values
 GetVHF_SMValue;
 GetUHF_SMValue;

 // Display Data Source
 DisplayVHFDataSource;
 DisplayUHFDataSource;

 // Set Volume Levels
 SetVHF_AGValue(gvstrVHFAudioLevel);
 SetUHF_AGValue(gvstrUHFAudioLevel);

 // Set Squelch Settings
 SetVHF_SQValue(gvstrVHFSquelchLevel);
 SetUHF_SQValue(gvstrUHFSquelchLevel);

 // Turn off Mute

 // Turn AI On
 SendCommand ('AI', '1');

 // Default TX Status
 gvblnTXStatus := False;

end;// procedure TMV7_Init;

//========================================================================================
end.// unit Init

