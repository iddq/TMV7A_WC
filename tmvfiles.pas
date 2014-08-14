unit TMVFiles;

{$mode objfpc}{$H+}

//========================================================================================
//
//  TMVFiles.pas
//
//  Calls: AppConstants
//         AppVariables
//         DataEntry
//         StatusBar : DisplayTMVFileStatus
//         TMVFiles_DTMF : ParseDTMFRecord
//                        MakeDTMFRecord
//         TMVFiles_FAV : ParseFAVRecord
//                        MakeFAVRecord
//         TMVFiles_UHF : ParseUHFRecord
//                        MakeUHFRecord
//         TMVFiles_VHF : ParseVHFRecord
//                        MakeVHFRecord
//         Utilities : GetToneFrequencyFromToneNr
//
//  Called By: Init : Initialize
//             Final : Finalize
//             Main : TfrmMain.mnuFileNewClick
//                    TfrmMain.mnuFileOpenClick
//                    TfrmMain.mnuFileSaveClick
//                    TfrmMain.mnuFileSaveAsClick
//             TMVFiles_VHF : ParseVHFRecord
//
//  Ver: 1.0.0
//
//  Date: 5 Aug 2014
//
//========================================================================================

interface

uses
  Classes, Dialogs, Messages, SysUtils,
  // Application Units
  AppConstants, AppTypes, AppVariables, StatusBar, TMVFiles_DTMF, TMVFiles_FAV,
  TMVFiles_UHF, TMVFiles_VHF, Utilities;

procedure NewTMVFile;
procedure OpenTMVFile(vstrFName : string);
procedure SaveTMVFile;
procedure SaveTMVFileAs;
procedure WriteTMVFile(vstrFileName : string);

implementation

uses
  DataEntry, Main;

var
  TMVFile : TextFile;

//----------------------------------------------------------------------------------------
//     CLOSE TMV FILE
//----------------------------------------------------------------------------------------
procedure CloseTMVFile;
begin
  Close(TMVFile);
end;// procedure CloseTMVFile;

//========================================================================================
//          OPEN TMV FILE
//========================================================================================
procedure OpenTMVFile(vstrFName : string);

var
  vstrTStr : string;
  vbytRecNr : Byte;

begin

  if Length(vstrFName) = 0 then
  begin
    frmMain.OpenDialog1.Title := 'Open TMV File';
    frmMain.OpenDialog1.InitialDir := gvstrAppPath + gcstrTMV7FileDir;
    frmMain.OpenDialog1.Filter := 'TMVFiles|*.tmv';
    frmMain.OpenDialog1.DefaultExt := 'tmv';

    if frmMain.OpenDialog1.Execute then
      gvstrTMVFileName := frmMain.OpenDialog1.Filename
    else
    begin
      ShowMessage('Cancelled');
      Exit;
    end;// if frmMain.OpenDialog1.Execute
  end;// if Length(vstrFName) = 0

    // Open the file for Reading
    Assign(TMVFile, gvstrTMVFileName);
    Reset(TMVFile);

    // Read the first line to validate the file. If it is [TMVFile Ver. 1.0.0] it is
    // a valid file
    ReadLn(TMVFile, vstrTStr);

    if vstrTSTr <> '[TMVFile Ver. 1.0.0]' then
    begin
      ShowMessage('Invalid File');
      Exit;
    end;// if vsrtTSTr <> '[TMVFile Ver. 1.0.0]'

    // Now bypass the VHF header
    ReadLn(TMVFile, vstrTStr);

    // and read all of the VHF data into the VHF array
    ReadLn(TMVFile, vstrTStr);
    vbytRecNr := 1;
    Repeat
      ParseVHFRecord(vbytRecNr, vstrTStr);
      ReadLn(TMVFile, vstrTStr);
      vbytRecNr := vbytRecNr + 1;
    until vstrTStr = gcstrTMV7UHFHeader;

    // Read all of the UHF data into the UHF array
    ReadLn(TMVFile, vstrTStr);
    vbytRecNr := 1;
    Repeat
      ParseUHFRecord(vbytRecNr, vstrTStr);
      ReadLn(TMVFile, vstrTStr);
      vbytRecNr := vbytRecNr + 1;
    until vstrTStr = gcstrTMV7FAVHeader;

    // Read all of the FAV data into the FAV array
    ReadLn(TMVFile, vstrTStr);
    vbytRecNr := 1;
    Repeat
      ParseFAVRecord(vbytRecNr, vstrTStr);
      ReadLn(TMVFile, vstrTStr);
      vbytRecNr := vbytRecNr + 1;
    until vstrTStr = gcstrTMV7DTMFHeader;

    // and read all of the DTMF data into the DTMF array
    vbytRecNr := 1;
    Repeat
      ReadLn(TMVFile, vstrTStr);
      ParseDTMFRecord(vbytRecNr, vstrTStr);
      vbytRecNr := vbytRecNr + 1;
    until EOF(TMVFile);

    // Close the TMV File
    CloseTMVFile;

    // Set Data Changed flag
    gvstrTMVDataChanged := False;

    // Update the Button captions
    frmMain.bbtFAV01.Caption := gvstrFAVChannelDataArray[1, gcbytChannelNameField];
    frmMain.bbtFAV02.Caption := gvstrFAVChannelDataArray[2, gcbytChannelNameField];
    frmMain.bbtFAV03.Caption := gvstrFAVChannelDataArray[3, gcbytChannelNameField];
    frmMain.bbtFAV04.Caption := gvstrFAVChannelDataArray[4, gcbytChannelNameField];
    frmMain.bbtFAV05.Caption := gvstrFAVChannelDataArray[5, gcbytChannelNameField];
    frmMain.bbtFAV06.Caption := gvstrFAVChannelDataArray[6, gcbytChannelNameField];
    frmMain.bbtFAV07.Caption := gvstrFAVChannelDataArray[7, gcbytChannelNameField];
    frmMain.bbtFAV08.Caption := gvstrFAVChannelDataArray[8, gcbytChannelNameField];
    frmMain.bbtFAV09.Caption := gvstrFAVChannelDataArray[9, gcbytChannelNameField];
    frmMain.bbtFAV10.Caption := gvstrFAVChannelDataArray[10, gcbytChannelNameField];
    frmMain.bbtFAV11.Caption := gvstrFAVChannelDataArray[11, gcbytChannelNameField];
    frmMain.bbtFAV12.Caption := gvstrFAVChannelDataArray[12, gcbytChannelNameField];

    // Update the Status Bar
    DisplayTMVFileStatus;

end;// procedure OpenTMVFile;

//========================================================================================
//     WRITE TMV FILE
//========================================================================================
procedure WriteTMVFile(vstrFileName : string);

var
  vbytTemp : Integer;

begin

    // Open the file for Writing
  Assign(TMVFile, vstrFileName);
  Rewrite(TMVFile);

  Writeln(TMVFile, '[TMVFile Ver. 1.0.0]');

  Writeln(TMVFile, gcstrTMV7VHFHeader);
  for vbytTemp := 1 to gcbytMaxVHFChannels do
    Writeln(TMVFile, MakeVHFRecord(vbytTemp));

  Writeln(TMVFile, gcstrTMV7UHFHeader);
  for vbytTemp := 1 to gcbytMaxUHFChannels do
    Writeln(TMVFile, MakeUHFRecord(vbytTemp));

  Writeln(TMVFile, gcstrTMV7FAVHeader);
  for vbytTemp := 1 to gcbytMaxFAVChannels do
    Writeln(TMVFile, MakeFAVRecord(vbytTemp));

  Writeln(TMVFile, gcstrTMV7DTMFHeader);
  for vbytTemp := 1 to gcbytMaxDTMFCodes do
    Writeln(TMVFile, MakeDTMFRecord(vbytTemp));

  // Close the TMV File
  CloseTMVFile;

  // Set Data Changed flag
  gvstrTMVDataChanged := False;

  // Update the Status Bar
  DisplayTMVFileStatus;

end;// procedure WriteTMVFile;

//========================================================================================
//          NEW TMV FILE
//========================================================================================
procedure NewTMVFile;

var
  vintTempRec : Integer;
  vbytTempField : Byte;

begin

  // First we clear all channel arrays
  // UHF Data Array
  For vintTempRec := 1 to gcbytMaxUHFChannels do
  begin
   For vbytTempField := gcbytVFOField to gcbytMaxChannelFieldCount do
     gvstrUHFChannelDataArray[vintTempRec, vbytTempField] := '';
  end;// For vintTempRec := 1 to gcbytMaxUHFChannels do

  // VHF Data Array
  For vintTempRec := 1 to gcbytMaxVHFChannels do
  begin
    For vbytTempField := gcbytVFOField to gcbytMaxChannelFieldCount do
      gvstrVHFChannelDataArray[vintTempRec, vbytTempField] := '';
  end;// For vintTempRec := 1 to gcbytMaxVHFChannels do

  // FAV Data Array
  For vintTempRec := 1 to gcbytMaxFAVChannels do
  begin
    For vbytTempField := gcbytVFOField to gcbytMaxChannelFieldCount do
      gvstrFAVChannelDataArray[vintTempRec, vbytTempField] := '';
  end;// For vintTempRec := 1 to gcbytMaxFAVChannels do

  // Now the DTMF Data Array
  For vintTempRec := 1 to gcbytMaxDTMFCodes do
  begin
    gvstrDTMFCodeDataArray[vintTempRec] := '';
  end;// For vintTempRec := 1 to gcbytMaxDTMFCodes do

  // Now we create the file and write the data
  WriteTMVFile(gvstrTMVFileName);

  // Clear the FAV buttona
  frmMain.bbtFav01.Caption := '';
  frmMain.bbtFav02.Caption := '';
  frmMain.bbtFav03.Caption := '';
  frmMain.bbtFav04.Caption := '';
  frmMain.bbtFav05.Caption := '';
  frmMain.bbtFav06.Caption := '';
  frmMain.bbtFav07.Caption := '';
  frmMain.bbtFav08.Caption := '';
  frmMain.bbtFav09.Caption := '';
  frmMain.bbtFav10.Caption := '';
  frmMain.bbtFav11.Caption := '';
  frmMain.bbtFav12.Caption := '';

  // Updaste the Status bar
  DisplayTMVFileStatus;

end;// procedure NewTMVFile;

//========================================================================================
procedure SaveTMVFile;
begin
  WriteTMVFile(gvstrTMVFileName);
end;// procedure SaveTMVFile;

//========================================================================================
procedure SaveTMVFileAs;
begin

    frmMain.SaveDialog1.Title := 'Save the TMV File as';
    frmMain.SaveDialog1.InitialDir := gvstrAppPath + gcstrTMV7FileDir;
    frmMain.SaveDialog1.Filter := 'TMVFiles|*.tmv';
    frmMain.SaveDialog1.DefaultExt := 'tmv';

    if frmMain.SaveDialog1.Execute then
    begin
      gvstrTMVFileName := frmMain.SaveDialog1.Filename;
      WriteTMVFile(gvstrTMVFileName);
        DisplayTMVFileStatus;
    end
    else
      ShowMessage('Save TMV File As Cancelled');

end;// procedure SaveTMVFileAs;

//========================================================================================
end.// unit TMVFiles;

