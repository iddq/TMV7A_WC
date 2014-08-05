unit TMVFileReport;

{$mode delphi}

//========================================================================================
//
//  TMVFileReport.pas
//
//  Calls:
//
//  Called By:
//
//  Ver: 1.0.0
//
//  Date: 27 Jul 2014
//
//========================================================================================


interface

uses
  Classes, db, SysUtils, SdfData, FileUtil, LR_Class, LR_DBSet, Forms, Controls,
  Graphics, Dialogs, StdCtrls;

type

  { TfrmTMVFileReport }

  TfrmTMVFileReport = class(TForm)
    edtCHName: TEdit;
    edtRXFrequency: TEdit;
    edtCHNR: TEdit;
    frDBDTMFDataSet: TfrDBDataSet;
    frDBFAVDataSet: TfrDBDataSet;
    frDBUHFDataSet: TfrDBDataSet;
    frDBVHFDataSet: TfrDBDataSet;
    frTMVFileReport: TfrReport;
    SdfDTMFDataSet: TSdfDataSet;
    SdfFAVDataSet: TSdfDataSet;
    SdfUHFDataSet: TSdfDataSet;
    SdfVHFDataSet: TSdfDataSet;
    procedure FormCreate(Sender: TObject);
    procedure frVHFReportGetValue(const ParName: String; var ParValue: Variant);
  private
    { private declarations }
  public
    { public declarations }
    procedure CreateTmpFiles(vstrFileName : string);
    procedure DeleteTempFiles;
  end;

var
  frmTMVFileReport: TfrmTMVFileReport;

implementation

{$R *.lfm}

const
  cstrDTMFFileName = 'DTMF.TMP';
  cstrFAVFileName = 'FAV.TMP';
  cstrUHFFileName = 'UHF.TMP';
  cstrVHFFileName = 'VHF.TMP';

  cstrDelimeter = ',';

  cstrCHNRFieldDef = 'CHNR';
  cstrVFOFieldDef = 'VFO';
  cstrRXFREQFieldDef = 'RXFREQ';
  cstrSTEPFieldDef = 'STEP';
  cstrSHIFTFieldDef = 'SHIFT';
  cstrREVERSEFieldDef = 'REVERSE';
  cstrTONEFieldDef = 'TONE';
  cstrTFREQFieldDef = 'TFREQ';
  cstrDTSSFieldDef = 'DTSS';
  cstrDTSSCODEFieldDef = 'DTSSCODE';
  cstrSHIFTOFFSETFieldDef = 'SHIFTOFFSET';
  cstrSCANFieldDef = 'SCAN';
  cstrRFPOWERFieldDef = 'RFPOWER';
  cstrCHNAMEFieldDef = 'CHNAME';
  cstrCOMMENTSFieldDef = 'COMMENTS';

  cstrVHFUHFHeader = cstrCHNRFieldDef + cstrDelimeter +
                   cstrVFOFieldDef + cstrDelimeter +
                   cstrRXFREQFieldDef + cstrDelimeter +
                   cstrSTEPFieldDef + cstrDelimeter +
                   cstrSHIFTFieldDef + cstrDelimeter +
                   cstrREVERSEFieldDef + cstrDelimeter +
                   cstrTONEFieldDef + cstrDelimeter +
                   cstrTFREQFieldDef + cstrDelimeter +
                   cstrDTSSFieldDef + cstrDelimeter +
                   cstrDTSSCODEFieldDef + cstrDelimeter +
                   cstrSHIFTOFFSETFieldDef + cstrDelimeter +
                   cstrSCANFieldDef + cstrDelimeter +
                   cstrRFPOWERFieldDef + cstrDelimeter +
                   cstrCHNAMEFieldDef + cstrDelimeter +
                   cstrCOMMENTSFieldDef + cstrDelimeter;

  cstrCODENRFieldDef = 'CODENR';
  cstrCODEFieldDef = 'CODE';

  cstrDTMFHeader = cstrCODENRFieldDef + cstrDelimeter +
                   cstrCODEFieldDef + cstrDelimeter;


var
  vfilTMVFile : TextFile;
  vstrTMVFileName : string;

{ TfrmTMVFileReport }


  vstrTMVFilePath : string;
  vstrDTMFFileName : string;
  vfilFAVFile : TextFile;
  vstrFAVFileName : string;
  vfilDTMFFile : TextFile;
  vstrUHFFileName : string;
  vfilUHFFile : TextFile;
  vstrVHFFileName : string;
  vfilVHFFile : TextFile;

//========================================================================================
//          PUBLIC ROUTINES
//========================================================================================
procedure TfrmTMVFileReport.CreateTmpFiles(vstrFileName : string);
var
  vbytTemp : byte;
  vstrTStr : string;

begin

  vstrTMVFileName := vstrFileName;

  // Get the TMV Filepath ao we can create the TMP files in the same folder
  vstrTMVFilePath := ExtractFileDir(vstrTMVFileName);

  // Open the file for Reading
  AssignFile(vfilTMVFile, vstrTMVFileName);
  Reset(vfilTMVFile);

  // Read the first line to validate the file. If it is [TMVFile Ver. 1.0.0] it is
  // a valid file
  Readln(vfilTMVFile, vstrTStr);

  if vstrTSTr <> '[TMVFile Ver. 1.0.0]' then
  begin
    ShowMessage('Invalid TMVFile');
    CloseFile(vfilTMVFile);
    Exit;
  end;// if vsrtTSTr <> '[TMVFile Ver. 1.0.0]'



  //=============================================================
  // We have a valid input file so now we create the TMP VHF file
  //=============================================================
  vstrVHFFileName := vstrTMVFilePath + '\' + cstrVHFFileName;
  AssignFile(vfilVHFFile, vstrVHFFileName);
  Rewrite(vfilVHFFile);

  // Now bypass the VHF header
  ReadLn(vfilTMVFile, vstrTStr);

  // Create the VHF-UHF CSV Header and read all of the VHF data into the TMP file
  Writeln(vfilVHFFile, cstrVHFUHFHeader);

  for vbytTemp := 1 to 99 do
  begin
    ReadLn(vfilTMVFile, vstrTStr);
    Writeln(vfilVHFFile, vstrTStr);
  end;

  // Close the VHF file
  CloseFile(vfilVHFFile);

  // and Open the VHF Database
  SdfVHFDataSet.FileName := vstrVHFFileName;
  sdfVHFDataset.Active := True;
  sdfVHFDataSet.First;

    //===============================
    // Now we create the TMP UHF file
    //===============================
    vstrUHFFileName := vstrTMVFilePath + '\' + cstrUHFFileName;
    AssignFile(vfilUHFFile, vstrUHFFileName);
    Rewrite(vfilUHFFile);

    // and Reset the TMV File and bypassthe UHF header
    Reset(vfilTMVFile);
    repeat
      ReadLn(vfilTMVFile, vstrTStr);
    until vstrTStr = '[UHF MEMORY]';

    // Create the VHF-UHF CSV Header and read all of the UHF data into the TMP file
    Writeln(vfilUHFFile, cstrVHFUHFHeader);

    for vbytTemp := 1 to 99 do
    begin
      ReadLn(vfilTMVFile, vstrTStr);
      Writeln(vfilUHFFile, vstrTStr);
    end;

    // Close both files
    CloseFile(vfilTMVFile);
    CloseFile(vfilUHFFile);

    // and Open the UHF Database
    SdfUHFDataSet.FileName := vstrUHFFileName;
    SdfUHFDataSet.Active := True;
    SdfUHFDataSet.First;

    //===============================
    // Now we create the TMP FAV file
    //===============================
    vstrFAVFileName := vstrTMVFilePath + '\' + cstrFAVFileName;
    AssignFile(vfilFAVFile, vstrFAVFileName);
    Rewrite(vfilFAVFile);

    // and Reset the TMV File and bypassthe FAV header
    Reset(vfilTMVFile);
    repeat
      ReadLn(vfilTMVFile, vstrTStr);
    until vstrTStr = '[FAV MEMORY]';

    // Create the VHF-UHF CSV Header and read all of the UHF data into the TMP file
    Writeln(vfilFAVFile, cstrVHFUHFHeader);

    for vbytTemp := 1 to 12 do
    begin
      ReadLn(vfilTMVFile, vstrTStr);
      Writeln(vfilFAVFile, vstrTStr);
    end;

    // Close both files
    CloseFile(vfilTMVFile);
    CloseFile(vfilFAVFile);

    // and Open the FAV Database
    sdfFAVDataSet.FileName := vstrFAVFileName;
    SdfFAVDataSet.Active := True;
    SdfFAVDataSet.First;






    //===============================
    // Now we create the TMP DTMF file
    //===============================
    vstrDTMFFileName := vstrTMVFilePath + '\' + cstrDTMFFileName;
    AssignFile(vfilDTMFFile, vstrDTMFFileName);
    Rewrite(vfilDTMFFile);

    // and Reset the TMV File and bypassthe FAV header
    Reset(vfilTMVFile);
    repeat
      ReadLn(vfilTMVFile, vstrTStr);
    until vstrTStr = '[DTMF MEMORY]';

    // Create the DTMF CSV Header and read all of the UHF data into the TMP file
    Writeln(vfilDTMFFile, cstrDTMFHeader);

    for vbytTemp := 1 to 10 do
    begin
      ReadLn(vfilTMVFile, vstrTStr);
      Writeln(vfilDTMFFile, vstrTStr);
    end;

    // Close both files
    CloseFile(vfilTMVFile);
    CloseFile(vfilDTMFFile);

    // and Open the FAV Database
    sdfDTMFDataSet.FileName := vstrDTMFFileName;
    sdfDTMFDataSet.Active := True;
    sdfDTMFDataSet.First;








end;// procedure TfrmTMVFileReport.CreateTmpFiles

//========================================================================================
procedure TfrmTMVFileReport.DeleteTempFiles;
begin
//  vstrDTMFFileName : string;
//***  vfilFAVFile.Delete;;
//  vstrFAVFileName : string;
//  vfilDTMFFile : TextFile;
//  vstrUHFFileName : string;
 // vfilUHFFile : TextFile;
//  vstrVHFFileName : string;
//  vfilVHFFile : TextFile;

end;// TfrmTMVFileReport.DeleteTempFIles

//========================================================================================
//          FORM ROUTINES
//========================================================================================
procedure TfrmTMVFileReport.FormCreate(Sender: TObject);
begin

    // Set up the VHF Dataset
    sdfVHFDataSet.FieldDefs.Add(cstrCHNRFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrCHNRFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrVFOFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrVFOFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrRXFREQFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrRXFREQFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrSTEPFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrSTEPFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrSHIFTFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrSHIFTFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrREVERSEFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrREVERSEFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrTONEFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrTONEFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrTFREQFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrTFREQFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrDTSSFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrDTSSFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrDTSSCODEFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrDTSSCODEFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrSHIFTOFFSETFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrSHIFTOFFSETFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrSCANFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrSCANFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrRFPOWERFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrRFPOWERFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrCHNAMEFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrCHNAMEFieldDef);
    sdfVHFDataSet.FieldDefs.Add(cstrCOMMENTSFieldDef, ftString);
    sdfVHFDataSet.Schema.Add(cstrCOMMENTSFieldDef);

    // Set up the UHF Dataset
    sdfUHFDataSet.FieldDefs.Add(cstrCHNRFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrCHNRFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrVFOFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrVFOFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrRXFREQFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrRXFREQFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrSTEPFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrSTEPFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrSHIFTFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrSHIFTFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrREVERSEFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrREVERSEFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrTONEFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrTONEFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrTFREQFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrTFREQFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrDTSSFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrDTSSFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrDTSSCODEFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrDTSSCODEFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrSHIFTOFFSETFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrSHIFTOFFSETFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrSCANFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrSCANFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrRFPOWERFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrRFPOWERFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrCHNAMEFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrCHNAMEFieldDef);
    sdfUHFDataSet.FieldDefs.Add(cstrCOMMENTSFieldDef, ftString);
    sdfUHFDataSet.Schema.Add(cstrCOMMENTSFieldDef);

    // Set up the FAV Dataset
    sdfFAVDataSet.FieldDefs.Add(cstrCHNRFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrCHNRFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrVFOFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrVFOFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrRXFREQFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrRXFREQFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrSTEPFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrSTEPFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrSHIFTFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrSHIFTFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrREVERSEFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrREVERSEFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrTONEFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrTONEFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrTFREQFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrTFREQFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrDTSSFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrDTSSFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrDTSSCODEFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrDTSSCODEFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrSHIFTOFFSETFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrSHIFTOFFSETFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrSCANFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrSCANFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrRFPOWERFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrRFPOWERFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrCHNAMEFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrCHNAMEFieldDef);
    sdfFAVDataSet.FieldDefs.Add(cstrCOMMENTSFieldDef, ftString);
    sdfFAVDataSet.Schema.Add(cstrCOMMENTSFieldDef);

    // Set up the DTMF Dataset
    sdfDTMFDataSet.FieldDefs.Add(cstrCODENRFieldDef, ftString);
    sdfDTMFDataSet.Schema.Add(cstrCODENRFieldDef);
    sdfDTMFDataSet.FieldDefs.Add(cstrCODEFieldDef, ftString);
    sdfDTMFDataSet.Schema.Add(cstrCODEFieldDef);

end;// procedure TfrmTMVFileReport.FormCreate

//========================================================================================
procedure TfrmTMVFileReport.frVHFReportGetValue(const ParName: String; var ParValue: Variant);
begin

  if ParName = 'rpvTMVFileName' then
    ParValue := ExtractFileName(vstrTMVFileName);

end;// procedure TfrmTMVFileReport.frVHFReportGetValue

//========================================================================================
end.// unit TMVFileReport;

