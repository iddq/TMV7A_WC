unit AppTypes;

{$mode objfpc}{$H+}

//========================================================================================
//
//  AppTypes.pas
//
//  Calls:
//
//  Called By: DataEntry
//             DataEntry_FAV : DataEntry_FAV_Save
//             DataEntry_VHFMEM : DataEntry_VHFMEM_Save
//             Main : TfrmMain.mnuMemVHFClick
//                    TfrmMain.mnuMemUHFClick
//                    TfrmMain.mnuMemDTMFClick
//             Utilities : DisplayDataArray
//
//  Ver: 1.0.0
//
//  Date: 9 Dec 2013 2013
//
//========================================================================================

interface

uses
  Classes, SysUtils;

type
  TDataRecordType = (drtVHFMEM, drtUHFMEM, drtFAV, drtDTMF);
  TDataEntryType = (detFAV, detVFO, detUHFMEM, detVHFMEM);
  TRFPowerLevel = (rfpLow, rfpMedium, rfpHigh);
  TSelectedBand = (sbUHF, sbVHF);

implementation

end.// unit AppTypes;

