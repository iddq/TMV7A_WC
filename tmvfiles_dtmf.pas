unit TMVFiles_DTMF;

{$mode objfpc}{$H+}

interface

//========================================================================================
//
//  TMVFiles_DTMF.pas
//
//  Calls: AppConstants
//         AppVariables
//         Utilities : GetToneFrequencyFromToneNr
//
//  Called By: TMVFiles : OpenTMVFile
//
//  Ver: 1.0.0
//
//  Date: 9 Aug 2013
//
//========================================================================================

uses
  Classes, SysUtils;

function MakeDTMFRecord(vbytRecord : Byte) : string;
procedure ParseDTMFRecord(vbytRecNr : Byte; vstrRecord : string);

implementation

//========================================================================================
function MakeDTMFRecord(vbytRecord : Byte) : string;

var
  vstrTRecord : string;

begin

  // Record Nr
  vstrTRecord := IntToStr(vbytRecord) + ',';

  Result := vstrTRecord;

end;// function MakeDTMFRecord

//========================================================================================
procedure ParseDTMFRecord(vbytRecNr : Byte; vstrRecord : string);

var
  vbytCommaPos : Byte;
  vstrTStr : string;
  vbytTbyt : Byte;
  vstrTToneNr : string;

begin

  // Bypass the Record Nr
  vbytCommaPos := Pos(',', vstrRecord );
  vstrRecord := Copy(vstrRecord, vbytCommaPos+1, Length(vstrRecord));

end;// procedure ParseVHFRecord;

//========================================================================================
end.// unit TMVFiles_DTMF;

