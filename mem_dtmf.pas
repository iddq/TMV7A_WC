unit MEM_DTMF;

{$mode objfpc}{$H+}

//========================================================================================
//
//  Mem_VHF.pas
//
//  Calls: AppConstants
//         AppVariables
//         Mem
//
//  Called By:
//
//  Ver: 1.0.0
//
//  Date: 12 Aug 2013
//
//========================================================================================


interface

uses
  Classes, SysUtils,
  // Application Units
  AppConstants;

procedure LoadDTMFStringGrid;
procedure SetDTMFCode;

implementation

uses
  Mem;

//========================================================================================
procedure LoadDTMFStringGrid;

var
  vbytTemp : Byte;

begin

  for vbytTemp := 1 to gcbytMaxDTMFCodes do
  begin

    // Channel Nr
    frmMem.sgrDTMF.Cells[gcbytChMemNrCol, vbytTemp] := IntToStr(vbytTemp);

  end;// for vbytTemp := 1 to gcbytMaxDTMFCodes do

end;// procedure LoadDTMFStringGrid;

//========================================================================================
procedure SetDTMFCode;
begin

end;// procedure SetDTMFCode;

//========================================================================================
end.// unit MEM_DTMF;

