unit TMVFileSave;

{$mode objfpc}{$H+}

//========================================================================================
//
//  TMNVFILESAVE.pas
//
//  Calls:
//
//  Called By:
//
//  Ver: 1.0.0
//
//  Date: 8 Jul 2013
//
//========================================================================================

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TfrmTMVFileSave }

  TfrmTMVFileSave = class(TForm)
    bbtCreateNew: TBitBtn;
    bbtReplace: TBitBtn;
    bbtCancel: TBitBtn;
    Label1: TLabel;
    procedure bbtCancelClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTMVFileSave: TfrmTMVFileSave;

implementation

{$R *.lfm}

{ TfrmTMVFileSave }


procedure TfrmTMVFileSave.bbtCancelClick(Sender: TObject);
begin
//  ModalResult := mrNone;
end;

end.// unit TMVFileSave;

