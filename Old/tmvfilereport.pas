unit TMVFileReport;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_DBSet, LR_DSet, LR_Desgn, LR_View,
  Forms, Controls, Graphics, Dialogs, DbCtrls, Buttons, StdCtrls;

type

  { TfrmTMVFileReport }

  TfrmTMVFileReport = class(TForm)
    frReport1: TfrReport;
    frUserDataset1: TfrUserDataset;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTMVFileReport: TfrmTMVFileReport;


implementation

{$R *.lfm}


{ TfrmTMVFileReport }



end.

