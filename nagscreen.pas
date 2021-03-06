unit NagScreen;

{$mode objfpc}{$H+}

//========================================================================================
//
//  NagScreen.pas
//
//  Calls: AppConstants
//         Register
//
//  Called By: Init : Initialize
//
//  Ver: 1.0.0
//
//  Date: 23 Dec 2013
//
//========================================================================================

interface

uses
  Classes, DOS, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls,
  // Application Units
  AppConstants, HUtils, Register;

type

  { TdlgNagScreen }

  TdlgNagScreen = class(TForm)
    bbtHelp: TBitBtn;
    bbtRegister: TBitBtn;
    bbtCancel: TBitBtn;
    Label1: TLabel;
    procedure bbtCancelClick(Sender: TObject);
    procedure bbtHelpClick(Sender: TObject);
    procedure bbtRegisterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dlgNagScreen: TdlgNagScreen;

implementation

{$R *.lfm}

const
  dlgNagScreenCaption = 'TMV7A Not Registered !';
  Label1Caption = 'This copy of TMV7A.EXE is not yet registered.' + #13 +
             'Registration is free and provides you with' + #13 +
             'information and details on program development,' + #13 +
             'bug fixes, program enhancements and new releases.' + #13 +
             #13 +
             'Registration is optional and program functionality' + #13 +
             'is not restricted in any way if you do not wish to' + #13 +
             'register. You will, however, continue to see this' + #13 +
             'annoying Nag screen every time you launch TMV7.EXE.';

//========================================================================================
//          FORM ROUTINES
//========================================================================================
procedure TdlgNagScreen.FormCreate(Sender: TObject);
begin
  Caption := dlgNagScreenCaption;
  Label1.Caption := Label1Caption;
end;// procedure TdlgNagScreen.FormCreate

//========================================================================================
procedure TdlgNagScreen.FormShow(Sender: TObject);
begin
  bbtRegister.SetFocus;
end;// procedure TdlgNagScreen.FormShow

//========================================================================================
//          BUTTON ROUTINES
//========================================================================================
procedure TdlgNagScreen.bbtRegisterClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;// procedure TdlgNagScreen.bbtRegisterClick

//========================================================================================
procedure TdlgNagScreen.bbtCancelClick(Sender: TObject);
begin
  InfoMessageDlgOk('Please Register', 'Please consider registration in the future');
end;// procedure TdlgNagScreen.bbtCancelClick

//========================================================================================
procedure TdlgNagScreen.bbtHelpClick(Sender: TObject);
begin
  Exec('hh.exe', '-mapid 2 ' + gcstrAppName + '.chm');
end;// procedure TdlgNagScreen.bbtHelpClickprocedure TdlgNagScreen.bbtHelpClick

//========================================================================================
end.// unit NagScreen;

