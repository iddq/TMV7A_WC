unit SplashAbout;

{$mode objfpc}{$H+}

//========================================================================================
//
//  SplashAbout.pas
//
//
//  Calls: AppConstants
//
//  Called By: Main : TfrmMain.FormShow
//                    TfrmMain.mnuHelpAboutClick
//
//  Ver: 1.0.0
//
//  Date: 18 Aug 2013
//
//========================================================================================


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons,
  // Application Units
  AppConstants;

type

  { TdlgSplashAbout }
  TdlgSplashAbout = class(TForm)
    bbtHelp: TBitBtn;
    bbtOk: TBitBtn;
    Image1: TImage;
    lblAppName: TLabel;
    lblVerNr: TLabel;
    lblDate: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Timer1: TTimer;
    procedure ShowAbout;
    procedure ShowSplash;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dlgSplashAbout: TdlgSplashAbout;

implementation

{$R *.lfm}

//========================================================================================
procedure TdlgSplashAbout.ShowAbout;
begin
  Timer1.Enabled := False;
  lblAppName.Caption := gcstrAppName+'.EXE';
  lblVerNr.Caption := 'Ver: '+gcstrAppVersion;
  lblDate.Caption := gcstrAppDate;
  BorderIcons := [];
  BorderStyle := bsSingle;
  BorderIcons := [];
  bbtHelp.Visible := True;
  bbtOk.Visible := True;
  ShowModal;
end;// procedure TdlgSplashAbout.ShowAbout;

//========================================================================================
procedure TdlgSplashAbout.ShowSplash;
begin
  Timer1.Interval:=5000;
  lblAppName.Caption := gcstrAppName+'.EXE';
  lblVerNr.Caption := 'Ver: '+gcstrAppVersion;
  lblDate.Caption := gcstrAppDate;
  BorderStyle := bsNone;
  bbtHelp.Visible := False;
  bbtOk.Visible := False;
  ShowModal;
end;// procedure TdlgSplashAbout.ShowSplash;

//========================================================================================
procedure TdlgSplashAbout.Timer1Timer(Sender: TObject);
begin
  Close;
end;// procedure TdlgSplashAbout.Timer1Timer

//========================================================================================
end.

