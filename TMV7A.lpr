program TMV7A;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uecontrols, RackCtlsPkg, lazcontrols, dbflaz, printer4lazarus, rx,
  sdflaz, sdposeriallaz, Main, Init, Final, AppConstants, AppVariables,
  INIStuff, StatusBar, COMPort, ResponseParser, LCDDisplay, BCCommand,
  PSCommand, SerialStuff, BUFCommand, ColourSchemes, BYCommand, DataEntry, Fav,
  TMVFiles, Utilities, DataEntry_FAV, AppTypes, SMCommand, AGCommand, SQCommand,
  RXCommand, TXCommand, Mute, MEM, dataentry_vhfmem, MEM_VHF, MEM_UHF, MEM_DTMF,
  TMVFiles_DTMF, TMVFiles_UHF, TMVFiles_VHF, TMVFiles_FAV, DataEntry_UHFMem,
  Register, SplashAbout, NagScreen, HUtils, Reverse, PCCommand, TMVFileReport,
  StepMessage;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmCOMPort, frmCOMPort);
  Application.CreateForm(TfrmDataEntry, frmDataEntry);
  Application.CreateForm(TfrmMEM, frmMEM);
  Application.CreateForm(TdlgRegister, dlgRegister);
  Application.CreateForm(TdlgSplashAbout, dlgSplashAbout);
  Application.CreateForm(TdlgNagScreen, dlgNagScreen);
  Application.CreateForm(TfrmTMVFileReport, frmTMVFileReport);
  Application.CreateForm(TfrmStepMessage, frmStepMessage);
  Application.Run;
end.

