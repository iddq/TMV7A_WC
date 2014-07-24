unit RadioStatus;

{$mode objfpc}{$H+}

{========================================================================================}
{                                                                                        }
{  RadioStatus.pas                                                                       }
{                                                                                        }
{  Calls: Main;                                                                          }
{                                                                                        }
{  Called By: Init : Initialize                                                          }
{                                                                                        }
{  Ver: 1.0.0                                                                            }
{                                                                                        }
{  Date: 25 mar 2013                                                                     }
{                                                                                        }
{========================================================================================}

interface

uses
  Classes, SysUtils;

procedure RadioStatusOff;
procedure RadioStatusOn;

implementation

uses
  Main;

procedure RadioStatusOff;
begin
  frmMain.pnlLCD.Visible := False;
end;// procedure RadioStatusOff

//========================================================================================
procedure RadioStatusOn;
begin
  frmMain.pnlLCD.Visible := True;
end;// procedure RadioStatusOn

//========================================================================================

end.// unit RadioStatus;

