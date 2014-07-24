unit BYCommand;

{$mode objfpc}{$H+}

{========================================================================================}
{                                                                                        }
{  BYCommand.pas                                                                         }
{                                                                                        }
{  Calls: AppConstants                                                                      }
{         LCDDisplay : DisplayUHFBusyStatus                                              }
{                      DisplayVHFBusyStatus                                              }
{                      SerialStuff : SendCommand                                         }
{         SerialStuff : SednCommand                                                      }
{                                                                                        }
{  Called By: ResponseParser : ParseResponse                                             }
{             Init : Initialize                                                          }
{                                                                                        }
{  Ver: 1.0.0                                                                            }
{                                                                                        }
{  Date: 9 May 2013                                                                      }
{                                                                                        }
{========================================================================================}

interface

uses
  Classes, SysUtils,
  //     APPLICATION UNITS
  AppConstants, LCDDisplay, SerialStuff;

procedure BYResponseHandler (vstrKeywordRcvd, vstrParameters : string);
procedure GetUHFBYStatus;
procedure GetVHFBYStatus;

implementation
//========================================================================================

procedure BYResponseHandler (vstrKeywordRcvd, vstrParameters : string);

var
  vstrBand : string;
  vstrBusy : string;

begin

  // First we parse the reponse (n1,n2) where n1 = Band (0 = VHF, 1 = UHF) and
  // n2 = Busy (0 = Not Busy, 1 = Busy).
  vstrBand := Copy(vstrParameters, 1,1);
  vstrBusy := Copy(vstrParameters, 3,1);

  if vstrBand = gcstrUHF then
  begin
    if vstrBusy = gcstrOn then
      DisplayUHFBusyStatus('BUSY')
    else
      DisplayUHFBusyStatus('');
  end
  else
  begin
    if vstrBusy = gcstrOn then
      DisplayVHFBusyStatus('BUSY')
    else
      DisplayVHFBusyStatus('');
  end;// if vstrBand = gcstrUHF

end;// procedure BYResponseHandler

//========================================================================================

procedure GetUHFBYStatus;
begin
  SendCommand('BY',gcstrUHF);
end;// procedure GetUHFBYStatus;

//========================================================================================

procedure GetVHFBYStatus;
begin
  SendCommand('BY',gcstrVHF);
end;// procedure GetVHFBYStatus;

//========================================================================================

end.// unit BYCommand;

