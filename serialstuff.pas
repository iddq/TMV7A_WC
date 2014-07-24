unit SerialStuff;

{$mode objfpc}{$H+}

//========================================================================================
//
//  SerialStuff.pas
//
//  Calls: Main;
//         HUtils
//         AppConstants
//         AppVariables
//
//  Called By: AGCommand : GetVHF_AGValue
//                         GetUHF_AGValue
//             BCCommand : TogglePTTBand
//             BYCommand : GetUHFBYStatus
//                         GetVHFBYStatus
//            Init : Initialize
//            PCCommand : ToggleRFPower
//            PSCommand : TogglePowerOnOff
//            SMCommand : GetVHF_SMValue
//                        GetUHF_SMValue
//
//  Ver: 1.0.0
//
//  Date: 22 Sep 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, Forms, SysUtils,
  // Application Units
  AppConstants, AppVariables, COMPort, HUtils;

Function OpenPort : Boolean;
Procedure ClosePort;
Function SendCommand (vstrKeyword, vstrParameters : string) : boolean;

implementation

uses
  Main;

//========================================================================================
//          SDPOSERIAL1 ROUTINES
//========================================================================================
Function OpenPort : Boolean;
begin

  // See if we have a COM port configured. If not, we prompt for one
  if gvstrCOMPort = '' then
  begin
    InfoMessageDlgOk('No COM Port Configured', 'Please enter a COM Port');
    frmCOMPort.showmodal;
  end;// if gvstrCOMPort = ''

  // Try to open the COM port
  frmMain.sdpoSerial1.Device := gvstrCOMPort;
  try
    frmMain.sdpoSerial1.Active := True;
  except
  end;// try

  if not frmMain.sdpoSerial1.Active then
  begin
      // It did not open, so we assume that the port is incorrect and promt
      // for a new selection.
      MessageDlg(' Unable to open ' + gvstrCOMPort + '. This is probably due' + #13 +
             '       to an invalid COM port selection. ' + #13 +
             '         Select the correct COM port.',
             mtError, [mbOk], 0 );
    // Now we try to opne the new port. If that fails, we give up, display
    // an error message and return to frmMain.
    frmCOMPort.showmodal;

    // Try to open the COM port
    try
      frmMain.sdpoSerial1.Active := True;
    except
    end;// try

    if not frmMain.sdpoSerial1.Active then
    begin
      MessageDlg('Unable to Open this port as well' + gvstrCOMPort + '. There appears ' + #13 +
               'to be a system problem.',
               mtError, [mbOk], 0 );
      Result := False;
      gvstrCOMPort := gcstrDefCOMPort;
      gvblnTMV7OnLine := False;
      Exit;
    end;//if not frmMain.sdpoSerial1.Active
  end;// if not frmMain.sdpoSerial1.Active

  // Now see if the radio is there. This command will time out if
  //     The TMV7 is not connected to the serial cable, or
  //     The TMV7 is not turned on initially.
  if not SendCommand ('PS', '') then
  begin
    MessageDlg(' Unable to Communicate with the Radio. This is ' + #13 +
             ' probably due to a bad or misconnected serial' + #13 +
             '     cable or the TMV7 may be turned off.', mtError, [mbOk], 0 );
    ClosePort;
    gvblnTMV7OnLine := False;
    Result := False;
    Exit;
  end;// if not SendCommand ('PS', '')

  gvblnTMV7OnLine := True;

end;// Function OpenPort

//========================================================================================
Procedure ClosePort;
begin
  frmMain.sdpoSerial1.Active := False;
end;//Procedure ClosePort

//========================================================================================
Function SendCommand (vstrKeyword, vstrParameters : string) : boolean;

var
  vstrCommand : string;

begin

  if frmMain.SdpoSerial1.Active then
  begin

    // Form the TMV7 command. If there are Parameters passed, then we add a <Space> and the
    // Parameters and a <CR> to the Keyowrd, otherwise we simply terminate the Keyword with
    // a <CR>.
    if Length (vstrParameters) > 0 then
      vstrCommand := vstrKeyword + ' ' + vstrParameters + #13
    else
      vstrCommand := vstrKeyword + #13;

    // Wait for all received messages to be processed. If we time out we have an error
    // and display a message and exit the function with a Result of False.

    //   SendCommand := False;
    //   Send message

    // The Receive buffer is clear so now we can send the command
    gvblnKeywordMatched := False;
    gvstrKeywordSent := vstrKeyword;
    frmMain.sdpoSerial1.WriteData (vstrCommand);
    SendTimeoutTimerReset;

    // Here we wait for a match from the received response. If we get it, we reset it to
    // False, set the Function Result to True and exit the function. Otherwise we exit
    // the Function with a result of False (defaulted earlier.
    while frmMain.tmrSendTimeout.Enabled do
    begin
      Application.ProcessMessages;
      if gvblnKeywordMatched then
      begin
        SendCommand := True;
        Exit;
      end;
    end;// while frmMain.tmrSendTimeout.Enabled

    SendCommand := False;

  end
  else
  begin

  end;// if frmMain.sdpoSerial1.Active

end;// Function SendCommand

//========================================================================================

end.// unit SerialStuff;

