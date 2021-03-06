unit HUtils;

//========================================================================================
//
//  HUtils.pas
//
//  Calls:
//
//  Called By:
//
//  Ver: 1.0.0
//
//  Date: 21 Dec 2013
//
//========================================================================================

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, Forms, SysUtils;

// Character Validation Routines
function ValidAlphaCharacter( Key: char) : char;
function ValidCallsignCharacter( Key: char) : char;
function ValidDigitCharacter( Key: char) : char;
// Message Boxes
function ErrorMessageDlgOk(vstrCaption, vstrMsg  : string) : TModalResult;
function InfoMessageDlgOk(vstrCaption, vstrMsg  : string) : TModalResult;
function ConfirmationMessageDlg(vstrCaption, vstrMsg  : string) : TModalResult;
// Registration Routines
function CalculateRegistrationKey (vstrInputString : string) : string;

implementation

//========================================================================================
//          CHARACTER VALIDATION ROUTINES
//========================================================================================
function ValidAlphaCharacter( Key: char) : char;
begin
    // Returns only Valid Alphabetic Characters. Non-valid characters are converted
    // into Null (#0) characters.
    //Valid Alpha C haracters are:
    // <BS>
    // <SP>
    // [A..Z]
    // [a..z]
    Result := Key;
    case Key of
      #8 : Exit; // <BS>
      #32 : Exit; // <SP>
      #65..#90 : Exit; // [A..Z]
      #97..#122 : Exit; // [a..z]
    else
      Result := #0;
    end;// case Key of
end;// function ValidAlphaCharacter(var Key: char);

//========================================================================================
 function ValidCallsignCharacter( Key: char) : char;
begin
    // Returns only Valid Callsign Characters. Non-valid characters are converted
    // into Null (#0) characters.
    //Valid Alpha C haracters are:
    // <BS>
    // </>
    // [0..9]
    // [A..Z]
    // [a..z] Converted to Uppercase
    Result := Key;
    case Key of
      #8 : Exit; // <BS>
      #47 : Exit;  // </>
      #48..#57 : Exit; // [0..9]
      #65..#90 : Exit; // [A..Z]
      #97..#122 : begin
                    Result := UpCase(Key);
                    Exit; // [a..z]
                  end;
    else
      Result := #0;
    end;// case Key of
end;// function ValidCallsignCharacter(var Key: char);

//========================================================================================
 function ValidDigitCharacter( Key: char) : char;
 begin
     // Returns only Valid Digits. Non-valid characters are converted
     // into Null (#0) characters.
     //Valid Digit Characters are:
     // <BS>
     // [0..9]
     Result := Key;
     case Key of
       #8 : Exit; // <BS>
       #48..#57 : Exit; // [0..9]
     else
       Result := #0;
     end;// case Key of
 end;// function ValidDigitCharacter(var Key: char);

//========================================================================================
//          MESSAGES
//========================================================================================
function ErrorMessageDlgOk(vstrCaption, vstrMsg  : string) : TModalResult;
begin
  Result := MessageDlg(vstrCaption, vstrMsg, mtError, [mbOk], 0);
end;// function ErrorMessageDlgOk

//========================================================================================
function InfoMessageDlgOk(vstrCaption, vstrMsg  : string) : TModalResult;
begin
  Result := MessageDlg(vstrCaption, vstrMsg, mtInformation, [mbOk], 0);
end;// function InfoMessageDlgOk

//========================================================================================
function ConfirmationMessageDlg(vstrCaption, vstrMsg  : string) : TModalResult;
begin
  Result := MessageDlg(vstrCaption, vstrMsg, mtConfirmation, [mbYes, mbNo], 0);
end;// function ConfirmationMessageDlg

//========================================================================================
//          REGISTRATION ROUTINES
//========================================================================================
 function CalculateRegistrationKey (vstrInputString : string) : string;

 var
   vintVal1 : Longint;
   vintVal2 : Longint;
   vintVal3 : Longint;
   vintVal4 : Longint;
   vstrTStr : string;
   vchrChar1 : Char;
   vchrChar2 : Char;

 begin
   // A Registration Key is based on the Ordinal value of the first and last characters
   // of a string passed in the vstrInputString variable. These values are multiplied
   // five times to obtain at least a 10 digit integer. That integer is converted
   // into a string and the firt eight characters are returned as a calculated "Key"
   // value for that specific Input String.
   //
   // For Testing purposes, an input value of 'HU' will produce a Key of '93636000'
   // 'HS' will produce a Key of '89281440' and VU wil produce a Key of '13359025'.

   if Length(vstrInputString) < 2 then
   begin
     Result := '';
     Exit;
   end;// if Length() < 2

   vstrTStr := UpperCase(vstrInputString);
   vintVal1 := Ord(vstrTStr[1]);
   if vintVal1 < 32 then
     vintVal1 := 32;
   if vintVal1 > 90 then
     vintVal1 := 90;
   vintVal2 := Ord(vstrTStr[Length(vstrTStr)]);
   if vintVal2 < 32 then
     vintVal2 := 32;
   if vintVal2 > 90 then
     vintVal2 := 90;

   vintVal3 := vintVal1*vintVal2*vintVal1*vintVal2;
   vintVal4 := vintVal1*vintVal2*vintVal1*vintVal2;
   Result := Copy(IntToStr(vintVal3*25),1,8);

 end;// function ValidRegistration

//========================================================================================
end.// unit HUtils;

