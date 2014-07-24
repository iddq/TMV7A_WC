unit HUReports;

{$mode objfpc}{$H+}

//========================================================================================
//
//  HUReports.pas
//
//  Calls:
//
//  Called By:
//
//  Ver: 1.0.0
//
//  Date: 11 Nov 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, Printers, SysUtils, Types;

type THUReport = class(TPrinter)
end;

type THUPage = class(TObject)
    // Printer properties
//  PaperName : string;
   private
    Constructor Create;
//  protected
  public
    PaperName : string;
//  published
end;// type THUPage


implementation

Constructor THUPage.Create;
begin
  Inherited Create;
  PaperName := Printer.PaperSize.PaperName;
end;

end.// unit HUReports;

