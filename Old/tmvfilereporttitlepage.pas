unit TMVFileReportTitlePage;

{$mode objfpc}{$H+}

//========================================================================================
//
//  TMVFileReportTitlePage.pas
//
//  Calls: TMVFileReport
//
//  Called By: TMVFileReport : PrintTMVFileReport
//
//  Ver: 1.0.0
//
//  Date: 20 Nov 2013
//
//========================================================================================

interface

uses
  Classes, Dialogs, Graphics, Printers, SysUtils;

procedure PrintTitlePage( vstrTitle : string );

//========================================================================================
implementation

uses
  TMVFileReport;

var
  vintTitlePageLeft : Integer;
  vintTitlePageTop : Integer;
  vintTitlePageRight : Integer;
  vintTitlePageBottom : Integer;
  vintTitlePageWidth : Integer;
  vintTitlePageHeight : Integer;

  vintTitlePageCentreX : Integer;
  vintTitlePageCentreY : Integer;

  vintTitleWidth : Integer;
  vintTitleHeight : Integer;
  vintTitleX : Integer;
  vintTitleY : Integer;

//========================================================================================
procedure PrintTitlePage( vstrTitle : string );
begin

  //======================================================================================
  // We want a double Frame for the Title page so we print the Outside Frame from
  // the TMVFileReport unit, then re-set the frame points to calculate the Inner
  // Frame and define the printable area on the Title page only.
  // and then
  //======================================================================================

  PrintPageFrame;
  vintTitlePageTop := vintPageTop + 30;
  vintTitlePageLeft := vintPageLeft + 30;
  vintTitlePageRight := vintPageRight - 30;
  vintTitlePageBottom := vintPageBottom - 30;
  vintTitlePageWidth := vintTitlePageRight - vintTitlePageLeft;
  vintTitlePageHeight := vintTitlePageBottom - vintTitlePageTop;

  Printer.Canvas.Frame( vintTitlePageLeft, vintTitlePageTop,
                        vintTitlePageRight, vintTitlePageBottom);

  //======================================================================================
  // Now we have to find the centre of the Printable Area. We will use this to
  // to position and draw the Frame for the Title and the Title string.
  //======================================================================================

  vintTitlePageCentreX := vintTitlePageLeft + ( vintTitlePageWidth div 2);
  vintTitlePageCentreY := vintTitlePageBottom - ( vintTitlePageHeight div 2 );


  //======================================================================================
  // Now we have to set the Font, calculate the height and width of the Title string.
  // We use these values to printe the Title and to create the Frame around the Title.
  //======================================================================================

    // Set the Font
  Printer.Canvas.Font.Name := 'Arial';
  Printer.Canvas.Font.Size := 15;
  Printer.Canvas.Font.Color := clBlack;
  Printer.Canvas.Font.Style:=[fsBold];

    // Calculate the size of the |Title string
  vintTitleWidth := Printer.Canvas.TextWidth( vstrTitle );
  vintTitleHeight := Printer.Canvas.TextHeight( vstrTitle );
    // We now calculate the Start positon (Left, Top) of the Title
  vintTitleX := vintTitlePageCentreX - ( vintTitleWidth div 2 );
  vintTitleY := vintTitlePageCentreY - ( vintTitleHeight div 2 );
    // Now we print the Title
  Printer.Canvas.TextOut( vintTitleX, vintTitleY, vstrTitle );
    // Now we print the Title Frame
  Printer.Canvas.Frame( vintTitleX - 35, vintTitleY - 30,
                        vintTitleX + vintTitleWidth + 40, vintTitleY + vintTitleHeight + 40);
  Printer.Canvas.Frame( vintTitleX - 55, vintTitleY - 50,
                        vintTitleX + vintTitleWidth + 60, vintTitleY + vintTitleHeight + 60);

end;// procedure PrintTitlePage

//========================================================================================
end.// unit TMVFileReportTitlePage

