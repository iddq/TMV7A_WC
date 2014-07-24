unit TMVFileReportDetailPage;

{$mode objfpc}{$H+}

//========================================================================================
//
//  TMVFileReportDetailPage.pas
//
//  Calls: TMVFileReport : PrintPageFrame
//
//  Called By: TMVFileReport : PrintTMVFileReport
//
//  Ver: 1.0.0
//
//  Date: 26 Nov 2013
//
//========================================================================================

interface

uses
  Classes, Graphics, Printers, SysUtils;
  // Application Units

procedure PrintChannelRecord( vstrSection, vstrTStr : string );

//========================================================================================
implementation

uses
  tmvFileReport;

const
  cstrHeaderText = ' MEMORY CHANNELS';

var
  vstrDetailSection : string;
  vintDetailPageWidth : integer;
  vintDetailPageCentreX : Integer;

  vintHeaderBottom : integer;
  vintHeaderX : integer;
  vintHeaderY : integer;
  vstrHeader : string;
  vintHeaderWidth : integer;
  vintHeaderHeight : integer;

  vintFooterTop : integer;

//========================================================================================
procedure PrintPageHeaderandFooter;
begin

  // For the first page we set the font and calculate all X and Y values that we need to
  // print the Header Line, Footer line, and text strings. We then use these values for
  // all subsequent Detail Pages.
  if vbytPageNr = 1 then
  begin

    // Calculate the common points
    vintDetailPageWidth := vintPageRight - vintPageLeft;
    vintDetailPageCentreX := vintPageLeft + ( vintDetailPageWidth div 2);

    // Set the Header Font
    Printer.Canvas.Font.Name := 'Arial';
    Printer.Canvas.Font.Size := 15;
    Printer.Canvas.Font.Color := clBlack;
    Printer.Canvas.Font.Style:=[fsBold];
    // and calculate the Header points
    vintHeaderBottom := vintPageTop+300;
    // Calculate the size of the Header string
    vintHeaderWidth := Printer.Canvas.TextWidth( vstrDetailSection + cstrHeaderText );
    vintHeaderHeight := Printer.Canvas.TextHeight( cstrHeaderText );
    // We now calculate the Start positon (Left, Top) of the Header
//***    vintHeaderX := vintTitlePageCentreX - ( vintTitleWidth div 2 );
//***    vintHeaderY := vintTitlePageCentreY - ( vintTitleHeight div 2 );

    // Set the Footer Font
    Printer.Canvas.Font.Name := 'Arial';
    Printer.Canvas.Font.Size := 10;
    Printer.Canvas.Font.Color := clBlack;
    Printer.Canvas.Font.Style:=[];
    // and calculate the Footer points
    vintFooterTop := vintPageBottom-300;

  end;// if vbytPageNr = 1



  // Set the Header Font
  Printer.Canvas.Font.Name := 'Arial';
  Printer.Canvas.Font.Size := 15;
  Printer.Canvas.Font.Color := clBlack;
  Printer.Canvas.Font.Style:=[fsBold];
  // and print the Header
  Printer.Canvas.Line( vintPageLeft, vintHeaderBottom, vintPageRight, vintHeaderBottom);

  // Set the Footer Font
  Printer.Canvas.Font.Name := 'Arial';
  Printer.Canvas.Font.Size := 10;
  Printer.Canvas.Font.Color := clBlack;
  Printer.Canvas.Font.Style:=[];
  // and print the FOoter
  Printer.Canvas.Line( vintPageLeft, vintFooterTop, vintPageRight, vintPageBottom-300);

end;// procedure PrintPageHeader;

//========================================================================================
procedure StartNewPage;
begin

  with Printer do
  begin

    NewPage;
    Inc( vbytPageNr );
    vblnNewPage := False;
    PrintPageFrame;
    PrintPageHeaderandFooter;

  end;// with Printer do

end;// procedure StartNewPage;

//========================================================================================
procedure PrintChannelRecord( vstrSection, vstrTStr : string );
begin

  vstrDetailSection := vstrSection;
  if vblnNewPage then
   StartNewPage;


end;// procedure PrintChannelRecord( vstrTStr );

//========================================================================================
end.// unit TMVFileReportDetailPage;

