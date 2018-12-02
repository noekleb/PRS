
/*------------------------------------------------------------------------
    File        : baxi.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Brynjar
    Created     : Mon Feb 28 17:35:40 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

USING BBS.BAXI.*.
DEF VAR oBaxiCtrl AS CLASS BBS.BAXI.BaxiCtrl NO-UNDO.  

oBaxiCtrl = NEW BBS.BAXI.BaxiCtrl().

oBaxiCtrl:Open().

oBaxiCtrl:TransferAmount_V2("0000",0x30,100,0x30,0x30,0x30,100,"","").
/*
BBS.BAXI.BaxiCtrl:Open().
*/
/*
DEFINE VARIABLE class1 AS CLASS BBS.BAXI.BaxiCtrl.
class1 = NEW BBS.BAXI.BaxiCtrl().
*/








/*
USING Bullzip.PdfWriter.*.
/*PdfUtil:<static_method>( <parameters> ).*/
def var cPrinterName as char no-undo.
DEF VAR oPdfSettings AS class Bullzip.PdfWriter.PdfSettings NO-UNDO.

/* DEF VAR oPdfWriter   AS Bullzip.PdfWriter NO-UNDO. */
/* oPdfWriter = NEW Bullzip.PdfWriter.                */

oPdfSettings = NEW Bullzip.PdfWriter.PdfSettings().

oPdfSettings:PrinterName = "test".
cPrinterName = oPdfSettings:PrinterName.
MESSAGE cPrinterName
    VIEW-AS ALERT-BOX.
*/ 