
/*------------------------------------------------------------------------
    File        : BullZipPrintFile.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Brynjar
    Created     : Fri Dec 03 10:36:58 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

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

/*PdfUtil:PrintFile("c:\temp\respondents_sessions.txt", "Bullzip PDF Printer").*/


MESSAGE "hei"
VIEW-AS ALERT-BOX.
