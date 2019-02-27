
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

CLASS JBox2PdfBullZip: 
  DEF VAR cPrinterName        AS CHAR  NO-UNDO INIT "Bullzip PDF Printer".
  DEF VAR bOk                 AS LOG   NO-UNDO.
  DEF VAR cConvTypes          AS CHAR  NO-UNDO INIT "xls,xlsx,doc,docx,rtf,htm,html,mht,ppt,pptx".
  DEF VAR cNoWordConvertTypes AS CHAR  NO-UNDO INIT "xls,xlsx,tiff,jpg,txt,ppt,pptx".
  DEF VAR chWord              AS COM-HANDLE NO-UNDO.
  DEF VAR chExcel             AS COM-HANDLE NO-UNDO.
  DEF VAR oPdfSettings        AS CLASS Bullzip.PdfWriter.PdfSettings NO-UNDO.
	
	CONSTRUCTOR PUBLIC JBox2PdfBullZip (  ):
		
    SUPER().
        
    oPdfSettings = NEW Bullzip.PdfWriter.PdfSettings().
    oPdfSettings:SetValue("Showpdf","No").
    oPdfSettings:SetValue("ConfirmOverwrite","No"). 
    oPdfSettings:SetValue("ShowSaveAS","Never"). 
    oPdfSettings:SetValue("ShowSettings","Never").    
        
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

  METHOD PUBLIC CHARACTER convert2Pdf (INPUT icSourceFile AS CHAR,
                                       INPUT icTargetFile AS CHAR):
  
    DEF VAR cFileType   AS CHAR       NO-UNDO.
    DEF VAR chDocument  AS COM-HANDLE NO-UNDO.
    DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
    DEF VAR chWorksheet AS COM-HANDLE NO-UNDO.
    DEF VAR bOk         AS LOG        NO-UNDO.

    cFileType = ENTRY(NUM-ENTRIES(icSourceFile,"."),icSourceFile,".").

    IF NOT CAN-DO(cConvTypes,cFileType) THEN RETURN "".

    IF icTargetFile = "" THEN
      icTargetFile = SESSION:TEMP-DIR + ENTRY(NUM-ENTRIES(icSourceFile,"\"),icSourceFile,"\") + ".pdf".

    oPdfSettings:SetValue("Output",icTargetFile).      
    oPdfSettings:PrinterName = cPrinterName.
    oPdfSettings:WriteSettings().

    IF NOT CAN-DO(cNoWordConvertTypes,cFileType) AND VALID-HANDLE(chWord) THEN DO:
      chWord:Documents:Application:ActivePrinter = cPrinterName.

      chDocument = chWord:Documents:OPEN(icSourceFile).
      chDocument:PrintOut().
      chDocument:Close().
      RELEASE OBJECT chDocument.
    END.
    /*
    ELSE IF CAN-DO("xls,xslx",cFileType) AND VALID-HANDLE(chExcel) THEN DO:
      chWorkbook = chExcel:Workbooks:OPEN(icSourceFile).
      chWorkBook:PrintOut(,,1,cPrinterName,,TRUE,) NO-ERROR.
      MESSAGE ERROR-STATUS:GET-MESSAGE(1)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
        PdfUtil:PrintFile(icSourceFile,cPrinterName).
/*       MESSAGE ERROR-STATUS:ERROR SKIP         */
/*               ERROR-STATUS:GET-MESSAGE(1)     */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/*       IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:            */
/*         chExcel:Visible = YES.                                 */
/*         MESSAGE                                                */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/*         chWorkBook:PrintOut(,,1,cPrinterName,,TRUE,) NO-ERROR. */
/*         MESSAGE ERROR-STATUS:GET-MESSAGE(1)                    */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/*         chWorkbook:ActiveSheet:Visible = YES.                              */
/*         chWorkBook:ActiveSheet:PrintOut(,,1,cPrinterName,,TRUE,) NO-ERROR. */
/*       END. */
/*       IF ERROR-STATUS:ERROR THEN DO: */
/*         chWorksheet = chWord:Worksheets:Item(1) NO-ERROR.         */
/*         MESSAGE ERROR-STATUS:GET-MESSAGE(1)                       */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                    */
/*         IF NOT ERROR-STATUS:ERROR THEN                            */
/*           chWorksheet:PrintOut(,,1,cPrinterName,,TRUE,) NO-ERROR. */
/*         RELEASE OBJECT chWorksheet.                               */
/*       END.                                                        */
      ELSE chWorkbook:Close().
           MESSAGE 99
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RELEASE OBJECT chWorkbook.
    END.
    */
    ELSE PdfUtil:PrintFile(icSourceFile,cPrinterName) NO-ERROR.

    bOk = PdfUtil:WaitForFile(icTargetFile,10000).

   IF bOk THEN
      RETURN icTargetFile.
    ELSE RETURN "".
  END METHOD.

  METHOD PUBLIC VOID setPrinterName (INPUT icPrinterName AS CHAR):
    cPrinterName = icPrinterName.
  END METHOD.

  METHOD PUBLIC VOID setConvFileTypes (INPUT icConvTypes AS CHAR):
    cConvTypes = icConvTypes.
  END METHOD.

  METHOD PUBLIC CHARACTER addToConvFileTypes (INPUT icConvTypes AS CHAR):
    DEF VAR ix AS INT NO-UNDO.
    IF icConvTypes NE "" THEN DO ix = 1 TO NUM-ENTRIES(icConvTypes):
      IF NOT CAN-DO(cConvTypes,ENTRY(ix,icConvTypes)) THEN
        cConvTypes = cConvTypes + (IF cConvTypes NE "" THEN "," ELSE "") + ENTRY(ix,icConvTypes).
    END.
    RETURN cConvTypes.
  END METHOD.

  METHOD PUBLIC VOID setWordHandle (INPUT ichWord AS COM-HANDLE):
    chWord = ichWord.  
  END METHOD.

  METHOD PUBLIC VOID setExcelHandle (INPUT ichExcel AS COM-HANDLE):
    chExcel = ichExcel.
  END METHOD.

  METHOD PUBLIC CHARACTER setNoWordConvertTyoes (INPUT icNoWordConvertTypes AS CHAR):
    IF icNoWordConvertTypes NE "" THEN
      cNoWordConvertTypes = cNoWordConvertTypes + (IF cNoWordConvertTypes NE "" THEN "," ELSE "") + icNoWordConvertTypes.
    RETURN cNoWordConvertTypes.
  END METHOD.

  METHOD PUBLIC VOID resetPrinter():
    oPdfSettings:SetValue("Showpdf","").
    oPdfSettings:SetValue("ConfirmOverwrite",""). 
/*     oPdfSettings:SetValue("ShowSaveAS","No"). */
    oPdfSettings:SetValue("ShowSettings","").    
    oPdfSettings:SetValue("Output","").      
    oPdfSettings:PrinterName = cPrinterName.
    oPdfSettings:WriteSettings().
  END METHOD.

END CLASS.
