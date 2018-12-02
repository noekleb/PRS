

DEFINE input parameter ip_FileName AS CHAR  NO-UNDO. 

DEFINE VARIABLE chOpenOffice    AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE chWorkBook      AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE chDesktop       AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE chWorkSheet     AS COM-HANDLE   NO-UNDO.
DEFINE VARIABLE chCell          AS COM-HANDLE   NO-UNDO.

DEFINE VARIABLE mFileProperties  AS RAW     NO-UNDO.

DEFINE VARIABLE cImportCol AS CHAR EXTENT 200 NO-UNDO. 
DEFINE VARIABLE cLine AS CHAR NO-UNDO. 
DEFINE VARIABLE iRow AS INT INIT 0 NO-UNDO. 

DEF VAR cDelimiter AS CHAR. 
DEFINE VARIABLE iCnt AS INT NO-UNDO. 
DEFINE STREAM instream. 



FUNCTION writeCellData RETURNS LOGICAL 
( INPUT ip_Col          AS INT,           /* Column Number */
  INPUT  ip_Row          AS INT,           /* Row Number */
  INPUT  ip_Data         AS CHAR): 
    
    DEF VAR         decTest         AS DEC      NO-UNDO.
    ASSIGN chCell = chWorkSheet:GetCellByPosition(ip_Col,ip_Row).

    ASSIGN decTest = DEC(ip_Data) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN chCell:SetFormula(ip_Data).
    ELSE IF decTest = 0 AND 
            ip_data NE ""  THEN chCell:SetValue(DEC(ip_Data)).
    ELSE IF decTest NE 0 AND 
            decTest NE ? THEN   chCell:SetValue(DEC(ip_Data)).
                           
    IF chCell <> ? THEN 
    DO:
        RELEASE OBJECT chCell.
        ASSIGN chCell  = ?.
    END.                     
END.

DEFINE VARIABLE iSep AS INT EXTENT 2 NO-UNDO. 
DEFINE VARIABLE iSepUse as int no-undo. 

ASSIGN
    chOpenOffice = ?
    chWorkBook   = ?
    chDesktop    = ?
    chWorkSheet  = ?.

    CREATE "com.sun.star.ServiceManager" chOpenOffice CONNECT NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) <> "" THEN
        CREATE "com.sun.star.ServiceManager" chOpenOffice.
    
    chDesktop = chOpenOffice:createInstance("com.sun.star.frame.Desktop").
  
    chWorkBook  = chDesktop:loadComponentFromURL("private:factory/scalc", "_blank", 0, mFileProperties).
    chWorkSheet = chWorkBook:Sheets:getByIndex(0).
    chWorkSheet:COLUMNS("A:CC"):OptimalWidth = TRUE.

    chCell  = chWorkSheet:GetCellRangeByName("A1:CC1") .
    chCell:CharWeight = 150. 
   
    INPUT STREAM instream FROM VALUE (ip_fileName). 
    REPEAT: 
        IMPORT STREAM instream UNFORMATTED cLine.
        iSep[1] = NUM-ENTRIES(cLine,CHR(9)).
        iSep[2] = NUM-ENTRIES(cLine,CHR(59)).
        
        IF iSep[1] GE iSep[2] THEN iSepUse = 9. ELSE iSepUse = 59. 
        
        DO iCnt = 1 TO NUM-ENTRIES(cLine,CHR(iSepUse)):
           cImportCol[iCnt] = ENTRY(icnt,cLine,CHR(iSepUse)). 
           writeCellData(icnt - 1,iRow,cImportCol[iCnt]).
        END.
        iRow = iRow + 1. 
    END.

    chWorkSheet:COLUMNS("A:CC"):OptimalWidth = TRUE.

    IF chCell           <> ? THEN RELEASE OBJECT chCell.
    IF chOpenOffice     <> ? THEN RELEASE OBJECT chOpenOffice.
    IF chWorkBook       <> ? THEN RELEASE OBJECT chWorkBook.
    IF chDesktop        <> ? THEN RELEASE OBJECT chDesktop.
    IF chWorkSheet      <> ? THEN RELEASE OBJECT chWorkSheet.




