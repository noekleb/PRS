/*  */

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  lesexcelvpifil.p (<cFileName>,output <cUtfilnamn>).
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFileName  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cUtfilnamn AS CHARACTER   NO-UNDO.

DEF VAR chExcelApplication    AS COM-HANDLE.  
DEF VAR chWorkbooks           AS COM-HANDLE.
DEF VAR chWorksheets          AS COM-HANDLE.
  
DO ON ERROR UNDO, LEAVE:
    CREATE "Excel.Application" chExcelApplication.  
    chExcelApplication:Visible = FALSE.                                     
    chExcelApplication:DisplayAlerts = FALSE.

    /* Leser inn filen i et excel Ark. */
    chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).

    STATUS DEFAULT "Setter aktivt ark...".
    chWorkSheets = chExcelApplication:Sheets:Item(1).

    /* Skriver til temp fil csv format */
    chWorkSheets:SaveAs(cUtfilnamn,20). /* 42 */ 

    RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
    RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
    chExcelApplication:Workbooks:CLOSE().
    chExcelApplication:Quit().
END.
RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */

ASSIGN chWorksheets       = ?
       chWorkbooks        = ?
       chExcelApplication = ?.
