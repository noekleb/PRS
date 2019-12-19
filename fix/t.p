DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.

ASSIGN 
    cInnfil = 'C:\tmp\tn\lokasjon14112019.xlsx'
    .
                                                  
RUN konvExcel2csv (cInnFil,
                   '20',
                   OUTPUT cUtFil
                  ).
                                                  
    PROCEDURE konvExcel2csv:
        DEF INPUT PARAMETER pcInnFil AS CHARACTER.
	    DEF INPUT PARAMETER pcformat AS CHARACTER.
	    DEF OUTPUT PARAMETER pcUtFil AS CHARACTER.
		
		DEFINE VARIABLE pbOk               AS LOGICAL          NO-UNDO.

        DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.  
        DEFINE VARIABLE chWorkbooks        AS COMPONENT-HANDLE NO-UNDO.
        DEFINE VARIABLE chWorksheets       AS COMPONENT-HANDLE NO-UNDO.
          
/*        pcUtFil  = getTmpFileName().*/
        ASSIGN 
            pcFormat = (IF pcFormat = '' THEN '20' ELSE pcformat)
            FILE-INFO:FILE-NAME = SEARCH(pcInnFil).
            pcUtFil  = FILE-INFO:FILE-NAME
            .  
        ENTRY(
              NUM-ENTRIES(pcUtFil,'.'),
              pcUtFil,
              '.') = 'csv'
              .
        KONV_BLOKK:
        DO:
            CREATE "Excel.Application" chExcelApplication.  
            chExcelApplication:Visible = FALSE.                                     
            chExcelApplication:DisplayAlerts = FALSE.

            chWorkbooks = chExcelApplication:Workbooks:OpenText(pcInnFil,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
            chWorkSheets = chExcelApplication:Sheets:Item(1).
            chWorkSheets:SaveAs(pcUtfil,pcFormat). /* 42 */
            chExcelApplication:Workbooks:CLOSE().
            chExcelApplication:QUIT().
        END. /* KONV_BLOKK */
        
        RELEASE OBJECT chWorkSheets NO-ERROR.
        RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
        
        ASSIGN 
            chWorksheets       = ?
            chWorkbooks        = ?
            chExcelApplication = ?.
 
        /* Flagger resultatet. */
        IF SEARCH(pcUtfil) <> ? THEN 
            pbOk = TRUE.
            
		RETURN.

	END PROCEDURE.
