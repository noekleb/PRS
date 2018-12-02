  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  /* Use the hook (procedure) setSelectorAttributes to add optional panel for extra parameters
     and getSelectorAttributes to retrieve panel input
     See also winsrc\samples\SelectorUsage.w */

  cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                "SalesRep",      /* Buffer(list) for query */
                                "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                "where RepName begins 'b'").

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
/*   RUN JBoxDSelector.w (THIS-PROCEDURE,0,  */
                      "SalesRep"      
                      + ";SalesRep"  
                      + ";RepName"
                      + ";MonthQuota[1]"
                      + ";MonthQuota[2]"
                      ,"WHERE true",
                      INPUT-OUTPUT cRowIdList,
                      "SalesRep", /* Return values for this/these in cIdList (comma-sep list of fields) */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cRowIdList NE "" THEN DO:
    IF NOT JBoxServerApi:Instance:CallServerProc("<myServerProc.p>",cRowIdList) THEN
      JBoxServerAPI:Instance:viewCallMessage().
  END.
            
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
