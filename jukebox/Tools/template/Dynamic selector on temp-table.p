  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.
  DEF VAR bOk         AS LOG  NO-UNDO.

  /* Use the hook (procedure) setSelectorAttributes to fill the temp-table
     See also winsrc\samples\SelectorUsageTT.w */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
/*   RUN JBoxDSelector.w (THIS-PROCEDURE,0, */
                      "temp-table"                               
                      + ";!dummy|character|x"           /* <- to avoid initial sort invoke a dummy non-visual field */
                      + ";ValueField|INTEGER|>>9||Valuelabel"
                      + ";Description|CHARACTER|x(30)"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "ValueField",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cRowIdList NE "" THEN DO:
    IF NOT JBoxServerApi:Instance:CallServerProc("<myServerProc.p>",cRowIdList) THEN
      JBoxServerAPI:Instance:viewCallMessage().
  END.
            
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
