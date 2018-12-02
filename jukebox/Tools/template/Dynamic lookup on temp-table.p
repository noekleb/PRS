  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  /* Use the hook (procedure) setLookupAttributes to fill the temp-table */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "temp-table ttList"
                    + ";Identifier|INTEGER|>>9||Id"          /* Fieldname|Datatype|Format|Initial value|Label */
                    + ";Description|CHARACTER|x(30)||Desc"
                   ,"WHERE false"
                    ,""              /* [ ABL attribute list (MULTIPLE,TITLE|<myTitle>,NUM-LOCKED-COLUMNS|2,..) ] */  	                                        
                    ,"Identifier",   /* <- return values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    IF NOT JBoxServerApi:Instance:CallServerProc("<myServerProc.p>",cReturnValues) THEN
      JBoxServerAPI:Instance:viewCallMessage().
  END.
            
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
