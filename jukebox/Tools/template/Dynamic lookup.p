  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  /* See also call-back procedure setLookupAttributes */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Customer"
                    + ";Custnum"
                    + ";Name"
                  + ",SalesRep"
                    + ";Repname"
                   ,"WHERE false"
                  + ",FIRST SalesRep NO-LOCK OF Customer" 
                    ,""                /* [ ABL attribute list (MULTIPLE,TITLE|<myTitle>,NUM-LOCKED-COLUMNS|2,..) ] */                                       
                    ,"CustNum,Name",   /* <- return (pipe-separated) values for these fields */
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    IF NOT JBoxServerApi:Instance:CallServerProc("<myServerProc.p>",cReturnValues) THEN
      JBoxServerAPI:Instance:viewCallMessage().
  END.
            
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
