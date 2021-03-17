DO:
  /* Invoked on top of server-side procedures.
     You should comment out the "validsession" test in a production environment. */ 
  DEF VAR cOrgDateFormat    AS CHAR   NO-UNDO.
  DEF VAR cOrgNumFormat     AS CHAR   NO-UNDO.
  DEF VAR cCurrUserId       AS CHAR   NO-UNDO.
  DEF VAR iCurrCompanyId    AS INT    NO-UNDO.
  DEF VAR cCurrLanguage     AS CHAR   NO-UNDO.
  DEF VAR iCodeMaster       AS INT    NO-UNDO.
  DEF VAR cCurrContext      AS CHAR   NO-UNDO.

  cCurrUserId = USERID('skotex').

  /*
  {incl/demo.i}
  
  IF 
    icSessionId NE "validsession" AND 
    (SEARCH("jbserv_validatesession.r") NE ? OR SEARCH("jbserv_validatesession.p") NE ?) THEN DO:

    RUN jbserv_validatesession.p (INPUT icSessionId, 
                                  OUTPUT ocReturn, 
                                  OUTPUT cCurrUserId, 
                                  OUTPUT iCurrCompanyId,
                                  OUTPUT cCurrLanguage
                                  ).
    IF ocReturn NE "" THEN RETURN.
  END.
  ELSE 
    IF icSessionId NE "validsession" THEN 
      DO:
    ocReturn = "Invalid session".
    RETURN.
  END.
  */

  ASSIGN cOrgDateFormat  = SESSION:DATE-FORMAT
         cOrgNumFormat   = SESSION:NUMERIC-FORMAT.
END.

