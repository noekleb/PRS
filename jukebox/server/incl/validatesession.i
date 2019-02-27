DO:
  /* Invoked on top of server-side procedures.
     You should comment out the "validsession" test in a production environment. */ 
  DEF VAR cOrgDateFormat    AS CHAR   NO-UNDO.
  DEF VAR cOrgNumFormat     AS CHAR   NO-UNDO.
  DEF VAR cCurrUserId       AS CHAR   NO-UNDO.
  DEF VAR iCurrCompanyId    AS INT    NO-UNDO.
  DEF VAR cCurrLanguage     AS CHAR   NO-UNDO.
  DEF VAR cCurrContext      AS CHAR   NO-UNDO.
  DEF VAR iCodeMaster       AS INT    NO-UNDO.
  DEF VAR hBuffCompToComp   AS HANDLE NO-UNDO.

  {incl/demo.i}
  {incl/server_devmode.i}
  
  IF 
    icSessionId NE "validsession" AND 
    (SEARCH("jbserv_validatesession.r") NE ? OR SEARCH("jbserv_validatesession.p") NE ?) THEN DO:

    RUN jbserv_validatesession.p (INPUT icSessionId, 
                                  OUTPUT ocReturn, 
                                  OUTPUT cCurrUserId, 
                                  OUTPUT iCurrCompanyId,
                                  OUTPUT cCurrLanguage,
                                  OUTPUT cCurrContext
                                  ).
    IF ocReturn NE "" THEN 
      RETURN.  
    ELSE DO:
      CREATE BUFFER hBuffCompToComp FOR TABLE "JBoxCompanyToCompany" NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
        hBuffCompToComp:FIND-FIRST("WHERE iJBoxCompanyId = " + STRING(iCurrCompanyId) + " AND cCompanyRole = 'codemaster'"
                                   ,NO-LOCK) NO-ERROR.  
        IF NOT ERROR-STATUS:ERROR AND hBuffCompToComp:AVAIL THEN
          iCodeMaster = hBuffCompToComp:BUFFER-FIELD("iJBoxToCompanyid"):BUFFER-VALUE.
        ELSE iCodeMaster = iCurrCompanyId.
        DELETE OBJECT hBuffCompToComp NO-ERROR.
      END.        
      ELSE iCodeMaster = iCurrCompanyId.
    END.
  END.
  ELSE 
    IF icSessionId NE "validsession" THEN 
    DO:
      ocReturn = "Invalid session".
      RETURN.
    END.
    ELSE IF USERID(LDBNAME(1)) NE "" THEN cCurrUserId = USERID(LDBNAME(1)).

  ASSIGN cOrgDateFormat  = SESSION:DATE-FORMAT
         cOrgNumFormat   = SESSION:NUMERIC-FORMAT.
         
  IF CAN-DO("WEBSPEED,APPSERVER",SESSION:TYPE) AND PROGRAM-NAME(1) NE "jbserv_api_for_server.p" AND
     (SEARCH("jbserv_loadserverlib.p") NE ? OR SEARCH("jbserv_loadserverlib.p") NE ?) THEN 
    RUN jbserv_loadserverlib.p (icSessionId).
END.
