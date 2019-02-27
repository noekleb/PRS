DEF INPUT  PARAM icSessionId       AS CHAR NO-UNDO.
DEF INPUT  PARAM iiJBoxCompanyId   AS INT  NO-UNDO.
DEF OUTPUT PARAM ocUserLevel       AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk              AS LOG  NO-UNDO.

DEF VAR ocReturn          AS CHAR   NO-UNDO.
DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR hUserBuffer       AS HANDLE NO-UNDO.
DEF VAR hCompUserBuffer   AS HANDLE NO-UNDO.

{incl/validatesession.i}

CREATE BUFFER hUserBuffer FOR TABLE "JBoxUser" NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN DO:
  bOk = hUserBuffer:FIND-FIRST("WHERE cJBoxUserId = '" + cCurrUserId + "'") NO-ERROR.
  IF bOk THEN DO:
    IF hUserBuffer:BUFFER-FIELD("bSuperUser"):BUFFER-VALUE THEN 
      ASSIGN ocUserLevel = "SUPER"
             obOk        = YES.
    ELSE DO:          
      CREATE BUFFER hCompUserBuffer FOR TABLE "JBoxCompanyUser" NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN DO:
        bOk = hCompUserBuffer:FIND-FIRST("cJBoxUserId = '" + cCurrUserId + "' AND iJBoxCompanyId = " + STRING(iiJBoxCompanyId)) NO-ERROR.
        IF bOk THEN DO:
          IF hCompUserBuffer:BUFFER-FIELD("bSuperUserCompany"):BUFFER-VALUE THEN
            ASSIGN ocUserLevel = "companysuper"
                   obOk        = YES.
          ELSE 
            ASSIGN ocUserLevel = "normal"
                   obOk        = YES.
        END.
      END.
    END.
  END.
END.
DELETE OBJECT hUserBuffer NO-ERROR.
DELETE OBJECT hCompUserBuffer NO-ERROR.

