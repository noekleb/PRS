DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO. 
DEF INPUT  PARAM iiEntityId    AS INT  NO-UNDO.
DEF OUTPUT PARAM cQueryString  AS CHAR NO-UNDO.
DEF OUTPUT PARAM cFields       AS CHAR NO-UNDO.
DEF OUTPUT PARAM cBuffers      AS CHAR NO-UNDO.
DEF OUTPUT PARAM cLabels       AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hBuffLogin AS HANDLE NO-UNDO.
DEF VAR iCompanyId AS INT    NO-UNDO.

DEF VAR ix          AS INT NO-UNDO.
DEF VAR bOK         AS LOG NO-UNDO.


{incl/validatesession.i}

CREATE BUFFER hBuffLogin FOR TABLE "JBoxLoginSession" NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN DO:
  bOk = hBuffLogin:FIND-UNIQUE("WHERE cSessionId = '" + icSessionId + "'",NO-LOCK) NO-ERROR.
  IF bOK THEN
    iCompanyId = hBuffLogin:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE.
  ELSE iCompanyId = 1.
END.

{incl/ttjbqueryfieldselect.i}
DEF BUFFER bqEntity   FOR JBoxQEntity.
DEF BUFFER bqRelation FOR JBoxQRelation.

FIND FIRST JBoxQEntity WHERE JBoxQEntity.iEntityId = iiEntityId NO-LOCK NO-ERROR.
RUN BuildQuery (OUTPUT bOK).

IF VALID-HANDLE(hBuffer)    THEN DELETE OBJECT hBuffer.
IF VALID-HANDLE(hBuffLogin) THEN DELETE OBJECT hBuffLogin.

PROCEDURE BuildQuery:
/*****************************************
******************************************/

DEF OUTPUT PARAM obOK  AS LOG NO-UNDO.

DEF VAR cTabList         AS CHAR NO-UNDO.
DEF VAR bWhere           AS LOG  NO-UNDO.
DEF VAR iy               AS INT NO-UNDO.

IF AVAIL (JBoxQEntity) THEN DO:
  RUN GetTabList (JBoxQEntity.iEntityId, INPUT-OUTPUT cTabList).

  ASSIGN cFields      = ""
         cBuffers     = ""
         cLabels      = ""
         cQueryString = "".


  DO ix = 1 TO NUM-ENTRIES(cTabList, ";") - 1:
    cBuffers = cBuffers + ENTRY(2,ENTRY(ix,cTabList,";"), "|") + ";". 

    IF VALID-HANDLE(hBuffer) THEN DELETE OBJECT hBuffer.
    CREATE BUFFER hBuffer FOR TABLE ENTRY(2,ENTRY(ix,cTabList,";"), "|").

    EMPTY TEMP-TABLE ttCriteria.
    bWhere = FALSE.
    FOR FIRST bqRelation
      WHERE bqRelation.iEntityIn = INT(ENTRY(1,ENTRY(ix,cTabList,";"), "|"))
    NO-LOCK:
    END.
    IF bqRelation.cRelation MATCHES "*WHERE*" THEN
      bWhere = TRUE.

    iy = 0.
    FOR EACH JBoxQRelation
        WHERE JBoxQRelation.iEntityId = INT(ENTRY(1,ENTRY(ix,cTabList,";"),"|"))
              NO-LOCK,
        FIRST bqEntity
        WHERE bqEntity.iEntityId = JBoxQRelation.iEntityIn
              NO-LOCK:

      IF bqEntity.cType = "FIELD" THEN DO:
        IF JBoxQRelation.cRelation NE "" THEN DO:

          cLabels = cLabels + bqEntity.cName + ";":U.                      
          cFields = cFields + bqEntity.cDBtable + "|" + bqEntity.cDBfield + "|" + JBoxQRelation.cRelation + ";":U.
        END.
        FOR EACH JBoxQCriteria
            WHERE JBoxQCriteria.iEntityId = bqEntity.iEntityId
                  NO-LOCK:
          hField = hBuffer:BUFFER-FIELD(bqEntity.cDBfield).
          CREATE ttCriteria.
          ASSIGN ttCriteria.iPos       = JBoxQCriteria.iSequence
                 ttCriteria.cTable     = bqEntity.cDBtable
                 ttCriteria.cFieldname = bqEntity.cDBfield
                 ttCriteria.cTabField  = bqEntity.cDBtable + "." + bqEntity.cDBfield
                 ttCriteria.cDataType  = bqEntity.cDataType
                 ttCriteria.cAndOr     = JBoxQCriteria.cAndOr
                 ttCriteria.cOp        = JBoxQCriteria.cOper
                 ttCriteria.cLeftPar   = JBoxQCriteria.cLeftPar
                 ttCriteria.cRightPar  = JBoxQCriteria.cRightPar
                 .
          IF hField:DATA-TYPE = "date" THEN
            CASE SESSION:DATE-FORMAT:
              WHEN "dmy" THEN ttCriteria.cValue  = "DATE('" + ENTRY(2,JBoxQCriteria.cValue,"/") + "/" + ENTRY(1,JBoxQCriteria.cValue,"/") + "/" + ENTRY(3,JBoxQCriteria.cValue,"/") + "')".
              WHEN "ymd" THEN ttCriteria.cValue  = "DATE('" + ENTRY(3,JBoxQCriteria.cValue,"/") + "/" + ENTRY(1,JBoxQCriteria.cValue,"/") + "/" + ENTRY(2,JBoxQCriteria.cValue,"/") + "')".
              OTHERWISE ttCriteria.cValue  = "DATE('" + JBoxQCriteria.cValue + "')".
            END CASE.  
          ELSE IF hField:DATA-TYPE = "decimal" AND INDEX(JBoxQCriteria.cValue,".") > 0 THEN 
             ttCriteria.cValue = "DEC('" + REPLACE(JBoxQCriteria.cValue,".",",") + "')".
          ELSE
             ttCriteria.cValue  = JBoxQCriteria.cValue.
        END.
      END.
      iy = iy + 1.
    END.

    IF iy = 0 THEN cFields = cFields + hBuffer:NAME + ";".

    IF cQueryString = "" THEN
      cQueryString = "FOR ".
    ELSE
      cQueryString = cQueryString + ", ".

    cQueryString = cQueryString + "EACH " + ENTRY(2,ENTRY(ix,cTabList,";"),"|") + " " +
                   bqRelation.cRelation.

    hField = hBuffer:BUFFER-FIELD("iJBoxCompanyId") NO-ERROR.
    IF AVAIL ttCriteria THEN DO:
      IF bWhere THEN   
        cQueryString = cQueryString + " AND ".
      ELSE
        cQueryString = cQueryString + " WHERE ".

      FOR EACH ttCriteria BY ttCriteria.iPos:
        cQueryString = cQueryString + 
                       ttCriteria.cAndOr    + " " +  
                       ttCriteria.cLeftPar  +        
                       ttCriteria.cTabField  + " " + 
                       ttCriteria.cOp       + " " +  
                       ttCriteria.cValue    +        
                       ttCriteria.cRightPar + " "    
                       .
      END.
      IF VALID-HANDLE(hField) THEN
        cQueryString = cQueryString + " AND " + hBuffer:NAME + ".iJBoxCompanyId = " + STRING(iCompanyId).
    END.
    ELSE IF VALID-HANDLE(hField) THEN DO:
      IF bWhere THEN   
        cQueryString = cQueryString + " AND ".
      ELSE
        cQueryString = cQueryString + " WHERE ".
      cQueryString = cQueryString + hBuffer:NAME + ".iJBoxCompanyId = " + STRING(iCompanyId).
    END.

    cQueryString = cQueryString + " NO-LOCK".
  END.
  cQueryString = cQueryString + ".".

  IF cFields = "" THEN DO:
    ocReturn = "Error: No fields selected".
    RETURN.
  END.

END.

END PROCEDURE.

PROCEDURE GetTabList:
/*********************
*********************/

DEF INPUT PARAM ioEntId AS INT  NO-UNDO.
DEF INPUT-OUTPUT PARAM cTabList AS CHAR NO-UNDO.

FOR EACH JBoxQRelation
     WHERE JBoxQRelation.iEntityId = ioEntId
           NO-LOCK,
     FIRST JBoxQEntity
       WHERE JBoxQEntity.iEntityId = JBoxQRelation.iEntityIn
         AND JBoxQEntity.cType = "TABLE"
              NO-LOCK:
  cTabList = cTabList + STRING(JBoxQEntity.iEntityId) + "|":U + JBoxQEntity.cDBtable + ";":U.
  RUN GetTabList (JBoxQEntity.iEntityId , INPUT-OUTPUT cTabList).
END.

END PROCEDURE.
