/* Save menu favorites
  
  Created 05 jan 12 by brynjar@chemistry.no
-----------------------------------------------------------------------*/                                                                         
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cUserId              AS CHAR NO-UNDO.
DEF VAR cMenuIdList          AS CHAR NO-UNDO.
DEF VAR cType                AS CHAR NO-UNDO.
DEF VAR ix                   AS INT  NO-UNDO.
DEF VAR hBuffMenuFavorites   AS HANDLE NO-UNDO.
DEF VAR hQuery               AS HANDLE NO-UNDO.
DEF VAR hFldType             AS HANDLE NO-UNDO.

ASSIGN cUserId     = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       cMenuIdList = ENTRY(1,icParam,";")
       .

IF NUM-ENTRIES(icParam,";") > 1 THEN
  cType = ENTRY(2,icParam,";").

CREATE BUFFER hBuffMenuFavorites FOR TABLE "JBoxMenuFavorites" NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

hFldType = hBuffMenuFavorites:BUFFER-FIELD("cType") NO-ERROR.

DO TRANSACTION:

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffMenuFavorites).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffMenuFavorites:NAME 
                     + " WHERE cJBoxUserId = '" + cUserId
                     + (IF VALID-HANDLE(hFldType) THEN "' AND cType = '" + cType ELSE "") 
                     + "' AND NOT CAN-DO('" + cMenuIdList + "',STRING(iJBoxMenuId))").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    hBuffMenuFavorites:FIND-CURRENT(EXCLUSIVE-LOCK) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      hBuffMenuFavorites:BUFFER-DELETE().
    hQuery:GET-NEXT().
  END.

  DO ix = 1 TO NUM-ENTRIES(cMenuIdList):
    FIND FIRST JBoxMenu NO-LOCK
         WHERE JBoxMenu.iJBoxMenuId = INT(ENTRY(ix,cMenuIdList))
         NO-ERROR.
    IF AVAIL JBoxMenu THEN DO:
      hBuffMenuFavorites:FIND-FIRST("WHERE cJBoxUserId = '" + cUserId 
                                  + "' AND iJBoxMenuId = " + ENTRY(ix,cMenuIdList) 
                                  + (IF VALID-HANDLE(hFldType) THEN " AND cType = '" + cType + "'" ELSE "")
                                   ,NO-LOCK) NO-ERROR.
      IF NOT hBuffMenuFavorites:AVAIL THEN DO:
        hBuffMenuFavorites:BUFFER-CREATE().
        ASSIGN hBuffMenuFavorites:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE = INT(ENTRY(ix,cMenuIdList))
               hBuffMenuFavorites:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE = cUserId
               hBuffMenuFavorites:BUFFER-FIELD("iSeq"):BUFFER-VALUE = ix
               hBuffMenuFavorites:BUFFER-FIELD("dCreated"):BUFFER-VALUE = TODAY
               hBuffMenuFavorites:BUFFER-FIELD("cCreatedBy"):BUFFER-VALUE = cUserId
               NO-ERROR.
        IF VALID-HANDLE(hFldType) THEN
          hFldType:BUFFER-VALUE = cType.
      END.
    END.
  END.
END.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffMenuFavorites.

obOK = ocReturn = "".

