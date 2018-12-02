DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icEntityId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.

DEF VAR bOK        AS LOG NO-UNDO.


DEF VAR iiEntityId AS INT NO-UNDO.

DEF BUFFER bJBoxQEntity FOR JBoxQEntity.

iiEntityId = INT(icEntityId).

{incl/validatesession.i}

{incl/ttjbquerytables.i}

RUN BuildTTselected(iiEntityId,0).

CREATE TEMP-TABLE hTempTable.
hTempTable:CREATE-LIKE("ttSelected"). 
hTempTable:TEMP-TABLE-PREPARE("ttSelected").

CREATE BUFFER hBuffer FOR TABLE "ttSelected".

httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  httBuffer:BUFFER-CREATE.
  httBuffer:BUFFER-COPY(hBuffer).
  hQuery:GET-NEXT().
END.

DELETE OBJECT hBuffer.
DELETE OBJECT hQuery.

PROCEDURE BuildTTselected:
  DEF INPUT PARAM iiEntId AS INT NO-UNDO.
  DEF INPUT PARAM iiLevel AS INT NO-UNDO.

  iiLevel = iiLevel + 1.

  FOR EACH JBoxQRelation
          WHERE JBoxQRelation.iEntityId = iiEntId
              NO-LOCK,
       FIRST JBoxQEntity
            WHERE JBoxQEntity.iEntityId = JBoxQRelation.iEntityIn
              AND JBoxQEntity.cType = "TABLE"
                NO-LOCK:

    CREATE ttSelected.

    ASSIGN ttSelected.iEntityId   = JBoxQRelation.iEntityIn
           ttSelected.iLevel      = iiLevel
           ttSelected.cName       = (IF iiLevel > 1 THEN FILL(" ",2 * iiLevel) ELSE "") + JBoxQEntity.cDBtable
           ttSelected.cDesc       = JBoxQEntity.cDesc
           ttSelected.cRelation   = JBoxQRelation.cRelation
           ocReturn               = ocReturn + ttSelected.cName + ";":U
           .
    FOR FIRST bJBoxQEntity
          WHERE bJBoxQEntity.iEntityId = iiEntId 
            AND bJBoxQEntity.cType NE "VIEW"
              NO-LOCK:
    END.
    IF AVAIL bJBoxQEntity THEN
      ttSelected.cMotherName = bJBoxQEntity.cName.

    RUN BuildTTselected(JBoxQRelation.iEntityIn, iiLevel).

  END.
  
END PROCEDURE.
