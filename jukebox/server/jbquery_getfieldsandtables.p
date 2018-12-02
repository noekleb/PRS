DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam       AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR httBuffer  AS HANDLE NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.

DEF VAR cBufferName AS CHAR NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.

DEF TEMP-TABLE ttJBoxQRelation LIKE JBoxQRelation.
DEF TEMP-TABLE ttJBoxQCriteria LIKE JBoxQCriteria.
DEF TEMP-TABLE ttJBoxQEntity   LIKE JBoxQEntity.

{incl/validatesession.i}

cBufferName = ENTRY(2,icParam).

RUN BuildTable (INT(ENTRY(1,icParam))).

CREATE TEMP-TABLE hTempTable.
hTempTable:CREATE-LIKE(cBufferName). 
hTempTable:TEMP-TABLE-PREPARE("tt" + cBufferName).

CREATE BUFFER hBuffer FOR TABLE cBufferName.

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


PROCEDURE BuildTable:
/*****************************************
******************************************/

DEF INPUT PARAM iiEntId AS INT NO-UNDO.

DEF VAR bSelected AS LOG NO-UNDO.

FOR EACH JBoxQRelation
   WHERE JBoxQRelation.iEntityId = iiEntId,
   FIRST JBoxQEntity
         WHERE JBoxQEntity.iEntityId = JBoxQRelation.iEntityIn
           AND JBoxQEntity.cType = "TABLE"
   NO-LOCK:

  CREATE ttJBoxQRelation.
  BUFFER-COPY JBoxQRelation TO ttJBoxQRelation.

  CREATE ttJBoxQEntity.
  BUFFER-COPY JBoxQEntity TO ttJBoxQEntity.

  FOR EACH JBoxQCriteria NO-LOCK
      WHERE JBoxQCriteria.iEntityId = iiEntId:

    CREATE ttJBoxQCriteria.
    BUFFER-COPY JBoxQCriteria TO ttJBoxQCriteria.
  END.

  RUN BuildTable(JBoxQRelation.iEntityIn).
END.

END PROCEDURE.
