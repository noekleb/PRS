/* Sletting av ImpKonv poster
   Parameter:  
   Opprettet:               
   Endret:    
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR cTekst      AS CHAR   NO-UNDO.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN 
DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("EDB-System","ImpKonv.EDB-System").
  httTable:ADD-LIKE-FIELD("InterntId","ImpKonv.InterntId").
  httTable:ADD-LIKE-FIELD("EksterntId","ImpKonv.EksterntId").
  httTable:TEMP-TABLE-PREPARE("ttImpKonv").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ImpKonv WHERE ROWID(ImpKonv) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ImpKonv THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ImpKonv:HANDLE).
      END.
    END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().


DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

    FIND FIRST ImpKonv 
         WHERE 
         ImpKonv.EDB-System = STRING(ihBuffer:BUFFER-FIELD("EDB-System"):BUFFER-VALUE) AND
         ImpKonv.Tabell     = STRING(ihBuffer:BUFFER-FIELD("Tabell"):BUFFER-VALUE) AND
         ImpKonv.InterntId  = STRING(ihBuffer:BUFFER-FIELD("InterntId"):BUFFER-VALUE) AND 
         ImpKonv.EksterntId = STRING(ihBuffer:BUFFER-FIELD("EksterntId"):BUFFER-VALUE)  
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAIL ImpKonv THEN
        DELETE ImpKonv.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

