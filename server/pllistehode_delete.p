/* Sletting av plukkliste
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE  NO-UNDO.
DEF VAR lPlListeId      AS DECIMAL NO-UNDO.

ASSIGN
  lPlListeId  = DECIMAL(ENTRY(1,icParam,';'))
.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  /*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
hvor mange som ble oppettet i artbas og vareboklinje*/
  DO TRANSACTION:
/*    FIND FIRST plListeHode WHERE plListeHode.plListeId = lplListeId*/
/*                   EXCLUSIVE-LOCK NO-ERROR.                        */
                   
                   
   FIND FIRST plListeHode WHERE 
       plListeHode.plListeId = DEC(STRING(ihBuffer:BUFFER-FIELD('plListeId'):BUFFER-VALUE))
       EXCLUSIVE-LOCK NO-ERROR.
                   
    IF AVAIL plListeHode THEN
    DO:
      FOR EACH plListeLinje OF plListeHode EXCLUSIVE-LOCK:
        DELETE plListeLinje.
      END.
      FOR EACH pllisteArtikkel OF plListeHode EXCLUSIVE-LOCK:
        DELETE pllisteArtikkel.
      END.
      FOR EACH pllistemodell OF plListeHode EXCLUSIVE-LOCK:
        DELETE pllistemodell.
      END.
      DELETE plListeHode.      
    END.
  END.
  hQuery:GET-NEXT().
END.
ASSIGN 
  ocReturn = ''
  obOk     = TRUE
.

