/* Registrer Slett tellelinje record
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

  MESSAGE 'kordrehode_plukkliste.p'
      DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  DO:
    FIND FIRST KordreHode WHERE 
        KordreHode.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    
    IF AVAIL KOrdreHode THEN
    DO:
      MESSAGE 'TEST'
          KordreHode.KOrdre_Id    
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL KOrdreHode THEN RELEASE KOrdreHode.
  hQuery:GET-NEXT().
END.

