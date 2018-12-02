/* Registrer Slett kundekort record
   Parameter:  
   Opprettet: 5.4.2011             
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
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST Kundekort WHERE KundeKort.KortNr = STRING(ihBuffer:BUFFER-FIELD('KortNr'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL KundeKort THEN
    SLETTEBLOKK:
    DO:
      /* Er det registrert kundetranser o.l., skal ikke kortet slettes */
      /*
      IF CAN-FIND(FIRST KundeTrans WHERE
                  KundeTrans.KortNr = KundeKort.KortNr) THEN
          LEAVE SLETTEBLOKK. 
      */
      FIND Kunde OF KundeKort NO-LOCK NO-ERROR.

      FIND FIRST MedlemsKort EXCLUSIVE-LOCK WHERE 
          MedlemsKort.InterntKKortId = KundeKort.InterntKKortId NO-ERROR.
      
      /* Er det registrert medlemstransaksjoner, skal ikke kortet slettes */ 
      IF AVAILABLE Medlemskort THEN
      DO:
          /*
          IF CAN-FIND(FIRST MedTrans WHERE
                      MedTrans.KortNr = MedlemsKort.KortNr) THEN
              LEAVE SLETTEBLOKK. 
          */
      END.
      
      /* DØDEN */
      IF AVAILABLE MedlemsKort THEN
          DELETE MedlemsKort.
      DELETE Kundekort NO-ERROR.
      
    END. /* SLETTEBLOKK*/
    
    obOk = NOT ERROR-STATUS:ERROR.
    IF NOT obOk THEN
    DO:
      ocReturn = ERROR-STATUS:GET-MESSAGE(1).
      LEAVE.
    END.
  END.
  IF AVAIL KundeKort THEN RELEASE KundeKort.
  hQuery:GET-NEXT().
END.

