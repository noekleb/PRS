/* kordrehode_LeverSpeditor.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iStatusLst AS INTEGER NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE bSTvang AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

obOk = rKundeordreBehandling:sjekkTvang( OUTPUT iStatusLst, OUTPUT bSTvang ).  

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

/* Er det tvang, og noen av postene ikke har fått skrevet ut pakkseddel, skal det varsles om det og avsluttes. */
IF bSTvang THEN 
DO:
    obOk = FALSE. 
    hQuery:GET-FIRST().
    SJEKKLOOP:
    REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE:
        IF ihBuffer:BUFFER-FIELD('levStatus'):BUFFER-VALUE < '42' THEN 
        DO:
            obOk = TRUE.
            LEAVE SJEKKLOOP.
        END.
        hQuery:GET-NEXT().
    END. /* SJEKKLOOP */
    IF obOk = TRUE THEN 
    DO:
        obOk = FALSE.
        ocReturn = 'En eller flere av de valgte kundeordre mangler utskrift av pakkseddel, postpakke etikett og/eller er ikke plukket.'.
        RETURN.    
    END.
END.

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

  FIND FIRST KordreHode WHERE 
      KordreHode.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)
      NO-LOCK NO-ERROR.
  
  IF AVAIL KOrdreHode THEN
  DO:    
    obOk = rKundeordreBehandling:plukkKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id)).  

    IF obOk THEN 
    DO TRANSACTION:
        FIND CURRENT KOrdreHode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE KOrdreHode AND NOT LOCKED KOrdreHode THEN 
        DO:
            rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id),
                                                       INPUT IF ((KOrdreHode.LevStatus < '45' OR KOrdreHode.LevStatus = '55') AND iStatusLst = 15) THEN 45 ELSE INT(KOrdreHode.LevStatus)).  
        END. 
        IF AVAILABLE KOrdreHode THEN
            FIND CURRENT KOrdreHode NO-LOCK NO-ERROR NO-WAIT.
    END. /* TRANSACTION */

    obOk = NOT ERROR-STATUS:ERROR.
    IF NOT obOk THEN
    DO:
      ocReturn = ERROR-STATUS:GET-MESSAGE(1).
      LEAVE.
    END.
  END.

  IF AVAIL KOrdreHode THEN RELEASE KOrdreHode.
  hQuery:GET-NEXT().
END.

