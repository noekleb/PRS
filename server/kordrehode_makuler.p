/* kordrehode_makuler.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cNettButikkType AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE lNekad          AS LOG NO-UNDO.
DEFINE VARIABLE lPs12           AS LOG NO-UNDO.
DEFINE VARIABLE pcOldLevStatus  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFraBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iTilbut AS INTEGER NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

ASSIGN 
    obOk     = TRUE
    cTekst   = ENTRY(1,icParam,'|')
    .
{syspara.i 150 1 20 cNettButikkType} /* 1 = Gant, 2 = JF */
{syspara.i 150 1 3 iTilBut INT}

SUBSCRIBE "getFraTilbutikkReturKOrdre" ANYWHERE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
LOOPEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    lNekad   = FALSE 
    lPs12    = FALSE
    ocReturn = ''
    .
    
  FIND FIRST KordreHode NO-LOCK WHERE 
      KordreHode.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE) NO-ERROR.
  IF AVAIL KOrdreHode THEN
  AVAILKORDREHODE:
  DO:    
    ASSIGN 
      iFraBut = KOrdreHode.ButikkNr
      .
    
    /* For JF. */    
    IF KOrdreHode.Opphav = 10 AND 
       cNettButikkType = "2" /* PRS nettbutikk */ THEN 
    DO:
        IF CAN-FIND(FIRST kordrelinje WHERE 
                    kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 2) THEN
            lNekad = TRUE.
        IF CAN-FIND(FIRST kordrelinje WHERE 
                    kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 0 AND 
                    KOrdrelinje.plockstatus < 3) THEN
            lPs12 = TRUE.
        IF lNekad AND lPs12 THEN 
            NEXT LOOPEN.
    END.

    /* Også JF funksjon. */
    IF KOrdreHode.Opphav = 10 AND 
       INT(KOrdreHode.LevStatus) <= 50 AND 
       cNettButikkType = "2" AND 
       lNekad THEN 
    DO:
        IF INT(KOrdreHode.LevStatus) < 50 THEN 
        DO:
            rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.Kordre_Id),
                                                       INPUT 60).  
            FOR EACH kordrelinje WHERE 
                kordrelinje.kordre_id = KOrdreHode.Kordre_id AND 
                KOrdrelinje.plockstatus > 0 
                USE-INDEX FaktLinje:
                KOrdrelinje.plockstatus = 0.
            END.
        END.
        NEXT LOOPEN.
    END.
    
    /* Generell håndtering - Gjelder også Gant. */
    IF (INT(KOrdreHode.LevStatus) < 50 OR INT(KOrdreHode.LevStatus) = 55) THEN 
    DO:
      
        /* Tar vare på gammel status. */
        ASSIGN
            pcOldLevStatus = KOrdreHode.LevStatus
            .
        IF KOrdreHode.Opphav = 10 THEN 
        DO TRANSACTION:
            FIND CURRENT KOrdreHode EXCLUSIVE-LOCK.
            rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.Kordre_Id),
                                                       INPUT 60).
            ASSIGN 
                KOrdreHode.SendingsNr =  "MAKULERT30" /* + pcOldLevStatus */
                KOrdreHode.Kundeservice = FALSE
                KOrdreHode.VerkstedMerknad = 'Kanselert ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' av ' + 
                                             USERID('skotex')   
                                             + ' ' + cTekst + 
                                             (IF KOrdreHode.VerkstedMerknad <> '' THEN CHR(10) ELSE '') + KOrdreHode.VerkstedMerknad
                .
            FIND CURRENT KOrdreHode NO-LOCK.
            RUN ovordre_reservervarer.p (STRING(KOrdreHode.KOrdre_Id),TRUE) NO-ERROR.
            FOR EACH kordrelinje WHERE 
                kordrelinje.kordre_id = KOrdreHode.Kordre_id AND 
                KOrdrelinje.plockstatus > 0 
                USE-INDEX FaktLinje:
                KOrdrelinje.plockstatus = 0.
            END.
        END. /* TRANSACTION */
        /* Vanlig makulering */
        ELSE rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.Kordre_Id),
                                                         INPUT 60).
    END.
  END. /* AVAILKORDREHODE */

  IF AVAIL KOrdreHode THEN 
    RELEASE KOrdreHode.
  hQuery:GET-NEXT().
END. /* LOOPEN */

