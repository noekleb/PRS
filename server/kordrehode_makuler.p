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

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

ASSIGN 
    obOk     = TRUE
    .

cNettButikkType = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                        "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20","Parameter1")).

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
    IF INT(KOrdreHode.LevStatus) <= 50 THEN 
    DO:
        /* Tar vare på gammel status. */
        ASSIGN
            pcOldLevStatus = KOrdreHode.LevStatus
            .
    
        /* Makulering av hele ordren for Nettbutikk. */
        IF (pcOldLevStatus = '50' AND KOrdreHode.Opphav = 10) THEN 
        DO TRANSACTION:
            FIND CURRENT KOrdreHode EXCLUSIVE-LOCK.
            rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.Kordre_Id),
                                                       INPUT 60).
            ASSIGN 
                KOrdreHode.SendingsNr =  "MAKULERT50".
            FIND CURRENT KOrdreHode NO-LOCK.
            RUN kordre_makuler.p(KOrdreHode.KOrdre_Id, pcOldLevStatus).
        END. /* TRANSACTION */
        
        /* Makulering av hele ordren for Nettbutikk. */
        ELSE IF (pcOldLevStatus = '30' AND KOrdreHode.Opphav = 10) THEN 
        DO TRANSACTION:
            FIND CURRENT KOrdreHode EXCLUSIVE-LOCK.
            rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.Kordre_Id),
                                                       INPUT 60).  
            ASSIGN 
                KOrdreHode.SendingsNr =  "MAKULERT30".
            FIND CURRENT KOrdreHode NO-LOCK.
            RUN opprett_overforingsordre.p(KOrdreHode.KOrdre_id),TRUE).
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

