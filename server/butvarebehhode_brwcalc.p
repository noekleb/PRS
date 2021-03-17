DEF VAR bSubselectKordreDone AS LOG NO-UNDO.

DEF TEMP-TABLE ttVarebehKOrdre
    FIELD fVarebehNr  AS DEC
    INDEX idxVarebehNr fVarebehNr.

PROCEDURE SubselectKundeordre:
  /* Utføres kun en gang for å filtrere ut suppl.bøker knyttet til kundeordre */
  FOR EACH KOrdreLinje FIELDS(BestNr) NO-LOCK
      WHERE KOrdreLinje.LeveringsDato NE ?
        AND KOrdreLinje.BestNr > 0
     ,FIRST BestHode FIELDS(VarebehNr) NO-LOCK
            OF KOrdreLinje:
    CREATE ttVarebehKOrdre.
    ttVarebehKOrdre.fVarebehNr = BestHode.VarebehNr.
  END.

  bSubselectKordreDone = YES.
END PROCEDURE.

PROCEDURE butvarebeh_kundeordrefilter:
  DEF INPUT  PARAM irVarebehHode       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icVarebehHodeFilter AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.
  
  IF icVarebehHodeFilter NE "" THEN DO:
    FIND VarebehHode NO-LOCK
         WHERE ROWID(VarebehHode) = irVarebehHode
         NO-ERROR.
    IF AVAIL VarebehHode THEN DO:
      IF NOT bSubselectKordreDone THEN RUN SubselectKundeordre.
    
      IF NOT CAN-FIND(FIRST ttVarebehKOrdre
                      WHERE ttVarebehKOrdre.fVarebehNr = VarebehHode.VarebehNr)
         THEN ocValue = "skiprow".
    END.
  END.

END PROCEDURE.

PROCEDURE butvarebehhode_messefilter:
  /* Bruk av parametere:
     entry(1,"¤"): Butikkliste
     entry(2,"¤"): Leverandørliste
     Dersom en av listene er ulik * og det fins en messe knyttet til vareh.boken
     så skal posten vises bare hvis "åpningstiden" for registrering er nå
  ----------------------------------------------------------------------------------------------------------------------*/      
  DEF INPUT  PARAM irVarebehhode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam       AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  DEF VAR cButikkListe AS CHAR NO-UNDO.
  DEF VAR cLevNrListe  AS CHAR NO-UNDO INIT "*".

  cButikkListe = REPLACE(ENTRY(1,icParam,"¤"),"&",",").
  IF NUM-ENTRIES(icParam,"¤") > 1 THEN
    cLevNrListe  = REPLACE(ENTRY(2,icParam,"¤"),"&",",").

  IF cButikkListe NE "*" OR cLevNrListe NE "*" THEN DO:
    FIND FIRST Varebehhode WHERE ROWID(Varebehhode) = irVarebehhode NO-LOCK NO-ERROR.
    IF AVAIL VarebehHode THEN DO:
      FIND FIRST Messe OF VarebehHode NO-LOCK NO-ERROR.
      IF AVAIL Messe THEN DO:
        IF (PubliserStartDato > TODAY OR TODAY > PubliserStoppDato) OR PubliserStartDato = ? OR PubliserStoppDato = ? OR Messe.MesseType NE 2 THEN ocValue = "skiprow".
        ELSE IF cButikkListe NE "*" AND 
                CAN-FIND(FIRST MesseForButikk OF Messe) AND
                NOT CAN-FIND(FIRST MesseForButikk OF Messe WHERE CAN-DO(cButikkListe,STRING(MesseForButikk.ButikkNr)))
                THEN ocValue = "skiprow".
      END.
    END.
  END.

END PROCEDURE.

PROCEDURE butvarebeh_levfilter:
  DEF INPUT  PARAM irVarebehHode       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icVarebehHodeFilter AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

  DEF VAR ix      AS INT NO-UNDO.
  DEF VAR bFunnet AS LOG NO-UNDO.

  IF icVarebehHodeFilter NE "" THEN DO:
    FOR FIRST VarebehHode FIELDS(VarebehNr)
        WHERE ROWID(VarebehHode) = irVarebehHode NO-LOCK:
      DO ix = 1 TO NUM-ENTRIES(icVarebehHodeFilter,"¤"):
        IF CAN-FIND(FIRST VarebehLinje OF VarebehHode WHERE VarebehLinje.LevNr = INT(ENTRY(ix,icVarebehHodeFilter,"¤"))) THEN DO:
          bFunnet = TRUE.
          LEAVE.
        END.
      END.
    END.
    IF NOT bFunnet THEN ocValue = "skiprow".
  END.
END PROCEDURE.
