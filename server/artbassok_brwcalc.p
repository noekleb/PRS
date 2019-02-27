DYNAMIC-FUNCTION("setCheckForSkipRow" IN SOURCE-PROCEDURE,YES).

PROCEDURE artbas_sjekk_strbeh:

  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  DEF VAR cStrList     AS CHAR NO-UNDO.
  DEF VAR bLager       AS LOG  NO-UNDO.
  DEF VAR iButikkNr    AS INT  NO-UNDO.
  DEF VAR iSumbeh      AS INT  NO-UNDO.
  DEF VAR cButikkListe AS CHAR NO-UNDO.
  DEF VAR iButAnt      AS INT  NO-UNDO EXTENT 200.
  DEF VAR ix           AS INT  NO-UNDO.
  DEF VAR bStrFound    AS LOG  NO-UNDO.

  ASSIGN cStrList     = REPLACE(ENTRY(1,icParam,"¤"),"&",",")
         bLager       = LOGICAL(ENTRY(2,icParam,"¤"))
         iButikkNr    = INT(ENTRY(3,icParam,"¤"))
         NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  FOR FIRST ArtBas FIELDS(ArtikkelNr) NO-LOCK
      WHERE ROWID(ArtBas) = irBuffer:
    IF cStrList NE "" THEN DO:
      FOR EACH ArtLag FIELDS(lagant butik) NO-LOCK
          WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr
            AND CAN-DO(cStrList,TRIM(ArtLag.Storl)):
        IF iButikkNr > 0 AND ArtLag.butik NE iButikkNr THEN NEXT.
        ELSE IF iButikkNr = -1 THEN DO:
          IF NOT CAN-DO(cButikkListe,STRING(ArtLag.butik)) THEN
            cButikkListe = cButikkListe + STRING(ArtLag.butik) + ",".
          iButAnt[LOOKUP(STRING(ArtLag.butik),cButikkListe)] = iButAnt[LOOKUP(STRING(ArtLag.butik),cButikkListe)] + ArtLag.lagant.
        END.
        ASSIGN iSumBeh = iSumBeh + ArtLag.lagant
               bStrFound = TRUE.
      END.
      IF NOT bStrFound THEN ocReturn = "skiprow".
    END.
    ELSE FOR EACH Lager FIELDS(lagant butik) NO-LOCK
             WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr:
      IF iButikkNr > 0 AND Lager.butik NE iButikkNr THEN NEXT.
      ELSE IF iButikkNr = -1 THEN DO:
        IF NOT CAN-DO(cButikkListe,STRING(Lager.butik)) THEN
          cButikkListe = cButikkListe + STRING(Lager.butik) + ",".
        iButAnt[LOOKUP(STRING(Lager.butik),cButikkListe)] = iButAnt[LOOKUP(STRING(Lager.butik),cButikkListe)] + Lager.lagant.
      END.
      iSumBeh = iSumBeh + Lager.lagant.
    END.
  END.

  IF NOT ocReturn = "skiprow" THEN DO:
    IF bLager AND iSumBeh LE 0 THEN ocReturn = "skiprow".
    ELSE IF iButikkNr = -1 THEN DO:
      cButikkListe = TRIM(cButikkListe,",").
      DO ix = 1 TO NUM-ENTRIES(cButikkListe):
        IF iButAnt[ix] NE 0 THEN
          ocReturn = ocReturn + ENTRY(ix,cButikkListe) + ": " + STRING(iButAnt[ix]) + " ".
      END.
    END.
    ELSE ocReturn = STRING(iSumBeh).
  END.

END PROCEDURE.

PROCEDURE artpris_pris_2:
  DEF INPUT PARAM  irBuffer    AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam     AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  DEF VAR iCl      AS INT NO-UNDO.
  DEF VAR fMaxPris AS DEC NO-UNDO.
  DEF VAR fPris    AS DEC NO-UNDO.
  DEF VAR iButikkNr AS INT NO-UNDO.

  ASSIGN iButikkNr = INTEGER(ENTRY(1,icParam,"¤")) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.
  
  ASSIGN fMaxPris  = DECIMAL(ENTRY(2,icParam,"¤")) NO-ERROR.

  FIND Butiker NO-LOCK WHERE
       Butiker.Butik = iButikkNr NO-ERROR.

  IF AVAILABLE Butiker THEN
    FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
        WHERE ROWID(ArtBas) = irBuffer:

      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
      IF AVAILABLE ArtPris THEN
        fPris = ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1].
    END.

  IF fPris = 0 THEN DO:
    {syspara.i 5 1 1 iCl INT}.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iCl NO-ERROR.

    IF AVAILABLE Butiker THEN
      FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
          WHERE ROWID(ArtBas) = irBuffer:

        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF AVAILABLE ArtPris THEN
          fPris = ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1].
      END.
  END.

  IF fMaxPris > 0 AND fPris > fMaxPris THEN ocValue = "skiprow".
  ELSE ocValue = STRING(fPris).
END PROCEDURE.

PROCEDURE artpris_innpris_2:
  DEF INPUT PARAM  irBuffer    AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam     AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  DEF VAR iCl      AS INT NO-UNDO.
  DEF VAR fMinPris AS DEC NO-UNDO.
  DEF VAR fMaxPris AS DEC NO-UNDO.
  DEF VAR fPris    AS DEC NO-UNDO.
  DEF VAR iButikkNr AS INT NO-UNDO.
  
  ASSIGN iButikkNr = INTEGER(ENTRY(1,icParam,"¤"))
   NO-ERROR.
  FIND Butiker NO-LOCK WHERE
       Butiker.Butik = iButikkNr NO-ERROR.

  IF AVAILABLE Butiker THEN
    FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
        WHERE ROWID(ArtBas) = irBuffer:
      
    FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
      IF AVAILABLE ArtPris THEN
        fPris = ArtPris.innkjopsPris[IF Artpris.Tilbud THEN 2 ELSE 1].
    END.
  
  IF fPris = 0 THEN DO:
    {syspara.i 5 1 1 iCl INT}.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iCl NO-ERROR.

    IF AVAILABLE Butiker THEN
      FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
          WHERE ROWID(ArtBas) = irBuffer:

        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF AVAILABLE ArtPris THEN
          fPris = ArtPris.innkjopsPris[IF Artpris.Tilbud THEN 2 ELSE 1].
      END.
  END.
/*   IF fMinPris > 0 AND fPris < fMinPris THEN   */
/*     ocValue = "skiprow".                      */
/*   ELSE                                        */
/*     IF fMaxPris > 0 AND fPris > fMaxPris THEN */
/*       ocValue = "skiprow".                    */
/*     ELSE                                      */
      ocValue = STRING(fPris).
END PROCEDURE.

PROCEDURE art_paa_tilbud:
  DEF INPUT  PARAM irBuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR  NO-UNDO.

  DEF VAR iCl       AS INT NO-UNDO.
  DEF VAR bKunTilb  AS LOG NO-UNDO.
  DEF VAR iButikkNr AS INT NO-UNDO.

  iButikkNr = INTEGER(ENTRY(1,icParam,"¤")).
  IF NUM-ENTRIES(icParam,"¤") > 1 THEN
    bKunTilb  = LOGICAL(ENTRY(2,icParam,"¤")).

  FIND Butiker NO-LOCK WHERE
       Butiker.Butik = iButikkNr NO-ERROR.

  IF AVAILABLE Butiker THEN
    FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
        WHERE ROWID(ArtBas) = irBuffer:

      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
      IF AVAILABLE ArtPris THEN
        ocValue = IF bKunTilb AND NOT ArtPris.Tilbud THEN "skiprow" ELSE STRING(Artpris.Tilbud).
    END.

  IF ocValue = "" THEN DO:
    {syspara.i 5 1 1 iCl INT}.
    FIND Butiker NO-LOCK WHERE
         Butiker.Butik = iCl NO-ERROR.

    IF AVAILABLE Butiker THEN
      FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
          WHERE ROWID(ArtBas) = irBuffer:

        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF AVAILABLE ArtPris THEN
          ocValue = IF bKunTilb AND NOT ArtPris.Tilbud THEN "skiprow" ELSE STRING(Artpris.Tilbud).
      END.
  END.
END PROCEDURE.

PROCEDURE art_tidl_kjopt:
  DEF INPUT  PARAM irBuffer    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR  NO-UNDO.

  DEF VAR iCl         AS INT   NO-UNDO.
  DEF VAR bKunKjopt   AS LOG   NO-UNDO.
  DEF VAR iButikkNr   AS INT   NO-UNDO.
  DEF VAR iAntMnd     AS INT   NO-UNDO.
  DEF VAR dForsteKjop AS DATE  NO-UNDO.
  DEF VAR fKundeNr    AS DEC   NO-UNDO.

  ASSIGN iButikkNr = INTEGER(ENTRY(1,icParam,"¤"))
         bKunKjopt = LOGICAL(ENTRY(2,icParam,"¤"))
         iAntMnd   = INTEGER(ENTRY(3,icParam,"¤"))
         fKundeNr  = DECIMAL(ENTRY(4,icParam,"¤"))
         NO-ERROR.
  IF (ERROR-STATUS:ERROR OR NOT bKunKjopt) THEN 
    RETURN.

  IF iAntMnd NE 0 THEN
    dForsteKjop = TODAY - iAntMnd * 30.
  ELSE dForsteKjop = 01/01/1900.


  FOR EACH ArtBas FIELDS (ArtikkelNr) NO-LOCK
      WHERE ROWID(ArtBas) = irBuffer:
    IF iButikkNr NE 0 AND NOT CAN-FIND(FIRST kundetrans 
                      WHERE kundetrans.KundeNr    = fKundeNr
                        AND kundetrans.Butik      = iButikkNr
                        AND kundetrans.ArtikkelNr = artbas.ArtikkelNr
                        AND kundetrans.Tid        GE 0
                        AND kundetrans.Dato       GT dForsteKjop) THEN
        ocValue = "skiprow".
    ELSE IF iButikkNr = 0 AND NOT CAN-FIND(FIRST kundetrans 
                      WHERE kundetrans.KundeNr    = fKundeNr
                        AND kundetrans.Butik      > 0
                        AND kundetrans.ArtikkelNr = artbas.ArtikkelNr
                        AND kundetrans.Tid        GE 0
                        AND kundetrans.Dato       GT dForsteKjop) THEN
        ocValue = "skiprow".
  END.
END PROCEDURE.
