/* Inndeling av ordrelinjer i pakke
   Parametere: <modus>;<pakkepris>;<rowid-liste for ordrelinjer>;<fordelingstype>;Gjeldende pakkenr;KOrdre_id
              Fordelingstype: 1: DB, 2: Pris (ikke implementert)
   Opprettet: 30.06.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT    NO-UNDO.
DEF VAR iy            AS INT    NO-UNDO.
DEF VAR iLnr          AS INT    NO-UNDO.

DEF VAR cRowidList    AS CHAR   NO-UNDO.
DEF VAR iFordType     AS INT    NO-UNDO.
DEF VAR cModus        AS CHAR   NO-UNDO.
DEF VAR fPakkePris    AS DEC    NO-UNDO.
DEF VAR iPakkeidx     AS INT    NO-UNDO.
DEF VAR fTotDb        AS DEC    NO-UNDO.
DEF VAR fTotPris      AS DEC    NO-UNDO.
DEF VAR fAndelRabKr   AS DEC    NO-UNDO.
DEF VAR hBufTTlinje   AS HANDLE NO-UNDO.
DEF VAR fSumKalkPris  AS DEC    NO-UNDO.
DEF VAR fKOrdre_id    AS DEC    NO-UNDO.

DEF TEMP-TABLE 
    ttKOrdreLinje LIKE KOrdreLinje
    FIELD fAndelDb     AS DEC
    FIELD MvaKrEnhet   AS DEC
    FIELD DbKrEnhet    AS DEC
    FIELD SumEksMvaKr  AS DEC
    FIELD rKOrdreLinje AS ROWID
    .

hBufTTlinje = BUFFER ttKOrdreLinje:HANDLE.

ASSIGN cRowidList = REPLACE(ENTRY(3,icParam,";"),"|",",")
       iFordType  = INT(ENTRY(4,icParam,";"))
       fPakkePris = DEC(ENTRY(2,icParam,";"))
       cModus     = ENTRY(1,icParam,";")
       iPakkeIdx  = INT(ENTRY(5,icParam,";"))
       fKOrdre_id = DEC(ENTRY(6,icParam,";"))
       .

FIND KOrdreHode NO-LOCK
     WHERE KOrdreHode.KOrdre_id = fKOrdre_id
     NO-ERROR.
IF NOT AVAIL KOrdreHode THEN DO:
  ocReturn = "Finner ikke angitt kundeordre".
  RETURN.
END.

FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK
    WHERE CAN-DO(cRowidList,STRING(ROWID(KOrdreLinje))):
  CREATE ttKOrdreLinje.
  BUFFER-COPY KOrdreLinje TO ttKOrdreLinje.
  ttKOrdreLinje.rKOrdreLinje = ROWID(KOrdreLinje).
END.

/* Overfører aktuelle rader til tt og beregner ny db% basert på pris: */
FOR EACH ttKOrdreLinje:
  IF ttKOrdreLinje.VareNr NE "" AND iFordType = 1 THEN 
    ASSIGN ttKOrdreLinje.MvaKrEnhet = ttKOrdreLinje.Pris - ttKOrdreLinje.Pris / (1 + ttKOrdreLinje.Mva% / 100)
           ttKOrdreLinje.DbKrEnhet  = ttKOrdreLinje.Pris - ttKOrdreLinje.MvaKrEnhet - ttKOrdreLinje.Varekost
           ttKOrdreLinje.DbKr       = ttKOrdreLinje.DbKrEnhet * ttKOrdreLinje.Antall
           .
  ELSE IF iFordType = 1 THEN 
    ttKOrdreLinje.DbKr = ttKOrdreLinje.NettoLinjesum * 0.35.
    
  ASSIGN fTotDb   = fTotDb + ttKOrdreLinje.DbKr
         fTotPris = fTotPris + ttKOrdreLinje.Pris * ttKOrdreLinje.Antall.
END.

/* Finner andel av total db for hver vare: */
FOR EACH ttKOrdreLinje:
  ASSIGN ttKordreLinje.fAndelDb = ttKordreLinje.DbKr / fTotDb
         ix = ix + 1.
END.

/* Setter totalprisen på artikkel ut fra andeld dbkr: */
FOR EACH ttKordreLinje
    BY ttKOrdreLinje.fAndelDb:
  ASSIGN ttKOrdreLinje.NettoPris  = (ttKordreLinje.Pris * ttKOrdreLinje.Antall - (fTotPris - fPakkePris) * ttKOrdreLinje.fAndelDb) / ttKOrdreLinje.Antall
         iy = iy + 1.
/*          ttKOrdreLinje.BruttoPris = ttKOrdreLinje.Pris  */
         .

  /* Bruker standard rutine for kalkulering - denne gang for temp-tabellen: */
  RUN kordrelinje_post_update.p (hBufTTlinje,"","",OUTPUT ocReturn).
  
  ASSIGN ttKOrdreLinje.SumEksMvaKr = ttKOrdreLinje.NettoLinjeSum - ttKOrdreLinje.MvaKr
         fSumKalkPris = fSumKalkPris + ttKOrdreLinje.NettoLinjeSum
         .

  /* Plasserer avrunding på linje med størst andel db: */
  IF iy = ix AND fSumKalkPris NE fPakkePris THEN 
    ttKOrdreLinje.NettoLinjeSum = ttKOrdreLinje.NettoLinjeSum - (fSumKalkPris - fPakkePris).


  /* Temp-tabell skal returneres for å kunne benytte samme rutine for summering som ellers: */
  IF cModus = "test" THEN DO:
    ihBuffer:BUFFER-CREATE().
    ihBuffer:BUFFER-COPY(hBufTTlinje).
  END.
END.

IF cModus NE "test" THEN Oppdatering: DO TRANSACTION ON ERROR UNDO, LEAVE:
  /* Fjerner først varer som ikke lenger er med i pakken (de settes på nytt med beste pris): */
  IF iPakkeidx NE 0 THEN
    FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK
        WHERE KOrdreLinje.Pakkeidx = iPakkeidx
          AND NOT CAN-DO(cRowidList,STRING(ROWID(KOrdreLinje))):
      RUN kordrelinje_set_varenr.p (STRING(ROWID(KOrdreLinje)) + "|" + KOrdreLinje.Varenr + "|" + KOrdreLinje.Storl,
                                    ?,
                                    icSessionId,
                                    OUTPUT ocReturn,
                                    OUTPUT obOk).
      IF NOT obOk THEN UNDO, LEAVE. 
      ELSE ocReturn = "".
      KOrdreLinje.Pakkeidx = 0.
    END.
  ELSE DO:  /* Finner høyest pakkeindex: */
    FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK
        BY KOrdreLinje.Pakkeidx DESC:
      iPakkeidx = KOrdreLinje.Pakkeidx + 1.
      LEAVE.
    END.
  END.

  FOR EACH ttKOrdreLinje:
    FIND KOrdreLinje EXCLUSIVE-LOCK
         WHERE ROWID(KOrdreLinje) = ttKOrdreLinje.rKOrdreLinje
         NO-ERROR.
    IF NOT AVAIL KOrdreLinje THEN DO:
      ocReturn = "Ordrelinje ikke tilgjengelig for oppdatering".
      UNDO, LEAVE Oppdatering.
    END.
    BUFFER-COPY ttKOrdreLinje TO KOrdreLinje.
    KOrdreLinje.Pakkeidx = iPakkeidx.
  END.
END.

obOk = ocReturn = "".

FUNCTION getCurrentValueFields RETURNS CHARACTER():
  RETURN "NettoPris".
END FUNCTION.
