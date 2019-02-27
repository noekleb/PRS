/* Samling av alle prosedyrer som kalkulerer verdier for vareh.bok supplering 
   for browser på artikkelnivå (Registrering)
   Opprettet: 05.05.06 av BHa
   Endret:    27.04.07 av BHa:
              - Rettet feil i kalkulasjonsprosedyre for antall bestilt
                Hvis det ikke fantes bestilling for varen på butikken
-------------------------------------------------------------------*/   
FUNCTION SjekkButListe RETURNS LOGICAL (INPUT iiBestNr AS INT,INPUT icButListe AS CHAR): 
  /* Ekskluder BESTILLINGSRAD dersom det er en butikkliste og bestillingslinje ikke matcher butikk */

  DEF VAR bMatch   AS LOG  NO-UNDO.

  IF icButListe NE "*" AND CAN-FIND(BestHode WHERE BestHode.BestNr = iiBestNr) THEN DO:
    FOR EACH BestLinje NO-LOCK
        WHERE BestLinje.BestNr = iiBestNr
          AND CAN-DO(icButListe,STRING(BestLinje.Butik)):
      bMatch = YES.
    END.
  END.
  ELSE bMatch = YES.

  RETURN bMatch.

END FUNCTION.


PROCEDURE butvarebehlinje_antall:
  /* Bruk av parametere:
     entry(1,"¤"): Fra registreringsdato, bestilling
     entry(2,"¤"): Til registreringsdato, bestilling
     entry(3,"¤"): Bestillingssatatus
     entry(4,"¤"): Butikkliste 
  ----------------------------------------------------------------------------------------------------------------------*/   

  DEF INPUT  PARAM irVarebehLinje   AS ROWID NO-UNDO.
  DEF INPUT  PARAM icBestHodeFilter AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId      AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue          AS CHAR NO-UNDO.

  DEF VAR fAntall       AS DEC  NO-UNDO.
  DEF VAR dRegFra       AS DATE NO-UNDO.
  DEF VAR dRegTil       AS DATE NO-UNDO.
  DEF VAR bFunnet       AS LOG  NO-UNDO.
  DEF VAR cButikkListe  AS CHAR NO-UNDO.
  DEF VAR iBestStat     AS INT  NO-UNDO.

  IF icBestHodeFilter NE "" THEN DO:
    IF ENTRY(1,icBestHodeFilter,"¤") NE "" THEN
      dRegFra = DATE(ENTRY(1,icBestHodeFilter,"¤")).
    IF ENTRY(2,icBestHodeFilter,"¤") NE "" THEN
      dRegTil = DATE(ENTRY(2,icBestHodeFilter,"¤")).

    ASSIGN iBestStat    = INT(ENTRY(3,icBestHodeFilter,"¤"))
           cButikkListe = REPLACE(ENTRY(4,icBestHodeFilter,"¤"),CHR(1),",").
  END.

  FOR FIRST VarebehLinje FIELDS()
      WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK,
      FIRST VarebehHode FIELDS(VareBehType) NO-LOCK OF VarebehLinje,
            EACH VareBehBestHode NO-LOCK OF VarebehLinje
            WHERE (IF dRegFra NE ? THEN VarebehBestHode.RegistrertDato GE dRegFra ELSE TRUE) 
              AND (IF dRegTil NE ? THEN VarebehBestHode.RegistrertDato LE dRegTil ELSE TRUE):

    IF SjekkButListe(VareBehBestHode.BestNr,cButikkListe) THEN DO:
      IF VarebehHode.VarebehType = 2 THEN
        fAntall = fAntall + VarebehBestHode.AntLevert.
      ELSE 
        FOR FIRST BestHode FIELDS(TotAntPar BestStat) NO-LOCK OF VarebehBestHode
            WHERE (IF iBestStat = 4 THEN BestHode.BestStat = iBestStat AND BestHode.BekreftetDato = ?
                   ELSE IF iBestStat = 44 THEN BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?
                   ELSE IF iBestStat > 0 THEN BestHode.BestStat = iBestStat
                   ELSE TRUE):
          fAntall = fAntall + BestHode.TotAntPar.
        END.
      bFunnet = TRUE.
    END.
/*     ELSE bFunnet = ?. */
  END.
  IF (NOT bFunnet AND (dRegFra NE ? OR dRegTil NE ?)) OR bFunnet = ? THEN
    ocValue = "skiprow".
  ELSE
    ocValue = STRING(fAntall).
END PROCEDURE.

PROCEDURE butvarebehlinje_varekost:
  /* Bruk av parametere:
     entry(1,"¤"): Fra registreringsdato, bestilling
     entry(2,"¤"): Til registreringsdato, bestilling
     entry(3,"¤"): Bestillingssatatus
     entry(4,"¤"): Butikkliste 
  ----------------------------------------------------------------------------------------------------------------------*/   

  DEF INPUT  PARAM irVarebehLinje   AS ROWID NO-UNDO.
  DEF INPUT  PARAM icBestHodeFilter AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId      AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue          AS CHAR NO-UNDO.

  DEF VAR fSumKost      AS DEC NO-UNDO.
  DEF VAR dRegFra       AS DATE NO-UNDO.
  DEF VAR dRegTil       AS DATE NO-UNDO.
  DEF VAR cButikkListe  AS CHAR NO-UNDO.
  DEF VAR iBestStat     AS INT  NO-UNDO.

  IF icBestHodeFilter NE "" THEN DO:
    IF ENTRY(1,icBestHodeFilter,"¤") NE "" THEN
      dRegFra = DATE(ENTRY(1,icBestHodeFilter,"¤")).
    IF ENTRY(2,icBestHodeFilter,"¤") NE "" THEN
      dRegTil = DATE(ENTRY(2,icBestHodeFilter,"¤")).

    ASSIGN iBestStat    = INT(ENTRY(3,icBestHodeFilter,"¤"))
           cButikkListe = REPLACE(ENTRY(4,icBestHodeFilter,"¤"),CHR(1),",").
  END.

  FOR FIRST VarebehLinje FIELDS()
      WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK,
      FIRST VarebehHode FIELDS(VareBehType) NO-LOCK OF VarebehLinje,
            EACH VareBehBestHode NO-LOCK OF VarebehLinje
            WHERE (IF dRegFra NE ? THEN VarebehBestHode.RegistrertDato GE dRegFra ELSE TRUE) 
              AND (IF dRegTil NE ? THEN VarebehBestHode.RegistrertDato LE dRegTil ELSE TRUE):

    IF SjekkButListe(VareBehBestHode.BestNr,cButikkListe) THEN DO:
      IF VarebehHode.VarebehType = 2 THEN
        fSumKost = fSumKost + VarebehBestHode.VerdiLevert.
      ELSE 
        FOR FIRST BestHode FIELDS(TotInnkjVerdi BestStat) NO-LOCK OF VarebehBestHode
            WHERE (IF iBestStat = 4 THEN BestHode.BestStat = iBestStat AND BestHode.BekreftetDato = ?
                   ELSE IF iBestStat = 44 THEN BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?
                   ELSE IF iBestStat > 0 THEN BestHode.BestStat = iBestStat
                   ELSE TRUE):
          fSumKost = fSumKost + BestHode.TotInnkjVerdi.
        END.
    END.
  END.
  ocValue = STRING(fSumKost).
END PROCEDURE.

PROCEDURE butvarebehlinje_pris:
  /* Bruk av parametere:
     entry(1,"¤"): Fra registreringsdato, bestilling
     entry(2,"¤"): Til registreringsdato, bestilling
     entry(3,"¤"): Bestillingssatatus
     entry(4,"¤"): Butikkliste 
  ----------------------------------------------------------------------------------------------------------------------*/   

  DEF INPUT  PARAM irVarebehLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icBestHodeFilter AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

  DEF VAR fSumPris      AS DEC NO-UNDO.
  DEF VAR dRegFra       AS DATE NO-UNDO.
  DEF VAR dRegTil       AS DATE NO-UNDO.
  DEF VAR ix            AS INT NO-UNDO.
  DEF VAR cButikkListe  AS CHAR NO-UNDO.
  DEF VAR iBestStat     AS INT  NO-UNDO.

  IF icBestHodeFilter NE "" THEN DO:
    IF ENTRY(1,icBestHodeFilter,"¤") NE "" THEN
      dRegFra = DATE(ENTRY(1,icBestHodeFilter,"¤")).
    IF ENTRY(2,icBestHodeFilter,"¤") NE "" THEN
      dRegTil = DATE(ENTRY(2,icBestHodeFilter,"¤")).

    ASSIGN iBestStat    = INT(ENTRY(3,icBestHodeFilter,"¤"))
           cButikkListe = REPLACE(ENTRY(4,icBestHodeFilter,"¤"),CHR(1),",").
  END.

  FOR FIRST VarebehLinje FIELDS(Pris)
      WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK,
      FIRST VarebehHode FIELDS(VareBehType) NO-LOCK OF VarebehLinje,
            EACH VareBehBestHode NO-LOCK OF VarebehLinje
            WHERE (IF dRegFra NE ? THEN VarebehBestHode.RegistrertDato GE dRegFra ELSE TRUE) 
              AND (IF dRegTil NE ? THEN VarebehBestHode.RegistrertDato LE dRegTil ELSE TRUE):


    IF SjekkButListe(VareBehBestHode.BestNr,cButikkListe) THEN DO:
      ix = ix + 1.
      IF VarebehHode.VarebehType = 2 THEN
        fSumPris = fSumPris + VarebehBestHode.AntLevert * VarebehLinje.Pris.
      ELSE 
        FOR FIRST BestHode FIELDS(TotSalgsVerdi BestStat) NO-LOCK OF VarebehBestHode
            WHERE (IF iBestStat = 4 THEN BestHode.BestStat = iBestStat AND BestHode.BekreftetDato = ?
                   ELSE IF iBestStat = 44 THEN BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?
                   ELSE IF iBestStat > 0 THEN BestHode.BestStat = iBestStat
                   ELSE TRUE):
          fSumPris = fSumPris + BestHode.TotSalgsVerdi.
        END.
    END.
  END.
  ocValue = STRING(fSumPris).
END PROCEDURE.

PROCEDURE butvarebehlinje_sumdb:
  /* Bruk av parametere:
     entry(1,"¤"): Fra registreringsdato, bestilling
     entry(2,"¤"): Til registreringsdato, bestilling
     entry(3,"¤"): Bestillingssatatus
     entry(4,"¤"): Butikkliste 
  ----------------------------------------------------------------------------------------------------------------------*/   

  DEF INPUT  PARAM irVarebehLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icBestHodeFilter AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

  DEF VAR fSumPris      AS DEC  NO-UNDO.
  DEF VAR fSumVarekost  AS DEC  NO-UNDO.
  DEF VAR dRegFra       AS DATE NO-UNDO.
  DEF VAR dRegTil       AS DATE NO-UNDO.
  DEF VAR ix            AS INT  NO-UNDO.
  DEF VAR fPrisExMVA    AS DEC  NO-UNDO.
  DEF VAR cButikkListe  AS CHAR NO-UNDO.
  DEF VAR iBestStat     AS INT  NO-UNDO.

  IF icBestHodeFilter NE "" THEN DO:
    IF ENTRY(1,icBestHodeFilter,"¤") NE "" THEN
      dRegFra = DATE(ENTRY(1,icBestHodeFilter,"¤")).
    IF ENTRY(2,icBestHodeFilter,"¤") NE "" THEN
      dRegTil = DATE(ENTRY(2,icBestHodeFilter,"¤")).

    ASSIGN iBestStat    = INT(ENTRY(3,icBestHodeFilter,"¤"))
           cButikkListe = REPLACE(ENTRY(4,icBestHodeFilter,"¤"),CHR(1),",").
  END.

  FOR FIRST VarebehLinje FIELDS(Pris DB% MVA% Varekost)
      WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK,
      FIRST VarebehHode FIELDS(VareBehType) NO-LOCK OF VarebehLinje,
            EACH VareBehBestHode NO-LOCK OF VarebehLinje
            WHERE (IF dRegFra NE ? THEN VarebehBestHode.RegistrertDato GE dRegFra ELSE TRUE) 
              AND (IF dRegTil NE ? THEN VarebehBestHode.RegistrertDato LE dRegTil ELSE TRUE):

    IF SjekkButListe(VareBehBestHode.BestNr,cButikkListe) THEN DO:
      ix = ix + 1.
      IF VarebehHode.VarebehType = 2 THEN
        ASSIGN fSumPris     = fSumPris     + VarebehBestHode.AntLevert * VarebehLinje.Pris
               fSumVarekost = fSumVarekost + VarebehBestHode.AntLevert * VarebehLinje.Varekost.
      ELSE 
        FOR FIRST BestHode FIELDS(TotSalgsVerdi TotInnkjVerdi BestStat) NO-LOCK OF VarebehBestHode
            WHERE (IF iBestStat = 4 THEN BestHode.BestStat = iBestStat AND BestHode.BekreftetDato = ?
                   ELSE IF iBestStat = 44 THEN BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?
                   ELSE IF iBestStat > 0 THEN BestHode.BestStat = iBestStat
                   ELSE TRUE):
          ASSIGN fSumPris     = fSumPris + BestHode.TotSalgsVerdi
                 fSumVarekost = fSumVarekost + BestHode.TotInnkjVerdi.
        END.
    END.
  END.
  ASSIGN fPrisExMVA = fSumPris / (1 + Mva% / 100)
         ocValue = STRING(fPrisExMVA - fSumVarekost).
END PROCEDURE.

PROCEDURE butvarebehlinje_bestilt:
  DEF INPUT PARAM  irBuffer         AS ROWID NO-UNDO.
  DEF INPUT PARAM  icBestHodeFilter AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId      AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn         AS CHAR  NO-UNDO INIT "no".

  DEF VAR iBestStat     AS INT  NO-UNDO.
  DEF VAR cButikkListe  AS CHAR NO-UNDO.

  IF icBestHodeFilter = "" THEN RETURN.

  ASSIGN iBestStat    = INT(ENTRY(1,icBestHodeFilter,"¤"))
         cButikkListe = REPLACE(ENTRY(2,icBestHodeFilter,"¤"),CHR(1),",").

  FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    FOR EACH BestHode NO-LOCK
        WHERE BestHode.VarebehNr  = VarebehLinje.VarebehNr
          AND BestHode.ArtikkelNr = VarebehLinje.ArtikkelNr
          AND (IF iBestStat = 4 THEN BestHode.BestStat = iBestStat AND BestHode.BekreftetDato = ?
               ELSE IF iBestStat = 44 THEN BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?
               ELSE BestHode.BestStat = iBestStat)
        :
      IF SjekkButListe(BestHode.BestNr,cButikkListe) THEN DO:
        ocReturn = "yes".
        LEAVE.
      END.
    END.
  END.

  IF ocReturn = "no" THEN ocReturn = "skiprow".

END PROCEDURE.

PROCEDURE butvarebehlinje_ikke_sendt_best:
  DEF INPUT PARAM  irBuffer         AS ROWID NO-UNDO.
  DEF INPUT PARAM  icBestHodeFilter AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId      AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn         AS CHAR  NO-UNDO INIT "no".

  DEF VAR cButikkListe  AS CHAR NO-UNDO.

  IF icBestHodeFilter = "" THEN RETURN.

  cButikkListe = REPLACE(icBestHodeFilter,CHR(1),",").

  FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    FOR EACH BestHode NO-LOCK
        WHERE BestHode.VarebehNr  = VarebehLinje.VarebehNr
          AND BestHode.ArtikkelNr = VarebehLinje.ArtikkelNr
          AND BestHode.BestStat   = 3
        :
      IF SjekkButListe(BestHode.BestNr,cButikkListe) THEN DO:
        ocReturn = "yes".
        LEAVE.
      END.
    END.
  END.

END PROCEDURE.

PROCEDURE butvarebehlinje_match_merknad:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO INIT "no".

  DEF VAR ix     AS INT NO-UNDO.
  DEF VAR bMatch AS LOG NO-UNDO.

  IF icParam = "" THEN RETURN.

  FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO ix = 1 TO NUM-ENTRIES(icParam,"¤"):
    IF CAN-DO(VarebehLinje.LinjeMerknad,ENTRY(ix,icParam,"¤")) THEN DO:
      bMatch = TRUE.
      LEAVE.
    END.
  END.
  IF NOT bMatch THEN ocReturn = "skiprow".
END PROCEDURE.

PROCEDURE butvarebehlinje_rgb:
  DEF INPUT  PARAM irVarebehLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  DEF VAR iColorEntry AS INT NO-UNDO.

  FIND VarebehLinje WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK NO-ERROR.

  IF AVAIL VarebehLinje THEN 
    FOR FIRST VarebehHode FIELDS() NO-LOCK
        OF VarebehLinje
       ,FIRST Messe NO-LOCK
              OF VarebehHode:
      iColorEntry = LOOKUP(VarebehLinje.LinjeMerknad,Messe.Oppmerking,"¤").
      IF iColorEntry NE 0 AND NUM-ENTRIES(Messe.Fargekoder) GE iColorEntry THEN
        ocReturn = ENTRY(iColorEntry,Fargekoder).
    END.

END PROCEDURE.

PROCEDURE artbas_kjedevare:
  DEF INPUT  PARAM irArtBas     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  IF icParam = "" THEN RETURN.

  FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.

  IF AVAIL ArtBas AND NOT ArtBas.KjedeVare THEN ocReturn = "skiprow".
END PROCEDURE.

PROCEDURE butvarebeh_sesongfilter:
  DEF INPUT  PARAM irArtBas     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  IF icParam = "" THEN RETURN.
  ELSE icParam = REPLACE(icParam,"¤",",").

  FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.

  IF AVAIL ArtBas AND NOT CAN-DO(icParam,STRING(ArtBas.SaSong)) THEN ocReturn = "skiprow".
END PROCEDURE.

PROCEDURE butvarebehlinje_rabatt:
  DEF INPUT  PARAM irVarebehLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  FIND VarebehLinje WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK NO-ERROR.

  IF AVAIL VarebehLinje THEN DO:      
    FOR FIRST VarebehHode FIELDS() NO-LOCK
        OF VarebehLinje
       ,FIRST Messe NO-LOCK
              OF VarebehHode:

      IF Messe.MesseType = 1 THEN
        ocReturn = STRING(VarebehLinje.forhRab%).
    END.
    IF ocReturn = "" THEN
      ocReturn = STRING(VareBehLinje.supRab%).
  END.


END PROCEDURE.
