/* Setter Varenr for kundeordrelinje
   Parametere:  ROWID(KOrdreLinje)|<varenr>|<storl>
   Kommentar: Dersom artikkel har bare en størrelse så velges denne automatisk 
   
   Opprettet: 31.10.05 av BHa                  
   Endret:    28.06.06 av BHa
              25.04.13 av BHa
              - For kunde som ikke er mva pliktig trekkes mva fra pris
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cStorl      AS CHAR   NO-UNDO.
DEF VAR cVareNr     AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr AS DEC    NO-UNDO.
DEF VAR ocBestPris  AS CHAR   NO-UNDO.
DEF VAR cValue      AS CHAR   NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.
DEF VAR iCL         AS INT    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.

ASSIGN cVareNr = ENTRY(2,icParam,"|")
       cStorl  = ENTRY(3,icParam,"|").
fArtikkelNr = DEC(cVareNr) NO-ERROR.

FIND KOrdreLinje EXCLUSIVE-LOCK
     WHERE ROWID(KOrdreLinje) = TO-ROWID(ENTRY(1,icParam,"|"))
     NO-ERROR.

IF AVAIL KOrdreLinje THEN DO:
  IF cVareNr NE "" THEN DO:

    FIND KOrdreHode OF KOrdreLinje NO-LOCK NO-ERROR.

    IF fArtikkelNr NE 0 THEN
      FIND FIRST ArtBas NO-LOCK
           WHERE ArtBas.ArtikkelNr = fArtikkelNr
           NO-ERROR.
    IF NOT AVAIL ArtBas THEN
      FIND FIRST strekkode NO-LOCK 
           WHERE strekkode.Kode = cVareNr
           NO-ERROR.
    IF NOT AVAIL artbas THEN
      FIND FIRST artbas NO-LOCK 
           WHERE artbas.LevKod = cVareNr
           NO-ERROR.
    IF AVAIL ArtBas AND AVAIL KOrdreHode THEN DO:
      ASSIGN KOrdreLinje.VareNr             = STRING(artbas.artikkelnr)
             KOrdreLinje.Varetekst          = ArtBas.Beskr
             KOrdreLinje.LevFargKod         = ArtBas.LevFargKod
             KOrdreLinje.Varespesifikasjon  = ArtBas.VareFakta
             KOrdreLinje.Bestillingsnummer  = ArtBas.LevKod
             .

      RUN kordrelinje_bestpris.p (ROWID(KOrdrelinje),icSessionId,OUTPUT ocBestPris).

      RUN art_paa_tilbud.p(ROWID(ArtBas),"",icSessionId,OUTPUT cValue).
      IF NOT LOGICAL(cValue) THEN DO:
        RUN art_vgrabatt%.p(ROWID(ArtBas),STRING(KordreHode.KundeNr),icSessionId,OUTPUT cValue).
        KOrdreLinje.KundeRab% = DEC(cValue).
      END.
      ELSE KOrdreLinje.Tilbud = YES.

      ASSIGN KOrdreLinje.LinjeRab%          = 0
             KOrdreLinje.BruttoPris         = DEC(ocBestPris)
             KOrdreLinje.NettoPris          = KOrdreLinje.BruttoPris - KOrdreLinje.BruttoPris * KOrdreHode.TotalRabatt% / 100
             KOrdreLinje.NettoPris          = KOrdreLinje.NettoPris - KOrdreLinje.NettoPris * KOrdreLinje.LinjeRab% / 100
             KOrdreLinje.Antall             = IF KOrdreLinje.Antall = 0 THEN 1 ELSE KOrdreLinje.Antall
             .

      FIND FIRST StrType OF ArtBas 
           NO-LOCK NO-ERROR.
      IF AVAIL StrType THEN DO:
        IF cStorl NE "" THEN
          DO ix = 1 TO NUM-ENTRIES(StrType.Alfafordeling):
            IF cStorl = TRIM(ENTRY(ix,StrType.Alfafordeling)) THEN DO:
              KordreLinje.Storl = cStorl.
              LEAVE.
            END.
          END.
        IF cStorl = "" OR KordreLinje.Storl = "" THEN
          KordreLinje.Storl = TRIM(ENTRY(1,StrType.Alfafordeling)).
      END.

      FIND FIRST Butiker NO-LOCK 
           WHERE Butiker.Butik = KOrdreHode.ButikkNr NO-ERROR.
      FIND FIRST ArtPris OF ArtBas NO-LOCK 
           WHERE ArtPris.ProfilNr = Butiker.ProfilNr
           NO-ERROR.
      IF NOT AVAIL ArtPris THEN DO:
        {syspara.i 5 1 1 iCl INT}.
        FIND FIRST Butiker NO-LOCK 
             WHERE Butiker.Butik = iCl NO-ERROR.
        FIND FIRST ArtPris OF ArtBas NO-LOCK 
             WHERE ArtPris.ProfilNr = Butiker.ProfilNr
             NO-ERROR.
        IF NOT AVAIL ArtPris THEN
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
      END.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      IF AVAIL ArtPris AND AVAIL VarGr THEN DO:
        ASSIGN KOrdreLinje.MomsKod  = VarGr.MomsKod
               KOrdreLinje.Pris     = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
               KOrdreLinje.Varekost = ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
               .
         /* Lagt til 25/4/13: */
         FIND FIRST Kunde NO-LOCK
              WHERE Kunde.KundeNr = KOrdreHode.KundeNr
              NO-ERROR.
         IF AVAIL Kunde AND Kunde.MvaFri AND KOrdreLinje.MomsKod NE 0 THEN DO:
           FIND FIRST Moms WHERE Moms.MomsKod = KOrdreLinje.MomsKod NO-LOCK NO-ERROR.
    
           IF AVAIL Moms AND Moms.MomsProc NE 0 THEN
             ASSIGN KOrdreLinje.Pris       = KOrdreLinje.Pris / (1 + Moms.MomsProc / 100)
                    KOrdreLinje.BruttoPris = KOrdreLinje.BruttoPris / (1 + Moms.MomsProc / 100)
                    KOrdreLinje.NettoPris  = KOrdreLinje.NettoPris / (1 + Moms.MomsProc / 100)
                    KOrdreLinje.MomsKod    = 0
                    .
         END.
         /* slutt 25/4/13 */
      END.

      FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
      IF AVAIL StrType AND NUM-ENTRIES(StrType.Fordeling) = 1 THEN 
        ASSIGN KOrdreLinje.Storl   = TRIM(StrType.AlfaFordeling)
               KOrdreLinje.StrKode = INT(StrType.Fordeling).

      hBuffer = BUFFER KOrdreLinje:HANDLE.
      RUN kordrelinje_post_update.p (hBuffer,"",icSessionId,OUTPUT cValue).
    END.
    ELSE ocReturn = "Ugyldig artikkelnr".
  END.
  ELSE KOrdreLinje.VareNr = "".
END.

IF ocReturn = "" THEN DO:
  obOK = TRUE.
  IF AVAIL artbas THEN
    ocReturn = STRING(ArtBas.ArtikkelNr).
END.
