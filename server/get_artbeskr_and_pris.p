/* Henter prisinfo og artikkelbeskrivelse 
   Parametere:   Artikkelnr (+ evt ,Kundenr)
   
   Opprettet: 10.08.04 av BHa. 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(1,icParam)) NO-LOCK NO-ERROR.
IF AVAIL ArtBas THEN DO:
  FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN DO:
    FIND FIRST VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAIL VarGr THEN DO:
      FIND FIRST Moms OF VarGr NO-LOCK NO-ERROR.
      IF AVAIL Moms THEN DO:
        ASSIGN ocReturn = ArtBas.beskr + "|" + 
                          STRING(ArtPris.Tilbud) + "|" + 
                          STRING(ArtPris.Pris[1]) + "|" + 
                          STRING(ArtPris.Pris[2]) + "|" + 
                          STRING(ArtPris.Varekost[1]) + "|" + 
                          STRING(ArtPris.Varekost[2]) + "|" +
                          STRING(Moms.MomsProc) + "|"
               obOK      = TRUE
               .
        IF NUM-ENTRIES(icParam) > 1 THEN DO:
          FIND FIRST Kunde WHERE Kunde.KundeNr = DEC(ENTRY(2,icParam)) 
               NO-LOCK NO-ERROR.
          IF AVAIL Kunde THEN DO:
            FIND FIRST VgKundeGrpRabatt OF VarGr
                 WHERE VgKundeGrpRabatt.GruppeId = Kunde.GruppeId
                 NO-LOCK NO-ERROR.          
            ocReturn = ocReturn +
                       IF AVAIL VgKundeGrpRabatt AND VgKundeGrpRabatt.Rabatt% > Kunde.TotalRabatt% THEN STRING(VgKundeGrpRabatt.Rabatt%)
                       ELSE STRING(Kunde.TotalRabatt%).
          END.
        END.
      END.
      ELSE ocReturn = "Finner ikke MVA% for artikkel".
    END.
    ELSE ocReturn = "Finner ikke varegruppe for artikkel".
  END.
  ELSE ocReturn = "Finner ikke pris (artpris) for artikkel".
END.
ELSE ocReturn = "Finner ikke artikkel".

