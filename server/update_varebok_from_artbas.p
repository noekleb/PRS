DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM hDummy      AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.

DEF VAR fArtikkelnr       AS DEC    NO-UNDO.
DEF VAR fVareboknr        AS DEC    NO-UNDO.
DEF VAR fMesseNr          AS DEC    NO-UNDO.
DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR iCL               AS INT    NO-UNDO.
DEF VAR cFieldList        AS CHAR   NO-UNDO.
DEF VAR hBuffVarebokLinje AS HANDLE NO-UNDO.

DEF BUFFER bVarebokLinje FOR VarebokLinje.
DEF BUFFER clButiker     FOR Butiker.

ASSIGN fArtikkelnr = DEC(ENTRY(1,icParam))
       fVareboknr  = DEC(ENTRY(2,icParam))
       .
IF NUM-ENTRIES(icParam) > 2 THEN
  cFieldList = REPLACE(ENTRY(3,icParam),"|",",").

FIND FIRST VarebokHode NO-LOCK
     WHERE VarebokHode.VarebokNr = fVarebokNr
     NO-ERROR.
IF NOT AVAIL VarebokHode THEN DO:
  ocReturn = "Feil i oppfrisking. Finner ikke varebok".
  RETURN.
END.

{syspara.i 5 1 1 iCL INT}
FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
IF NOT AVAIL clButiker THEN DO:
  ocReturn = "Finner ikke sentral-lager: " + STRING(iCL).
  RETURN.
END.

FIND ArtBas 
     WHERE ArtBas.Artikkelnr = fArtikkelnr NO-LOCK NO-ERROR.
/* bOk = hbArtBas:FIND-FIRST("WHERE Artikkelnr = " + STRING(fArtikkelnr)) NO-ERROR. */
IF AVAIL ArtBas THEN DO:
  FIND VarGr  OF ArtBas NO-LOCK NO-ERROR.
  FIND HuvGr  OF ArtBas NO-LOCK NO-ERROR.
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  FIND Produsent OF ArtBas NO-LOCK NO-ERROR.

  FIND ArtPris NO-LOCK WHERE
       ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
       ArtPris.ProfilNr   = VareBokHode.ProfilNr NO-ERROR.

  /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
  IF NOT AVAILABLE ArtPris THEN
    FIND ArtPris NO-LOCK WHERE
         ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
         ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN DO:
    ocReturn = "Ingen pris på artikkel " + STRING(fArtikkelNr).
    RETURN.  
  END.

  IF AVAIL VarGr AND AVAIL HuvGr AND AVAIL LevBas AND AVAIL ArtPris THEN DO:
    FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
    IF AVAIL Avdeling THEN DO:
      FIND VareBokLinje
           WHERE VareBokLinje.VareBokNr = fVarebokNr
             AND VareBokLinje.ArtikkelNr = ArtBas.ArtikkelNr
          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL VareBokLinje THEN DO:
      
MESSAGE 
ArtBas.ArtikkelNr ArtBas.Beskr SKIP
fArtikkelnr SKIP
Artbas.prodnr Produsent.Beskrivelse SKIP
Vareboklinje.ProdNr Vareboklinje.Produsentbeskrivelse
VIEW-AS ALERT-BOX.      
        BUFFER-COPY LevBas   TO VareBokLinje.
        BUFFER-COPY Avdeling TO VareBokLinje.
        BUFFER-COPY HuvGr    TO VareBokLinje.
        BUFFER-COPY VarGr    TO VareBokLinje.
     
        BUFFER-COPY ArtBas
            EXCEPT LinjeMerknad
                   Sortimentkoder
                   Kampanjeuker
                   Kampanjestotte
                   LevKod
                   ProdNr
                   Lagerkoder
                   Katalogpris
                   Beskr
                   LevFargKod
                   KatalogPris
                   AnbefaltPris
                   forhRab%
                   supRab%
                   KjedeInnkPris
                   KjedeRab%
                   LevDato1
                   LevDato2
                   LevDato3
                   LevDato4
                   Sasong
                   Gjennomfaktureres
                   KjedeVare
                   Utvidetsok
                   AnbefaltPris
                   VPIDato
            TO VareBokLinje.
        IF VarebokLinje.KatalogPris = 0 THEN VareBokLinje.KatalogPris = ArtBas.KatalogPris.

        DO ix = 1 TO NUM-ENTRIES(cFieldList):
          CASE ENTRY(ix,cFieldList):
            WHEN "LevKod"        THEN VarebokLinje.LevKod        = ArtBas.LevKod.
            WHEN "ProdNr"        THEN DO:
                                        VarebokLinje.ProdNr               = ArtBas.ProdNr.
                                        VareBokLinje.ProdusentBeskrivelse = Produsent.Beskrivelse.
                                      END.
            WHEN "Beskr"         THEN VarebokLinje.Beskr         = ArtBas.Beskr.
            WHEN "LevFargKod"    THEN VarebokLinje.LevFargKod    = ArtBas.LevFargKod.
            WHEN "InnkjopsPris"  THEN VarebokLinje.InnkjopsPris  = ArtBas.Katalogpris.
            WHEN "AnbefaltPris"  THEN VarebokLinje.AnbefaltPris  = ArtBas.AnbefaltPris.
            WHEN "Pris"          THEN VarebokLinje.Pris          = ArtBas.AnbefaltPris.
            WHEN "Mva%"          THEN VarebokLinje.Mva%          = ArtPris.Mva%[1].
            WHEN "forhRab%"      THEN VarebokLinje.forhRab%      = ArtBas.forhRab%.
            WHEN "supRab%"       THEN VarebokLinje.supRab%       = ArtBas.supRab%.
            WHEN "KjedeInnkPris" THEN VarebokLinje.KjedeInnkPris = ArtBas.KjedeInnkPris.
            WHEN "KjedeRab%"     THEN VarebokLinje.KjedeRab%     = ArtBas.KjedeRab%.
            WHEN "LevDato1"      THEN ASSIGN VarebokLinje.LevDato1      = ArtBas.LevDato1
                                             VarebokLinje.LevDato2      = ArtBas.LevDato2
                                             VarebokLinje.LevDato3      = ArtBas.LevDato3
                                             VarebokLinje.LevDato4      = ArtBas.LevDato4.
            WHEN "Gjennomfaktureres" THEN VarebokLinje.Gjennomfaktureres = TRUE /*ArtBas.Gjennomfaktureres*/.
            WHEN "KjedeVare"     THEN VarebokLinje.Kjedevare      = ArtBas.KjedeVare.
            WHEN "SaSong"        THEN VarebokLinje.Sasong         = ArtBas.SaSong.
          END CASE.
          /* Varene skal settes som gjennomfakturert når de sendes inn fra HK. */
          ASSIGN
              VarebokLinje.Gjennomfaktureres = TRUE.
        END.

        hBuffVarebokLinje = BUFFER VarebokLinje:HANDLE.
        IF VarebokLinje.InnkjopsPris NE 0 THEN
          RUN vareboklinje_kalkuler.p (hBuffVarebokLinje,"InnkjopsPris").

        obOk = TRUE.

        FIND FIRST VarebokHode OF VarebokLinje NO-LOCK.
        fMesseNr = VarebokHode.MesseNr.

        FOR EACH VarebokHode  
            WHERE VarebokHode.MesseNr   = fMesseNr
              AND VarebokHode.VarebokNr NE VarebokLinje.VarebokNr,
            FIRST bVarebokLinje OF VarebokHode
                  WHERE bVarebokLinje.ArtikkelNr = VarebokLinje.ArtikkelNr
                  EXCLUSIVE-LOCK:
          BUFFER-COPY LevBas   TO bVareBokLinje.
          BUFFER-COPY Avdeling TO bVareBokLinje.
          BUFFER-COPY HuvGr    TO bVareBokLinje.
          BUFFER-COPY VarGr    TO bVareBokLinje.
          
          BUFFER-COPY ArtBas 
              EXCEPT LinjeMerknad
                     Katalogpris
                     Sortimentkoder
                     Kampanjeuker
                     Kampanjestotte
                     Lagerkoder
                     LevKod
                     ProdNr
                     Beskr
                     LevFargKod
                     KatalogPris
                     AnbefaltPris
                     forhRab%
                     supRab%
                     KjedeInnkPris
                     KjedeRab%
                     LevDato1
                     LevDato2
                     LevDato3
                     LevDato4
                     Sasong
                     Gjennomfaktureres
                     KjedeVare
                     Utvidetsok
                     AnbefaltPris
                     VPIDato
              TO bVareBokLinje.
          IF VarebokLinje.KatalogPris = 0 THEN VareBokLinje.KatalogPris = ArtBas.KatalogPris.

          DO ix = 1 TO NUM-ENTRIES(cFieldList):
            CASE ENTRY(ix,cFieldList):
              WHEN "LevKod"        THEN bVarebokLinje.LevKod        = ArtBas.LevKod.
              WHEN "ProdNr"        THEN DO:
                                          bVarebokLinje.ProdNr               = ArtBas.ProdNr.
                                          bVareBokLinje.ProdusentBeskrivelse = Produsent.Beskrivelse.
                                        END.
              WHEN "Beskr"         THEN bVarebokLinje.Beskr         = ArtBas.Beskr.
              WHEN "LevFargKod"    THEN bVarebokLinje.LevFargKod    = ArtBas.LevFargKod.
              WHEN "InnkjopsPris"  THEN bVarebokLinje.InnkjopsPris  = ArtBas.Katalogpris.
              WHEN "Pris"          THEN bVarebokLinje.Pris          = ArtBas.AnbefaltPris.
              WHEN "Mva%"          THEN bVarebokLinje.Mva%          = ArtPris.Mva%[1].
              WHEN "forhRab%"      THEN bVarebokLinje.forhRab%      = ArtBas.forhRab%.
              WHEN "supRab%"       THEN bVarebokLinje.supRab%       = ArtBas.supRab%.
              WHEN "KjedeInnkPris" THEN bVarebokLinje.KjedeInnkPris = ArtBas.KjedeInnkPris.
              WHEN "KjedeRab%"     THEN bVarebokLinje.KjedeRab%     = ArtBas.KjedeRab%.
              WHEN "LevDato1"      THEN ASSIGN bVarebokLinje.LevDato1      = ArtBas.LevDato1
                                               bVarebokLinje.LevDato2      = ArtBas.LevDato2
                                               bVarebokLinje.LevDato3      = ArtBas.LevDato3
                                               bVarebokLinje.LevDato4      = ArtBas.LevDato4.
              WHEN "Gjennomfaktureres" THEN bVarebokLinje.Gjennomfaktureres = TRUE /*ArtBas.Gjennomfaktureres*/.
              WHEN "KjedeVare"     THEN VarebokLinje.Kjedevare      = ArtBas.KjedeVare.
              WHEN "SaSong"        THEN VarebokLinje.Sasong         = ArtBas.SaSong.
            END CASE.
          END.
          /* Varene skal settes som gjennomfakturert når de sendes inn fra HK. */
          ASSIGN
              bVarebokLinje.Gjennomfaktureres = TRUE.

          hBuffVarebokLinje = BUFFER bVarebokLinje:HANDLE.
          IF bVarebokLinje.InnkjopsPris NE 0 THEN
            RUN vareboklinje_kalkuler.p (hBuffVarebokLinje,"InnkjopsPris").
        END.
      END.
      ELSE ocReturn = "Oppdatering av varebok: Vareboklinje ikke tilgjengelig for oppdatering".
    END.
    ELSE ocReturn = "Oppdatering av varebok: Finner ikke avdeling for artikkel".
  END.
  ELSE IF NOT AVAIL VarGr THEN
    ocReturn = "Oppdatering av varebok: Finner ikke varegruppe for artikkel".
  ELSE IF NOT AVAIL HuvGr THEN
    ocReturn = "Oppdatering av varebok: Finner ikke hovedgruppe for artikkel".
  ELSE IF NOT AVAIL LevBas THEN
    ocReturn = "Oppdatering av varebok: Finner ikke leverandør for artikkel".
END.
ELSE ocReturn = "Oppdatering av varebok: Finner ikke artikkel: " + ENTRY(1,icParam).

