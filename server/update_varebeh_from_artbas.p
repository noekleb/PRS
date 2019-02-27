/* Opprettet 13.04.07 av BHa
   Kopiering av arikkelinformasjon til varehåndteringsbok
--------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM hDummy      AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.

DEF VAR fArtikkelnr       AS DEC    NO-UNDO.
DEF VAR fVarebehNr        AS DEC    NO-UNDO.
DEF VAR fMesseNr          AS DEC    NO-UNDO.
DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR iCL               AS INT    NO-UNDO.
DEF VAR cFieldList        AS CHAR   NO-UNDO.

DEF BUFFER bVarebehLinje FOR VarebehLinje.
DEF BUFFER clButiker     FOR Butiker.


ASSIGN fArtikkelnr = DEC(ENTRY(1,icParam))
       fVarebehNr  = DEC(ENTRY(2,icParam))
       .
IF NUM-ENTRIES(icParam) > 2 THEN
  cFieldList = REPLACE(ENTRY(3,icParam),"|",",").

FIND FIRST VarebehHode NO-LOCK
     WHERE VarebehHode.VarebehNr = fVarebehNr
     NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Feil i oppfrisking. Finner ikke varehåndteringbok".
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
IF AVAIL ArtBas THEN DO:
  FIND VarGr  OF ArtBas NO-LOCK NO-ERROR.
  FIND HuvGr  OF ArtBas NO-LOCK NO-ERROR.
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.

  FIND ArtPris NO-LOCK WHERE
       ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
       ArtPris.ProfilNr   = VarebehHode.ProfilNr NO-ERROR.

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
      FIND VarebehLinje
           WHERE VarebehLinje.VarebehNr = fVarebehNr
             AND VarebehLinje.ArtikkelNr = ArtBas.ArtikkelNr
          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL VarebehLinje THEN DO:
        BUFFER-COPY Avdeling TO VarebehLinje.
        BUFFER-COPY HuvGr TO VarebehLinje.
        BUFFER-COPY VarGr TO VarebehLinje.
        BUFFER-COPY LevBas TO VarebehLinje.
     
        BUFFER-COPY ArtBas
            EXCEPT LinjeMerknad
/*                    Katalogpris   */
/*                    Beskr         */
/*                    LevFargKod    */
/*                    KatalogPris   */
/*                    AnbefaltPris  */
/*                    forhRab%      */
/*                    supRab%       */
/*                    KjedeInnkPris */
/*                    KjedeRab%     */
/*                    LevDato1      */
            TO VarebehLinje.
/*         IF VarebehLinje.KatalogPris = 0 THEN VarebehLinje.KatalogPris = ArtBas.KatalogPris. */


        ASSIGN
            VarebehLinje.VareKost        = ArtPris.VareKost[1]
            VarebehLinje.InnkjopsPris    = ArtPris.InnkjopsPris[1]
            VarebehLinje.DB%             = ArtPris.DB%[1]
            VarebehLinje.DBkr            = ArtPris.DBkr[1]
            VarebehLinje.Mva%            = ArtPris.Mva%[1]   
            VareBehLinje.supRab%         = ArtPris.Rab1%[1]
            VareBehLinje.supRab1Kr       = ArtPris.Rab1Kr[1]
            VarebehLinje.supInnkjopsPris = VarebehLinje.InnkjopsPris
            VarebehLinje.supPris         = VarebehLinje.Pris        
            VarebehLinje.Pris            = ArtPris.Pris[1]
            .

/*
        DO ix = 1 TO NUM-ENTRIES(cFieldList):
          CASE ENTRY(ix,cFieldList):
            WHEN "Beskr"         THEN VarebehLinje.Beskr         = ArtBas.Beskr.
            WHEN "LevFargKod"    THEN VarebehLinje.LevFargKod    = ArtBas.LevFargKod.
            WHEN "InnkjopsPris"  THEN VarebehLinje.InnkjopsPris  = ArtBas.Katalogpris.
            WHEN "Pris"          THEN VarebehLinje.Pris          = ArtBas.AnbefaltPris.
            WHEN "Mva%"          THEN VarebehLinje.Mva%          = ArtPris.Mva%[1].
            WHEN "forhRab%"      THEN VarebehLinje.forhRab%      = ArtBas.forhRab%.
            WHEN "supRab%"       THEN VarebehLinje.supRab%       = ArtBas.supRab%.
            WHEN "KjedeInnkPris" THEN VarebehLinje.KjedeInnkPris = ArtBas.KjedeInnkPris.
            WHEN "KjedeRab%"     THEN VarebehLinje.KjedeRab%     = ArtBas.KjedeRab%.
            WHEN "LevDato1"      THEN ASSIGN VarebehLinje.LevDato1      = ArtBas.LevDato1
                                             VarebehLinje.LevDato2      = ArtBas.LevDato2
                                             VarebehLinje.LevDato3      = ArtBas.LevDato3
                                             VarebehLinje.LevDato4      = ArtBas.LevDato4.
            WHEN "Gjennomfaktureres" THEN VarebehLinje.Gjennomfaktureres = ArtBas.Gjennomfaktureres.
            WHEN "KjedeVare"     THEN VarebehLinje.Kjedevare      = ArtBas.KjedeVare.
          END CASE.
        END.
*/

      END.
      ELSE ocReturn = "Oppdatering av varehåndteringsbok: VarebehLinje ikke tilgjengelig for oppdatering".
    END.
    ELSE ocReturn = "Oppdatering av varehåndteringsbok: Finner ikke avdeling for artikkel".
  END.
  ELSE IF NOT AVAIL VarGr THEN
    ocReturn = "Oppdatering av varehåndteringsbok: Finner ikke varegruppe for artikkel".
  ELSE IF NOT AVAIL HuvGr THEN
    ocReturn = "Oppdatering av varehåndteringsbok: Finner ikke hovedgruppe for artikkel".
  ELSE IF NOT AVAIL LevBas THEN
    ocReturn = "Oppdatering av varehåndteringsbok: Finner ikke leverandør for artikkel".
END.
ELSE ocReturn = "Oppdatering av varehåndteringsbok: Finner ikke artikkel: " + ENTRY(1,icParam).

/* TN 20/6-07 */
IF ocReturn = "" THEN obOk = TRUE.


