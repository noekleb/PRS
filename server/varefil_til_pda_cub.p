/* VArefil til PDA CUB system */
/* varefil_til_pda_cub.p      */

DEF VAR ipButikkNr      AS INT  NO-UNDO.
DEF VAR cTekst          AS CHAR NO-UNDO.
DEF VAR cFilNavn        AS CHAR NO-UNDO.
DEF VAR cKatalog        AS CHAR NO-UNDO.
DEF VAR cButikkNr       AS CHAR NO-UNDO.
DEF VAR lVVAreKost      AS DEC  NO-UNDO.
DEF VAR cStrekkode      AS CHAR NO-UNDO.
DEF VAR c2Av5Interleave AS CHAR NO-UNDO.
DEF VAR bInterLeave     AS LOG  NO-UNDO.
DEF VAR cBestillingsnr  AS CHAR NO-UNDO.

DEF BUFFER clButiker FOR Butiker.
DEF BUFFER clArtPris FOR ArtPris.

DEF STREAM Ut.

{syspara.i 5 1 1 cButikkNr}
ASSIGN
    bInterLeave = TRUE /* Skal settes fra systemparameter. */
    ipButikkNr = int(cButikkNr) /* Skal komme eksternt fra... */
    cFilNavn   = 'Vare'
    cKatalog   = 'c:\HOME\lindbak\sendes'
    cFilNavn   = RIGHT-TRIM(cKatalog,'\') + '\' + 
                 cFilNavn + 
                 SUBSTRING(STRING(YEAR(TODAY),'9999'),3,2) + 
                 STRING(MONTH(TODAY),'99') + 
                 STRING(DAY(TODAY),'99') + '-' + 
                 REPLACE(STRING(TIME,'HH:MM:SS'),':','') + 
                 '.' + string(ipButikkNr)
    .

FIND clButiker NO-LOCK WHERE
    clButiker.Butik = INT(cButikkNr) NO-ERROR.
IF NOT AVAILABLE clButiker THEN
    RETURN.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = ipButikkNr NO-ERROR.

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

LOOPEN:
FOR EACH ArtBas NO-LOCK /*WHERE
    ArtBas.LevNr = 10*/,
    VarGr OF ArtBas NO-LOCK:

  STREKKODE:
  FOR EACH Strekkode OF ArtBas NO-LOCK
    BREAK BY Strekkode.ArtikkelNr /* 
          BY Strekkode.StrKode*/:

    FIND FIRST StrKonv OF Strekkode NO-LOCK NO-ERROR.

      ASSIGN
          cStrekkode      = cStrekkode + 
                            (IF cStrekkode = '' THEN '' ELSE ',') + 
                            Strekkode.Kode
          c2Av5Interleave = c2Av5Interleave + 
                           (IF c2Av5Interleave = '' THEN '' ELSE ',') + 
                            Strekkode.BestillingsNummer
          .

    IF LAST-OF(Strekkode.ArtikkelNr) THEN
    SISTE:
    DO:
        FIND clArtPris NO-LOCK WHERE
            clArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            clArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE clArtPris THEN
            NEXT LOOPEN.

        FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
            FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
            NEXT LOOPEN.

        /* Kun for lagerstyrte varer. */
        IF AVAILABLE Lager THEN RELEASE Lager.
        IF ArtBas.Lager THEN
        FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
            Lager.Butik      = INT(ipButikkNr) NO-ERROR.
        IF (AVAILABLE Lager AND Lager.VVAreKost <> ? AND Lager.VVArekost > 0) 
            THEN lVVarekost = Lager.VVareKost.
            ELSE lVVAreKost = ArtPris.VareKost[1].

        IF bInterleave THEN
            cBestillingsnr = ArtBas.LevKod.
        ELSE
            cBestillingsnr = (IF Strekkode.Bestillingsnummer <> '' 
                                THEN Strekkode.Bestillingsnummer
                                ELSE ArtBas.LevKod).

        PUT STREAM Ut UNFORMATTED
            /*  1 */ ArtBas.ArtikkelNr ';'
            /*  2 */ Strekkode.StrKode ';'
            /*  3 */ ArtBas.ModellFarge ';'
            /*  4 */ cBestillingsnr ';'

            /* Her må vi vareiere vareteksten avhengig av hva som skal gjøres. */
            /*  5 */ STRING(ArtBas.Vg) + '/' + 
                     (IF ArtBas.LopNr <> ?
                        THEN STRING(ArtBas.LopNr)
                        ELSE '0') ';'
            /*  6 */ REPLACE(REPLACE(REPLACE(TRIM(ArtBas.Beskr),';',','),CHR(10),''),CHR(13),'') ';'
            /*  7 */ REPLACE(REPLACE(REPLACE(TRIM(ArtBas.LevFargKod),';',','),CHR(10),''),CHR(13),'') ';'
            /*  8 */ REPLACE(REPLACE(REPLACE(TRIM(VarGr.VgBeskr),';',','),CHR(10),''),CHR(13),'') ';'
            /*  9 */ ArtBas.Vg ';'
            /* 10 */ (IF ArtBas.LopNr <> ?
               THEN ArtBas.LopNr
               ELSE 0) ';'
            /* 11 */ (IF AVAILABLE StrKonv THEN replace(trim(StrKonv.Storl),';',',') ELSE '') ';'
            /* 12 */ cButikkNr ';'
            /* 13 */ clArtPris.Pris[1] ';'
            /* 14 */ ArtPris.Pris[1] ';'
            /* 15 */ ArtPris.Varekost[1] ';'
            /* 16 */ lVVareKost ';'
            /* 17 */ ArtBas.Lager ';'
            /* 18 */ cStrekkode ';'      
            /* 19 */ c2Av5Interleave 
            SKIP
            .

        ASSIGN
            cStrekkode      = ''
            c2Av5Interleave = ''
            .
    END. /* SISTE */

  END. /* STREKKODE */
END. /* LOOPEN */

OUTPUT STREAM Ut CLOSE.
