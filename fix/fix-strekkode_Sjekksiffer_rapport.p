CURRENT-WINDOW:WIDTH = 200.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR lDec     AS DEC  NO-UNDO.
DEF VAR cEANKode AS CHAR NO-UNDO.

FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

DEF STREAM Ut.

ASSIGN
    cFilNavn = "Strekkode_Sjekksiffer.csv"
    .

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

PUT STREAM Ut UNFORMATTED
    "Feil EAN;Strekkode;ArtikkelNr;LEv.art.nr;Varetekst;Lev.fargekode;Bestillingsnr;SE nr."
    SKIP.

FOR EACH Strekkode NO-LOCK:
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    /* ----------------------
    DISPLAY
        Strekkode.Kode
        Strekkode.ArtikkelNr
        ArtBAs.LevKod WHEN AVAILABLE ArtBas
        ArtBAs.Beskr  WHEN AVAILABLE ArtBas
        ArtBAs.LevFargKod WHEN AVAILABLE ArtBas
        Strekkode.Bestillingsnummer
        STRING(Strekkode.ArtikkelNr) + "000" FORMAT "x(13)" COLUMN-LABEL "SE nr."
        WITH WIDTH 248
        .
    ---------------- */ 
    ASSIGN
        lDec = DEC(Strekkode.Kode)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    IF lDec <= 999999 THEN
        NEXT.

    ASSIGN 
        cEANKode = trim(Strekkode.Kode)
        cEANkode = DYNAMIC-FUNCTION('fixChkEAN':U,
                                        INPUT SUBSTR(cEANkode,1,12)).
  
    IF (TRIM(Strekkode.Kode) <> cEANKode) THEN
    PUT STREAM Ut UNFORMATTED
        Strekkode.Kode ";"
        cEANKode ";"
        Strekkode.ArtikkelNr ";"
        IF AVAILABLE ArtBas THEN ArtBAs.LevKod ELSE "** Ukjent artikkel" ";"
        IF AVAILABLE ArtBas THEN ArtBAs.Beskr  ELSE "" ";"
        IF AVAILABLE ArtBas THEN ArtBAs.LevFargKod ELSE "" ";"
        Strekkode.Bestillingsnummer ";"
        STRING(Strekkode.ArtikkelNr) + "000" FORMAT "x(13)" ";"
        SKIP.
END.

OUTPUT STREAM Ut CLOSE.

FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
  cKode = cKode + '0'.
  RUN bibl_chkean.p(INPUT-OUTPUT cKode).
  RETURN cKode.

  /*
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
  */
END FUNCTION.



