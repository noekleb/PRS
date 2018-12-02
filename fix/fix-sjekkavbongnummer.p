
/* Rapport som lister opp alle registrerte kvitteringer */


DEF VAR wDiff       AS INT  NO-UNDO.
DEF VAR wOldKasseNr AS INT  NO-UNDO.
DEF VAR wOldBongNr  AS INT  NO-UNDO.
DEF VAR wTekst      AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 160.

DEF STREAM Excel.

OUTPUT STREAM excel TO VALUE("BongSjekk.txt").

FOR EACH BongHode NO-LOCK WHERE
    BongHode.ButikkNr = 176 AND
    BongHode.KasseNr = 2 AND
    BongHode.DAto     >= 10/22/2004 AND
    BongHode.Dato     <= 10/25/2004
    BREAK BY BongHode.ButikkNR
          BY BongHode.GruppeNr
          BY BongHode.KasseNr
          BY BongHode.Dato
          BY BongHode.BongNr:

    ASSIGN
        wTekst = "".

    FIND butiker NO-LOCK WHERE
        Butiker.butik = BongHode.ButikkNR.

    IF FIRST-OF(BongHode.BongNr) THEN
    DO:
      
    END.



    /* Diff tracking */
    IF wOldKasseNr = BongHode.KasseNr THEN
        wDiff = BongHode.BongNr - wOldBongNr.
    ELSE 
        ASSIGN
            wDiff  = 0
            wTekst = "BYTTE".
     IF wDiff <> 1 THEN
         wTekst = "DIFF".
     IF wOldKasseNr <> BongHode.KasseNr THEN
         ASSIGN
         wTekst = "KASSEBYTTE"
         wDiff  = 0
         .

/*     DISPLAY                   */
/*         BongHode.ButikkNR     */
/*         Butiker.ButNamn       */
/*         BongHode.GruppeNr     */
/*         BongHode.KasseNr      */
/*         BongHode.Dato         */
/*         BongHode.BongNr       */
/*         wTekst                */
/*         wDiff WHEN wDiff <> 1 */
/*     WITH WIDTH 158.           */
    ASSIGN
        wOldKasseNr = BongHode.KasseNr
        wOldBongNr  = BongHode.BongNr
        .
    EXPORT STREAM excel DELIMITER ";"
        BongHode.ButikkNR
        Butiker.ButNamn
        BongHode.GruppeNr
        BongHode.KasseNr 
        BongHode.Dato    
        BongHode.BongNr 
        wTekst 
        wDiff WHEN wDiff <> 1
        .
END.

OUTPUT STREAM Excel CLOSE.
