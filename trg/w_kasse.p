TRIGGER PROCEDURE FOR WRITE OF Kasse.

{c_w_trg.i &Type="W" &Fil="Kasse"}

DEFINE VARIABLE bStatTilHK  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.

/* Skal det sendes artikkelstatistikk? */
{syspara.i 3 4 1 cTekst}
IF (cTekst = "1" OR cTekst = "") THEN
    bStatTilHK = TRUE.
ELSE
    bStatTilHK = FALSE.

IF bStatTilHK THEN 
LOGGSTAT:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Kasse" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(Kasse.ButikkNr) + CHR(2) + STRING(Kasse.GruppeNr) + CHR(2) + STRING(Kasse.KasseNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Kasse"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(Kasse.ButikkNr) + CHR(2) + STRING(Kasse.GruppeNr) + CHR(2) + STRING(Kasse.KasseNr) NO-ERROR.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE Elogg.
END. /* LOGGSTAT */

