TRIGGER PROCEDURE FOR WRITE OF KassererValuta.

{c_w_trg.i &Type="W" &Fil="KassererValuta"}

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
         ELogg.TabellNavn     = "KassererValuta" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(KassererValuta.ButikkNr) + CHR(1) + STRING(KassererValuta.KasseNr) + CHR(1) + STRING(KassererValuta.Dato) + CHR(1) + STRING(KassererValuta.KassererNr) + CHR(1) + STRING(KassererValuta.z_nummer) + CHR(1) + STRING(KassererValuta.ValKod) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "KassererValuta"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(KassererValuta.ButikkNr) + CHR(1) + STRING(KassererValuta.KasseNr) + CHR(1) + STRING(KassererValuta.Dato) + CHR(1) + STRING(KassererValuta.KassererNr) + CHR(1) + STRING(KassererValuta.z_nummer) + CHR(1) + STRING(KassererValuta.ValKod).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

