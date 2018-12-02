TRIGGER PROCEDURE FOR WRITE OF KassererKontanter.

{c_w_trg.i &Type="W" &Fil="KassererKontanter"}



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
         ELogg.TabellNavn     = "KassererKontanter" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(KassererKontanter.ButikkNr) + CHR(1) + STRING(KassererKontanter.Dato) + CHR(1) + STRING(KassererKontanter.KassererNr) + CHR(1) + STRING(KassererKontanter.z_nummer) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "KassererKontanter"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(KassererKontanter.ButikkNr) + CHR(1) + STRING(KassererKontanter.Dato) + CHR(1) + STRING(KassererKontanter.KassererNr) + CHR(1) + STRING(KassererKontanter.z_nummer).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.



