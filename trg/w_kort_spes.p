TRIGGER PROCEDURE FOR WRITE OF Kort_Spes.

{trg\c_w_trg.i &Fil=SkoTex.Kort_Spes &TYPE=W}

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
         ELogg.TabellNavn     = "Kort_Spes" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(Kort_Spes.dato) + CHR(1) + STRING(Kort_Spes.butikk) + CHR(1) + STRING(Kort_Spes.kasse) + CHR(1) + STRING(Kort_Spes.z_nummer) + CHR(1) + STRING(Kort_Spes.KortType) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Kort_Spes"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(Kort_Spes.dato) + CHR(1) + STRING(Kort_Spes.butikk) + CHR(1) + STRING(Kort_Spes.kasse) + CHR(1) + STRING(Kort_Spes.z_nummer) + CHR(1) + STRING(Kort_Spes.KortType).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.
