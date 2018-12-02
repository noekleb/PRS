TRIGGER PROCEDURE FOR WRITE OF LevSort.

DEF VAR piLevNr AS INT NO-UNDO.

/* Setter høyeste levnr som skal eksporteres. */
{syspara.i 16 2 1 piLevNr INT}
IF piLevNr = 0 THEN
    piLevNr = 999.

{trg\c_w_trg.i &Fil=SkoTex.LevSort &TYPE=W}

IF LevSort.LevNr <= piLevNr THEN
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "LevSort" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(LevSort.LevNr) + "," + LevSort.SortID NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "LevSort"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(LevSort.LevNr) + "," + LevSort.SortID.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
END.
RELEASE ELogg.


