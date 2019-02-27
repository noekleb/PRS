TRIGGER PROCEDURE FOR WRITE OF KassererOppgj.

{c_w_trg.i &Type="W" &Fil="KassererOppgj"}

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
         ELogg.TabellNavn     = "KassererOppgj" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(kassererOppgj.ButikkNr) + CHR(1) + STRING(kassererOppgj.Dato) + CHR(1) + STRING(kassererOppgj.KassererNr) + CHR(1) + STRING(kassererOppgj.z_nummer) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "KassererOppgj"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(kassererOppgj.ButikkNr) + CHR(1) + STRING(kassererOppgj.Dato) + CHR(1) + STRING(kassererOppgj.KassererNr) + CHR(1) + STRING(kassererOppgj.z_nummer).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

