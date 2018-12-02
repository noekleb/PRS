TRIGGER PROCEDURE FOR WRITE OF Selger.

DEFINE VARIABLE bStatTilHK  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.Selger &Type="W"}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Selger" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Selger.SelgerNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Selger"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Selger.SelgerNr).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


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
         ELogg.TabellNavn     = "Selger" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(Selger.SelgerNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Selger"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(Selger.SelgerNr) NO-ERROR.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE Elogg.
END. /* LOGGSTAT */
