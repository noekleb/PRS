TRIGGER PROCEDURE FOR WRITE OF MixMatchHode OLD BUFFER oldMixMatchHode.

{trg\c_w_trg.i &Fil=SkoTex.MixMatchHode &TYPE=W}

IF MixMatchHode.MixAktiver = TRUE AND oldMixMatchHode.MixAktiver = FALSE THEN DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "MixMatch" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(MixMatchHode.MixNr) NO-ERROR.
    IF NOT AVAIL ELogg THEN DO:
        CREATE ELogg.
        ASSIGN ELogg.TabellNavn     = "MixMatch"
               ELogg.EksterntSystem = "POS"
               ELogg.Verdier        = STRING(MixMatchHode.MixNr).
    END.
    ASSIGN ELogg.EndringsType = 1.
END.
ELSE IF MixMatchHode.MixAktiver = FALSE AND oldMixMatchHode.MixAktiver = TRUE THEN DO:
    FOR EACH MixMatchRad OF MixMatchHode NO-LOCK:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "MixMatch" AND
             ELogg.EksterntSystem = "POS"    AND
             ELogg.Verdier        = STRING(MixMatchRad.MixNr) + CHR(1) + STRING(MixMatchRad.Kode) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "MixMatch"
                   ELogg.EksterntSystem = "POS"   
                   ELogg.Verdier        = STRING(MixMatchRad.MixNr) + CHR(1) + STRING(MixMatchRad.Kode) NO-ERROR.
        END.
        ASSIGN ELogg.EndringsType = 3
               ELogg.Behandlet    = FALSE.
    END.
END.
RELEASE ELogg.


