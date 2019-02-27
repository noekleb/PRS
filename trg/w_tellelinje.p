TRIGGER PROCEDURE FOR WRITE OF SkoTex.TelleLinje.


{trg\c_w_trg.i &Fil="SkoTex.TelleLinje" &Type="W"}

    DEF VAR trgLinjeNr LIKE TelleLinje.LinjeNr NO-UNDO.

    DEF BUFFER trgTelleLinje FOR TelleLinje.

    IF TelleLinje.LinjeNr = 0 THEN
    DO:
        FIND LAST trgTelleLinje NO-LOCK WHERE
            trgTelleLinje.TelleNr = TelleLinje.TelleNr 
            USE-INDEX LinjeNr NO-ERROR.
        IF AVAILABLE trgTelleLinje THEN
            trgLinjeNr = TrgTelleLinje.LinjeNr + 1.
        ELSE
            trgLinjeNr = 1.
        ASSIGN
            TelleLinje.LinjeNr = trgLinjeNr.
    END.


