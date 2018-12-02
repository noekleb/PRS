TRIGGER PROCEDURE FOR CREATE OF FalckEksport.

DEF VAR trgEksportNr AS DECIMAL NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.FalckEksport &TYPE=C}

    LOOPEN:
DO WHILE TRUE:
    RUN trg/genfalceksportid.p (OUTPUT trgEksportNr).
    ASSIGN
      FalckEksport.EksportId = trgEksportNr
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.

