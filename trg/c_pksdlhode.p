TRIGGER PROCEDURE FOR CREATE OF PKSDLHode.

def var trgDec as DEC no-undo.

{trg/c_w_trg.i &Type="C" &Fil="SkoTex.PKSDLHode"}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genpksdlnr.p (OUTPUT trgDec).
    assign
      PKSDLHode.PkSdlId = trgDec
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.


