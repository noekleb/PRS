TRIGGER PROCEDURE FOR CREATE OF EANNrSerie.

DEF BUFFER trgEANNrSerie FOR EANNrSerie.

FIND LAST trgEANNrSerie NO-LOCK NO-ERROR.
IF AVAILABLE trgEANNrSerie THEN
    EANNrSerie.EANSerieId = trgEANNrSerie.EANSerieId + 1.
ELSE
    EANNrSerie.EANSerieId = 1.

{trg/c_w_trg.i &Type="c" &Fil="EANNrSerie"}




