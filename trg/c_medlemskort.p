TRIGGER PROCEDURE FOR CREATE OF MedlemsKort.

DEF VAR trgInterntMKortId AS DEC NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.MedlemsKort &Type="C"}

/* Feltet er slettet fra databasen
LOOPEN:
DO WHILE TRUE:    
    RUN trg/genmkortnr.p (OUTPUT trgInterntMKortId).
    ASSIGN
      MedlemsKort.InterntMKortId = trgInterntMKortId
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.
*/


