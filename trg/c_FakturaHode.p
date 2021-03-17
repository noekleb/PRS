TRIGGER PROCEDURE FOR CREATE OF FakturaHode.

DEF VAR trgFaktura_Id AS DEC NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.FakturaHode &Type="C"}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genfaktura_id.p (OUTPUT trgFaktura_Id).
    ASSIGN
        SkoTex.FakturaHode.Faktura_Id     = trgFaktura_Id
        Skotex.FakturaHode.FakturaNr      = ?
        .
    LEAVE LOOPEN.
END.



