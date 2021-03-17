TRIGGER PROCEDURE FOR CREATE OF Kundereskontr.

def var trgReskontro_Id AS DEC no-undo.

LOOPEN:
DO WHILE TRUE:
    RUN trg\genreskontro_id.p (OUTPUT trgReskontro_Id).
    assign
        SkoTex.Kundereskontr.Reskontro_Id     = trgReskontro_Id
        .
    {trg\c_w_trg.i &Fil=SkoTex.Kundereskontr &Type="C"}
    LEAVE LOOPEN.
END.


