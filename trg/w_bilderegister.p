TRIGGER PROCEDURE FOR WRITE OF Bilderegister OLD BUFFER OLDBilderegister.


ASSIGN
    Bilderegister.Dat = TODAY.

{trg\c_w_trg.i &Fil=Bilderegister &TYPE=W}
