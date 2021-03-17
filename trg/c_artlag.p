TRIGGER PROCEDURE FOR CREATE OF ArtLag.

/* {trg\c_w_trg.i &Fil=ArtLag &TYPE=C} */

ASSIGN 
    ArtLag.EndretDatoTid = NOW
    .

