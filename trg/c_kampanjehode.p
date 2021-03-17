TRIGGER PROCEDURE FOR CREATE OF KampanjeHode.

DEFINE BUFFER trgKampanjeHode FOR KampanjeHode.

{trg\c_w_trg.i &Fil=SkoTex.KampanjeHode &Type=C}

FIND LAST trgKampanjeHode USE-INDEX KampanjeId NO-LOCK NO-ERROR.

IF AVAILABLE trgKampanjeHode THEN 
  KampanjeHode.KampanjeId = trgKampanjeHode.KampanjeId + 1.
ELSE 
  KampanjeHode.KampanjeId = 1.


