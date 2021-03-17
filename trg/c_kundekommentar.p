TRIGGER PROCEDURE FOR CREATE OF KundeKommentar.

DEFINE BUFFER trgKundeKommentar FOR KundeKommentar.

DEFINE VARIABLE trgKommentarId AS INT NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.KundeKommentar &TYPE=C}

FIND LAST trgKundeKommentar NO-LOCK NO-ERROR.
IF AVAILABLE trgKundeKommentar 
  THEN trgKommentarId = trgKundeKommentar.KommentarId + 1.
  ELSE trgKommentarId = 1.
  
ASSIGN
  KundeKommentar.KommentarId = trgKommentarId 
  .

