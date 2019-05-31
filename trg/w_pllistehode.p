TRIGGER PROCEDURE FOR WRITE OF PlListeHode.

{trg\c_w_trg.i &Fil=PlListeHode &TYPE=W}

FOR EACH PlListeLinje OF PlListeHode EXCLUSIVE-LOCK:
  DELETE PlListeLinje.
END.

