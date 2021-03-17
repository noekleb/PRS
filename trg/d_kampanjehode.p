TRIGGER PROCEDURE FOR DELETE OF KampanjeHode.

/* Tar bort også kampanjemixmatchen */
IF KampanjeHode.KampId > 0 THEN 
DO:
  FIND FIRST KampanjeMixMatch EXCLUSIVE-LOCK WHERE
    KampanjeMixMatch.KampId = KampanjeHode.KampId NO-ERROR.
  IF AVAILABLE KampanjeMixMatch THEN 
    DELETE KampanjeMixMatch.
END.

FOR EACH Kampanjelinje OF KampanjeHode EXCLUSIVE-LOCK:
    DELETE kampanjelinje.
END.
FOR EACH KampanjeButKobling  EXCLUSIVE-LOCK WHERE 
    KampanjeButKobling.KampanjeId = KampanjeHode.KampanjeId:
    DELETE KampanjeButKobling.
END.

FOR EACH KampanjeProfil EXCLUSIVE-LOCK WHERE 
  KampanjeProfil.KampanjeId = KampanjeHode.KampanjeId:
  DELETE KampanjeProfil.
END.  