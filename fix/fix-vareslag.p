/* fix-vareslag.p */

PUBLISH 'infoDisp' ("Initierer vareslag..").

FOR EACH ArtBas EXCLUSIVE-LOCK:
    /* LAgerstyrt m/størrelser */
    IF ArtBas.LAger AND ArtBas.StrTypeId > 2 THEN
        ArtBas.ArtSlag = 0.
    /* LAgerstyrt u/størrelser */
    IF ArtBas.LAger AND ArtBas.StrTypeId = 2 THEN
        ArtBas.ArtSlag = 1.
    /* Ikke lagerstyrt m/størrelser */
    IF ArtBas.LAger = FALSE AND ArtBas.StrTypeId > 2 THEN
        ArtBas.ArtSlag = 2.
    /* Ikke lagerstyrt m/størrelser */
    IF ArtBas.LAger = FALSE AND ArtBas.StrTypeId = 2 THEN
        ArtBas.ArtSlag = 3.
    /* PLU artikkel */
    IF ArtBas.Opris THEN
        ArtBas.ArtSlag = 4.
    /* Pakkevare */
    IF ArtBas.Pakke THEN
        ArtBas.ArtSlag = 5.

END.
PUBLISH 'infoDisp' ("..").


