DEF INPUT  PARAM lArtikkelNr AS DEC  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF VAR iCl AS INT NO-UNDO.

{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.

IF AVAILABLE Butiker THEN
DO:
    FIND FIRST ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = lArtikkelNr AND 
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
    IF AVAILABLE ArtPris THEN
        ocValue = string(ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1]).
    ELSE
        ocValue = "".
END.
ELSE 
    ocValue = "".

