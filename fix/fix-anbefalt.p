DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
CURRENT-WINDOW:WIDTH = 150.
FOR EACH artbas WHERE artbas.sasong = 801:
    IF NOT CAN-FIND(artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                                  artpris.profilnr = 2) THEN
        NEXT.
    FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                       artpris.profilnr   = 1 NO-LOCK NO-ERROR.
    IF AVAIL artpris THEN DO:
        IF ArtBas.AnbefaltPris <> artpris.pris[1] THEN DO:
            ii = ii + 1.
            ArtBas.AnbefaltPris = artpris.pris[1].
        END.
    END.
END.
MESSAGE "ANTALL:" ii
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
