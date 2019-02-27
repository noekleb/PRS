DEF VAR iAnt AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH ArtBas EXCLUSIVE-LOCK,
    FIRST ArtPris OF ArtBas WHERE 
          ArtPris.ProfilNr = 1:
    IF ArtBas.AnbefaltPris <> ArtPris.Pris[1] AND 
        ArtBas.AnbefaltPris < ArtPris.Pris[1]
        THEN
    DO:
        /*ASSIGN ArtBas.AnbefaltPris = ArtPris.Pris[1].*/

        DISPLAY
            ArtBas.LevKod
            ArtBas.Beskr
            ArtBas.LevFargKod
            ArtBas.AnbefaltPris
            ArtPris.Pris[1]
        WITH WIDTH 350.

        iAnt = iAnt + 1.
    END.
END.
MESSAGE iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
