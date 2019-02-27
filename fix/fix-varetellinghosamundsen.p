CURRENT-WINDOW:WIDTH = 180.

DEF VAR X AS INT NO-UNDO.

OUTPUT TO "Amundsen.txt".

ARTBAS:
FOR EACH ArtBas NO-LOCK WHERE
    ArtBas.Lager = TRUE AND
    ArtBas.Storrelser = FALSE AND
    CAN-FIND(FIRST TelleLinje where
             TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr):
    ARTLAG:
    FOR EACH ArtLag NO-LOCK WHERE
        ArtLag.Butik    = 1 AND
        ArtLag.Vg       = ArtBas.Vg AND
        ArtLag.LopNr    = ArtBas.LopNr AND
        ArtLag.AntSolgt <> 0
        BREAK
        BY Artlag.Butik:
        IF FIRST-OF(Artlag.Butik) THEN
            ASSIGN
              X = X + 1
            .

        DISPLAY
            X FORMAT "999" COLUMN-LABEL "Nr"
            ArtBas.ArtikkelNr
            ArtBas.Beskr
            ArtBas.LAger      COLUMN-LABEL "Lag"
            ArtBas.Storrelser COLUMN-LABEL "StF"
            Artlag.Butik      COLUMN-LABEL "But"
            Artlag.Storl      COLUMN-LABEL "Str"
            Artlag.AntSolgt (TOTAL)
            WITH WIDTH 178 STREAM-IO
            .
    END. /* ARTLAG */

END. /* ArtBas */
