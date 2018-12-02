CURRENT-WINDOW:WIDTH = 250.

DEF VAR piLoop AS INT NO-UNDO.

LOOPEN:
FOR EACH ArtBas WHERE ArtBas.LevNr = 10 AND 
    CAN-FIND(FIRST ArtPris WHERE ArtPris.Varekost[1] > 0 AND 
              ArtPris.Pris[1] > 0) AND
    ArtBAs.LevFargKod > "":

    FIND FIRST ArtPris OF ArtBas NO-LOCK.

    IF ArtPris.Pris[1] <> ? AND ArtPris.Varekost[1] <> 0 THEN DO:
        DISPLAY
            ArtBas.ArtikkelNr
            ArtBAs.LevKod
            ArtBAs.Beskr
            ArtBas.LevFargKod
            ArtPris.Varekost[1]
            ArtPris.Pris[1]
            WITH WIDTH 250.

        piLoop = piLoop + 1.
        CREATE VPIMottak.
        DISPLAY
            VPIMottak.VPIMottakId
            .
        ASSIGN
            /*VPIMottak.VPIMottakId      = piLoop*/
            VPIMottak.BehStatus        = 1
            VPIMottak.VPIType          = 2   /* 2-Korr VPI */
            VPIMottak.ArtikkelNr       = ArtBas.ArtikkelNr
            VPIMottak.LevKod           = ArtBAs.LevKod
            VPIMottak.Beskr            = ArtBAs.Beskr
            VPIMottak.LevFargKod       = ArtBas.LevFargKod
            VPIMottak.InnkjopsPris     = ArtPris.InnkjopsPris[1]
            VPIMottak.Varekost         = ArtPris.Varekost[1]
            VPIMottak.Rab1%            = ArtPris.Rab1%[1]
            VPIMottak.Db%              = ArtPris.Db%[1]
            VPIMottak.Mva%             = ArtPris.Mva%[1]
            VPIMottak.AktiveresDato    = Today
            VPIMottak.GyldigTilDato    = ?
            VPIMottak.Pris             = ArtPris.Pris[1]
            VPIMottak.Pris             = ArtPris.Pris[1]
            VPIMottak.Pris             = ArtPris.Pris[1]

            .

    END.
    IF piLoop = 300 THEN
        LEAVE LOOPEN.


END.
