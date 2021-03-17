CURRENT-WINDOW:WIDTH = 300.
    
FOR EACH VArGr:
    Kost_Proc = 65.
END.

FOR EACH ArtBAs WHERE 
    ArtBAs.OPRis = TRUE:

    ArtBas.LEvNr = 999.
    FIND FIRST ArtPris OF ArtBAs.
    FIND VarGr OF ArtBas NO-LOCK.
    FIND LevBas OF ArtBas.
    
    ASSIGN
        ArtBAs.OPris = TRUE
        ArtPris.Valpris[1] = 65
        ArtPris.InnkjopsPris[1] = 65
        ArtPris.Varekost[1] = 65
        ArtPris.dbkr[1] = 35
        ArtPris.db%[1] = 35
        ArtPris.MvaKr[1] = 25
        ArtPris.Mva%[1] = 25
        ArtPris.Pris[1] = 125
        .
    
    DISPLAY ArtPris.ArtikkelNr
        ArtBas.LEvKod
        ArtBAs.Beskr
        ArtBas.LEvFargKod
        ArtPris.InnkjopsPris[1]
        ArtPris.VareKost[1]
        ArtPris.db%[1]
        ArtPris.Pris[1]
        VarGr.Kost_Proc
        ArtBas.LEvNr
        LevBAs.ValKod
        WITH WIDTH 300.
END.
