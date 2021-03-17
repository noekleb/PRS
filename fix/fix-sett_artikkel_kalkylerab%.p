DEF BUFFER bufArtPris FOR ArtPris.
DEFINE VARIABLE lforhRab%   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fMvaKr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fDbKr       AS DECIMAL   NO-UNDO.

DEF VAR iAnt AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtBAs NO-LOCK WHERE 
    ArtBas.RegistrertDato >= 01/01/2020,
    FIRST ArtPris EXCLUSIVE-LOCK WHERE 
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
      ArtPris.ProfilNr = 1 AND 
      ArtPris.Rab1%[1] > 10:
      
    iAnt = iAnt + 1.
    /*
    DISPLAY
    ArtBas.RegistrertDato
    ArtBAs.ArtikkelNr
    ArtBas.Beskr
    ArtBas.LevKod
    ArtBas.LEvFargKod
    ArtBas.KatalogPris
    artBas.ForhRab%    
    ArtBas.AnbefaltPris
    '|'
    ArtPris.ProfilNr
    ArtPris.InnkjopsPris[1]
    ArtPris.Rab1%[1]
    ArtPris.VareKost[1]
    ArtPris.Pris[1]
    WITH WIDTH 350.
    */
    
    ASSIGN 
        lForhRab% = 10
        .
    
    /* Regner om kalkylen hvis priser er angitt. */
    IF ArtPris.InnkjopsPris[1] > 0 AND lforhRab% > 0 THEN
    DO: 
      ASSIGN 
        ArtPris.Rab1%[1]        = lforhRab%
        ArtPris.Varekost[1]     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
        fMvaKr                  = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
        fDbKr                   = ArtPris.Pris[1] - fMvaKr - ArtPris.Varekost[1]                   
        ArtPris.Db%[1]          = ROUND((fDbKr * 100) / (ArtPris.Pris[1] - fMvaKr),2)
        ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
        .
    END.
    
END.
MESSAGE iAnt
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
