/*
Korriger_anbefalt_pris.p
*/
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

ON write OF artbas OVERRIDE DO: END.

ASSIGN 
    cLogg = 'Korriger_anbefalt_pris' + REPLACE(STRING(TODAY),'/','')
    .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).    

FOR EACH artbas:
    FIND artpris WHERE 
        artpris.artikkelnr = artbas.artikkelnr AND
        artpris.profilnr   = 1 NO-LOCK NO-ERROR.
    IF AVAIL artpris THEN 
    DO:
        IF ArtBas.AnbefaltPris < artpris.pris[1] AND artpris.pris[1] <> 0 THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                'Artikkel (ArtikkelNr/LevKod)' + STRING(ArtBas.ArtikkelNr) + '/' + ArtBas.LevKod + ' korrigert fra ' + STRING(ArtBas.AnbefaltPris) + ' til ' + STRING(artpris.pris[1]) 
                ).    
            ii = ii + 1.
            ASSIGN 
                ArtBas.AnbefaltPris = artpris.pris[1]
                NO-ERROR.

        END.
    END.
END.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt. Korrigert ' + STRING(ii) + ' poster.' 
    ).    
