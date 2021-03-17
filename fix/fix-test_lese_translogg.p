
DEF VAR dTilbudFraDato AS DATE NO-UNDO.
DEF VAR dTilbudTilDato AS DATE NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.
DEF VAR iSalgbut AS INT NO-UNDO.

ASSIGN
    dTilbudFraDato = TODAY - 30
    dTilbudTilDato = TODAY
    iSalgbut = 15.
    .
                              

FOR EACH ArtPris NO-LOCK WHERE 
    ArtPris.ProfilNr = 16 AND 
    ArtPris.Tilbud = TRUE AND 
    CAN-FIND(FIRST Translogg WHERE 
             Translogg.ArtikkelNr = ArtPris.ArtikkelNr):
    IF ArtPris.tilbudFraDato = ? OR 
        ArtPris.TilbudtilDato = ? THEN
        NEXT.
    DO dDato = ArtPris.TilbudFraDato TO ArtPris.TilbudTilDato:
    
        FOR EACH TransLogg NO-LOCK WHERE
            Translogg.ArtikkelNr = ArtPris.ArtikkelNr AND 
            TransLogg.Dato  = dDato AND
            TransLogg.Tid  >= 0 AND 
            TransLogg.Butik = iSalgBut:
    
            CASE Translogg.ttid:
                WHEN 1 THEN
                DISPLAY
                    ArtPris.ArtikkelNr
                    TransLogg.BatchNr
                    ArtPris.TilbudFraDato
                    ArtPris.TilbudTilDato
                    .
            END CASE.
        
        END.    
    END.
END.


