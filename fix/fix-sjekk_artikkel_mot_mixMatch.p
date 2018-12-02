DEF VAR cKombKampanjeIdList AS CHAR NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.
DEF VAR dArtikkelNr AS DEC NO-UNDO. 

ASSIGN
    cKombKampanjeIdList = '9035219'
    dArtikkelNr         = 21102
    .
FORM
    KampanjeTilbArtikkel.KampId
    ProduktFamMedlem.ProdFamId
    KampanjeTilbArtikkel.KampTilbId
    ProduktFamMedlem.ProdFamArtikkelNr
    WITH FRAME Gurre DOWN.


          DO piLoop = 1 TO NUM-ENTRIES(cKombKampanjeIdList):
              FOR EACH KampanjeTilbArtikkel FIELDS() NO-LOCK
                  WHERE KampanjeTilbArtikkel.KampId        = DEC(ENTRY(piLoop,cKombKampanjeIdList)) AND
                        KampanjeTilbArtikkel.KampTilbId   >= 0 AND                
                        KampanjeTilbArtikkel.KampTilbArtId = 0:
                FOR EACH ProduktFamMedlem NO-LOCK WHERE 
                    ProduktFamMedlem.ProdFamId = KampanjeTilbArtikkel.ProdFamId AND 
                    ProduktFamMedlem.ProdFamArtikkelNr = dArtikkelNr:
                         
MESSAGE 
    'KampId:' KampanjeTilbArtikkel.KampId SKIP
    'ProdFamId:' ProduktFamMedlem.ProdFamId SKIP
    'KampTilbId:' KampanjeTilbArtikkel.KampTilbId SKIP
    'ProdFamArtikkelNr:' ProduktFamMedlem.ProdFamArtikkelNr SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END.
              END.
          END. 
