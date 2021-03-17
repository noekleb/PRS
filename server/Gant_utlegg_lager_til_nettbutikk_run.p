DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.

ASSIGN
  cLogg = 'Gant_utlegg_lager_til_nettbutikk_run' + STRING(TODAY,"99-99-9999") 
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.'    
  ).    

iAnt = 0.
FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.OPris             = FALSE AND
    ArtBas.Lager             = TRUE AND 
    ArtBas.WebButikkArtikkel = TRUE:
    FOR EACH Lager OF ArtBAs EXCLUSIVE-LOCK:
        ASSIGN
            Lager.ETid = TIME
            iAnt = iAnt + 1
            .
    END.
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  Antall lagerposter: ' + string(iAnt) + '.'    
  ).    

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.'    
  ).    

QUIT.
