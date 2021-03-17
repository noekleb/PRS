DEF VAR iAnt AS INT NO-UNDO.
CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Ut.

OUTPUT STREAM Ut TO VALUE('konv\nettbutikk_tilbud.csv').

PUT STREAM Ut
    'ArtikkelNr;Varetekst;LevKod;LevFargKod;Varekost1;Varekost2;Pris1;pris2;AktivNettbutikk'
    SKIP.

FOR EACH ArtPris NO-LOCK WHERE 
    ArtPris.ProfilNr = 16 AND 
    ArtPris.Tilbud = TRUE,
    FIRST ArtBas OF ArtPris NO-LOCK:

    iAnt = iant + 1.
    
    PUT STREAM Ut UNFORMATTED 
    ArtPris.Artikkelnr ';'
    ArtBas.Beskr   ';'
    ArtBas.LevKod   ';'
    ArtBas.LevFargKod ';'
    ArtPris.Varekost[1]  ';'
    ArtPris.Varekost[2]  ';'
    ArtPris.Pris[1]  ';'
    ArtPris.Pris[2] ';'
    ArtBas.WebButikkArtikkel
    SKIP.

END.

OUTPUT STREAM Ut CLOSE.

MESSAGE iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
