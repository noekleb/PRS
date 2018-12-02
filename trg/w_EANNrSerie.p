TRIGGER PROCEDURE FOR WRITE OF EANNrSerie.

{trg/c_w_trg.i &Type="w" &Fil="EANNrSerie"}

IF EANNrSerie.EANType = 13 THEN
    ASSIGN
    EANNrSerie.AntSifferILevNr  = IF EANNrSerie.AntSifferILevNr < 4 THEN 4 ELSE EANNrSerie.AntSifferILevNr
    EANNrSerie.FraEANArtikkelNr = 0
    EANNrSerie.TilEANArtikkelNr = int(fill('9',10 - AntSifferILevNr))
    .
ELSE IF EANNrSerie.EANType = 8 THEN
    ASSIGN
    EANNrSerie.FraEANArtikkelNr = 0
    EANNrSerie.TilEANArtikkelNr = int(fill('9',5 - AntSifferILevNr))
    .



