DEF {&New} SHARED TEMP-TABLE tmp2ArtBas NO-UNDO 
    FIELD ArtikkelNr  LIKE ArtBas.ArtikkelNr
    FIELD Beskr       LIKE ArtBas.Beskr
    FIELD Lager       LIKE ArtBas.lager
    FIELD ModellFarge LIKE ArtBas.ModellFarge
    INDEX Artikkel ArtikkelNr
    INDEX Beskr Beskr
    .

