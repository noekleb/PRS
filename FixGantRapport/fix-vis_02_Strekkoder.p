CURRENT-WINDOW:WIDTH = 350.
OUTPUT TO VALUE('konv\02_strekkode.csv').
FOR EACH Strekkode NO-LOCK WHERE 
    Strekkode.Kode BEGINS '02' AND 
    LENGTH(strekkode.Kode) = 13
    BREAK BY Strekkode.EDato DESCENDING:

    FIND ArtBas OF Strekkode NO-LOCK.
    PUT UNFORMATTED 
        strekkode.kode ';'
        Strekkode.EDato ';'
        ArtBas.LevKod ';'
        ArtBas.Beskr ';'
        ArtBas.Farg
    SKIP.
    
END.
