OUTPUT TO VALUE('c:\tmp\Time_varer_med_o-kalkyle.csv').
FOR EACH ArtPris WHERE artpris.profilnr > 1 AND ArtPris.Pris[1] = 0 /* AND 
ArtPris.VareKost[1] = 1*/:
    FIND ArtBas OF ArtPris NO-LOCK.
    PUT UNFORMATTED
        artpris.artikkelnr ';'
        ArtBas.LevKod     ';'
        ArtBas.Beskr     ';'
        artpris.profilnr    ';'
        ArtPris.AktivFraDato   ';'
        artpris.innkjopspris[1]   ';'
        artpris.varekost[1]          ';'
        artpris.pris[1]
        SKIP.
    DELETE ArtPris.
END.
