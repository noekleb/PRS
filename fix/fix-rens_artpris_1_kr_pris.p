CURRENT-WINDOW:WIDTH = 300.
DEF STREAM ut.

OUTPUT STREAM ut TO VALUE('c:\appdir\se\Art_uten_pris.txt').

FOR EACH artpris WHERE artpris.profilnr > 1 AND 
    ArtPris.InnkjopsPris[1] = 1 AND
    ArtPris.Pris[1]        = 0,
    FIRST ArtBas OF ArtPris NO-LOCK.


    DISPLAY
        ArtBas.ArtikkelNr
        ArtPris.ProfilNr
        ArtPris.ValPris[1]
        ArtPris.InnkjopsPris[1]
        ArtPris.VareKost[1]
        ArtPris.Pris[1]
        WITH WIDTH 300.

    PUT STREAM ut UNFORMATTED 
        ArtBas.ArtikkelNr ';'
        ArtBas.Beskr ';'
        ArtPris.ProfilNr

        SKIP.

    DO:
       FIND ELogg WHERE 
            ELogg.TabellNavn     = "ArtBas" AND
            ELogg.EksterntSystem = "POS"    AND
            ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
       IF NOT AVAIL Elogg THEN DO:
           CREATE Elogg.
           ASSIGN ELogg.TabellNavn     = "ArtBas"
                  ELogg.EksterntSystem = "POS"   
                  ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
       END.
       ASSIGN ELogg.EndringsType = 1 
              ELogg.Behandlet    = FALSE.
       IF AVAILABLE ELogg THEN RELEASE ELogg.
    END.

    /*DELETE artpris.*/
END.

