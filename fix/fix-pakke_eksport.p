CURRENT-WINDOW:WIDTH = 250.

DEF BUFFER bufStrekkode FOR Strekkode.

OUTPUT TO VALUE('VPIPKAllePakker.csv').

FOR EACH ArtBAs WHERE
      ArtBAs.Pakke = TRUE,
      EACH PakkeLinje OF ArtBas NO-LOCK
      BREAK BY Pakkelinje.ArtikkelNr
            BY PakkeLinje.pkArtikkelNr:

    FIND LevBAs OF ArtBas NO-LOCK NO-ERROR.

    FIND LAST Strekkode NO-LOCK WHERE
        Strekkode.ArtikkelNr = PakkeLinje.ArtikkelNr NO-ERROR.
    FIND LAST bufStrekkode NO-LOCK WHERE
        bufStrekkode.ArtikkelNr = PakkeLinje.PkArtikkelNr AND
        bufStrekkode.StrKode    = PakkeLinje.StrKode NO-ERROR.

    PUT UNFORMATTED 
        (IF first-of(PakkeLinje.ArtikkelNr) THEN 'H' ELSE 'L') ';'
        ArtBas.LevNr  ';'
        (IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE '')  ';'
        /*
        ArtBas.LevKod
        ArtBas.Beskr
        ArtBas.LevFargKod
        Pakkelinje.ArtikkelNr
        Pakkelinje.pkArtikkelNr COLUMN-LABEL 'pkArtikkelNr'
        PakkeLinje.StrKode
        */
        (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')   ';'
        (IF AVAILABLE bufStrekkode THEN bufStrekkode.Kode ELSE '') ';'
        Pakkelinje.Antall
        SKIP.
END.
