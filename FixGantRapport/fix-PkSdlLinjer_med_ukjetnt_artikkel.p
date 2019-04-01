CURRENT-WINDOW:WIDTH = 350.

FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PksdlStatus = 10,
    EACH PkSdlLinje OF PkSdlHode EXCLUSIVE-LOCK WHERE 
        NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr):

    /*
    IF PkSdlLinje.ArtikkelNr = 9831679 THEN 
        PkSdlLinje.ArtikkelNr = 9847275.
    IF PkSdlLinje.ArtikkelNr = 9849205 THEN 
        PkSdlLinje.ArtikkelNr = 9850142.
    IF PkSdlLinje.ArtikkelNr = 9851788 THEN 
        PkSdlLinje.ArtikkelNr = 9851087.
    IF PkSdlLinje.ArtikkelNr = 9853886 THEN 
        PkSdlLinje.ArtikkelNr = 9853323.

    IF PkSdlLinje.ArtikkelNr = 9851871 THEN 
        PkSdlLinje.ArtikkelNr = 9849851.

    IF PkSdlLinje.ArtikkelNr = 9855381 THEN 
        PkSdlLinje.ArtikkelNr = 9853731.

    IF PkSdlLinje.ArtikkelNr = 9855413 THEN 
        PkSdlLinje.ArtikkelNr = 9853988.

    IF PkSdlLinje.ArtikkelNr = 9851781 THEN 
        PkSdlLinje.ArtikkelNr = 9850408.

    IF PkSdlLinje.ArtikkelNr = 9849877 THEN 
        PkSdlLinje.ArtikkelNr = 9849200.
    IF PkSdlLinje.ArtikkelNr = 9853176 THEN 
        PkSdlLinje.ArtikkelNr = 9853175.
    */

    

    DISPLAY
        PkSdlHode.PkSdlNr
        PkSdlLinje.ArtikkelNr
        PkSdlLinje.LevKod
        PkSdlLinje.LevFargKod
        PkSdlHode.EDato
    WITH WIDTH 350.
END.
