CURRENT-WINDOW:WIDTH = 200.

DEF VAR X AS INT NO-UNDO.

FIX:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.TTId   = 003  /*AND
    BongLinje.VareGr = 34   AND
    BongLinje.LopeNr = 6000*/:

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        NEXT FIX.

    X = X + 1.

    /*
    DISPLAY
        BongLinje.Butikk
        /*BongLinje.Gruppe*/
        BongLinje.Kasse
        BongLinje.Dato
        BongLinje.BongNr FORMAT ">>>>>9"
        BongLinje.LinjeNr
        BongLinje.ArtikkelNr
        BongLinje.VareGr
        BongLinje.LopeNr
        BongLinje.Storrelse FORMAT "x(4)"
        BongLinje.Antall
        BongLinje.LinjeSum
        BongLinje.LinjeRab
        BongLinje.SubTotalRab
        WITH WIDTH 198
        .
    */

    IF X MODULO 50 = 0 THEN
    DO:
        PAUSE 0.
        DISPLAY
        X 
        WITH FRAME G.
    END.

    FIND Lager EXCLUSIVE-LOCK WHERE
        Lager.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND
        Lager.Butik      = BongLinje.ButikkNr NO-ERROR.
    
    IF AVAILABLE Lager THEN
        ASSIGN
            Lager.LagAnt = Lager.Lagant - BongLinje.Antall
        .
        
    FIND ArtLag EXCLUSIVE-LOCK WHERE
        ArtLag.Butik  = BongLinje.ButikkNr AND
        ArtLag.Vg     = BongLinje.VareGr AND
        ArtLag.LopNr  = BongLinje.LopeNr AND
        ArtLag.Storl  = BongLinje.Storrelse NO-ERROR.
    
    IF AVAILABLE ArtLag THEN
        ASSIGN
        ArtLag.LagAnt = ArtLag.Lagant - BongLinje.Antall
        .
    


END. /* FIX */

DISPLAY X .
