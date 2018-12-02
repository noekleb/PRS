DEF VAR wLAger AS DEC NO-UNDO.
DEF VAR wLAger2 AS DEC NO-UNDO.
DEF VAR w2Lager AS DEC NO-UNDO.
DEF VAR w2LAger2 AS DEC NO-UNDO.

/* Viser avvik mellom lagerverdi på telleliste og lagerverdi på lagerliste. */
CURRENT-WINDOW:WIDTH = 200.

FOR EACH TelleLinje NO-LOCK WHERE
    TelleLinje.TelleNr = 11 AND 
    TelleLinje.AntallPar <> 0:

    FIND LAger NO-LOCK WHERE
        Lager.Butik = TelleLinje.butik AND
        LAger.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.

    ASSIGN
        wLAger = wLAger + (TelleLinje.AntallPar * TelleLinje.VVarekost)
        wLAger2 = wLAger2 + (TelleLinje.OpprVerdi)
        .
    IF NOT AVAILABLE LAger OR (LAger.VVarekost <> TelleLinje.VVarekost) THEN
    DO:
        ASSIGN
            w2LAger = w2LAger + (TelleLinje.AntallPar * TelleLinje.VVarekost)
            w2LAger2 = w2LAger2 + (TelleLinje.AntallPAr * (IF AVAILABLE Lager THEN Lager.VVarekost ELSE 0))
            .
        DISPLAY
        "***" WHEN NOT AVAILABLE Lager
        TelleLinje.ArtikkelNr
        TelleLinje.Beskr
        TelleLinje.VVArekost
        Lager.VVArekost WHEN AVAILABLE LAger
        TelleLinje.AntallPar
        (TelleLinje.AntallPar * TelleLinje.VVarekost)
        WITH WIDTH 198.
    END.
END.
MESSAGE 
program-name(1) SKIP
wLAger wLAger2 SKIP
    w2LAger w2LAger2 SKIP
VIEW-AS ALERT-BOX INFO BUTTONS OK.
