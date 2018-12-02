DEF VAR iAntMRab  AS INT NO-UNDO.
DEF VAR iAntPksdl AS INT NO-UNDO.
DEF VAR iAntPrisMRab  AS INT NO-UNDO.
DEF VAR iAntPrisPksdl AS INT NO-UNDO.
DEF VAR iAntNye AS INT NO-UNDO.
DEF VAR iAntOverstyrt AS INT NO-UNDO.
DEF VAR cArtListe AS CHAR NO-UNDO.
DEF VAR iAntMottak AS INT NO-UNDO.
DEF VAR iAntArtVaremottak AS INT NO-UNDO.
DEF VAR iAntArtOVaremottak AS INT NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.


/* Teller opp pakkseddler med problem. */
FOR EACH PkSdlHode NO-LOCK:
    FOR EACH PkSdlMottak OF PkSdlHode NO-LOCK:
        iAntMottak = iAntMottak + 1.
    END.
    IF NOT CAN-FIND(FIRST PkSdlMottak OF PkSdlHode) THEN
        cTekst = cTekst 
                 + (IF cTekst = "" THEN "" ELSE ",") 
                 + STRING(PkSdlHode.PkSdlNr).

    IF PkSdlHode.PkSdlStatus < 20  THEN
        iAntNye = iAntNye + 1.

    IF CAN-FIND(FIRST PkSdlPris OF PkSdlHode WHERE
             PkSdlPris.Rab1% > 0) THEN
        iAntMRab = iAntMRab + 1.
    iAntPksdl = iAntPksdl + 1.
END.

FOR EACH PkSdlPris NO-LOCK,
    EACH PkSdlHode OF PkSdlPris NO-LOCK /*WHERE
        PkSdlHode.EkstId = "193219" AND
        PkSdlHode.PkSdlNr = "8836" */, 
    EACH PkSdlLinje OF PkSdlPris
    BREAK BY PkSdlLinje.PkSdlId
          BY PkSdlLinje.ArtikkelNr:

    /* Totalt antall artikler/varemottak */
    IF FIRST-OF(PkSdlLinje.ArtikkelNr) THEN
        iAntPrisPkSdl = iAntPrisPkSdl + 1.

    /* Antall artikler med rabatt1 */
    IF FIRST-OF(PkSdlLinje.ArtikkelNr) AND
        PkSdlPris.Rab1% > 0 THEN 
        iAntPrisMRab = iAntPrisMRab + 1.

    /* Antall artikler med rab1 og varemottak */
    IF FIRST-OF(PkSdlLinje.ArtikkelNr) AND
        PkSdlPris.Rab1% > 0 AND
        can-find(FIRST PkSdlLinje OF PkSdlPris where
                       PkSdlLinje.antLevert > 0) THEN 
        iAntArtVaremottak = iAntArtVaremottak + 1.

    /* Antall art med rab og varemottak som ikke er overstyrt */
    IF FIRST-OF(PkSdlLinje.ArtikkelNr) AND
        PkSdlPris.Rab1% > 0 AND
        PkSdlPris.OverStyrPris = FALSE AND
        can-find(FIRST PkSdlLinje OF PkSdlPris where
                       PkSdlLinje.antLevert > 0) THEN 
        iAntArtOVaremottak = iAntArtOVaremottak + 1.
END.

MESSAGE "Antall pakkseddler totalt:" iAntPksdl SKIP
        "Antall innleverte pakksedle:" iAntMottak SKIP
        /*"Antall ikke innleverte pakksedler:" iAntNye "Liste:" cTekst SKIP*/
        "Antall pakksedler med en eller flere artikler med rabatt " iAntMRab SKIP(1)

        "Antall artikler totalt:" iAntPrisPkSdl SKIP
        "Antall artikler m/rab1"  iAntPrisMRab SKIP
        "Antall artikler m/rab1 og varemottak:" iAntArtVaremottak SKIP
        "Antall artikler m/rab1/ og varemottak og ikke overstyrt pris:" iAntArtOVaremottak SKIP(1)
        /*
        "Antall innlev. med overstyrt pris:" iAntOVerStyrt SKIP
        "Antall art/varemottak med feil:" iAntPrisMRab - iAntOVerStyrt SKIP
        "Antall artikler med feil:" NUM-ENTRIES(cArtListe)
        */
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
FOR EACH PkSdlHode NO-LOCK,
    EACH PkSdlPris OF PkSdlHode:
    FOR EACH PkSdllinje OF PkSdlHode NO-LOCK:
        
       
        DISPLAY
            PkSdlHode.PkSdlId
            PkSdlHode.PkSdlNr
            PkSdlHode.EkstId
            PkSdlLinje.ArtikkelNr
            PkSdlLinje.LevKod
            PkSdlLinje.Beskr
            PkSdlLinje.LevFargKod
            PkSdlPris.Rab1%
            WITH WIDTH 250.

    END.
END.
*/
