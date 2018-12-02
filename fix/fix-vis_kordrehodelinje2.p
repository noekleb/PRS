CURRENT-WINDOW:WIDTH = 300.

DEF VAR lAntall AS DEC NO-UNDO.

FOR EACH KORdreHod NO-LOCK WHERE
    KORdreHode.KOrdre_Id = 1150008910 AND
    LevStatus = '30', 
    EACH KORdreLinje OF KORdreHode NO-LOCK:
    lAntall = 0. 
    IF KOrdreHode.EkstOrdreNr BEGINS 'RETUR' THEN NEXT.
    IF KORdreLinje.VAreNr = 'BETALT' THEN NEXT.

    FOR EACH ArtLag NO-LOCK WHERE
        ArtLag.ArtikkelNr = DEC(KOrdreLinje.VareNr) AND
        ArtLag.Butik      = KOrdreHode.ButikkNr /* AND
        ArtLAg.StrKode    = KORdreLinje.StrKode*/:
        lAntall = lAntall + ArtLag.Lagant.
    END.
    DISPLAY 
        KORdreHode.butik
        KOrdreHode.KundeNr
        KORdreHode.KORdre_Id
        KOrdreHode.EkstOrdreNr
        KORdreHode.RegistrertDato
        KOrdreHode.LEvStatus
        KORdreLinje.VareNr
        KORdreLinje.Varetekst
        KOrdreLinje.Antall
        lAntall
    WITH WIDTH 300.
END.
 
