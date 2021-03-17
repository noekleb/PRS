CURRENT-WINDOW:WIDTH = 3350.

FOR EACH KORdreHode EXCLUSIVE-LOCK WHERE 
    KOrdreHode.butikkNr = 15 AND
    KOrdreHode.SendingsNr = '' AND 
    KOrdreHode.RegistrertDato >= DATE(10,12,2017) AND 
                                   /* mån,dag,år */
/*     KOrdreHode.RegistrertDato >= 12/10/2017 AND */
    KOrdreHode.LevStatus = '50':

    KOrdreHode.SendingsNr = 'UTLEVERT' + ' ' + REPLACE(STRING(TODAY),'/','') + ' ' + REPLACE(STRING(TIME),':','').

    DISPLAY
        KOrdreHode.butikkNr
        KOrdreHode.Kordre_Id
        KOrdreHode.ekstOrdreNr
        KOrdreHode.SendingsNr
        KOrdreHode.RegistrertDato
        KOrdreHode.LevStatus
        WITH WIDTH 350
        .
END.
