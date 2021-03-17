DEF VAR iAnt AS INT NO-UNDO.

{cls\dintero\ttMedlem.i}
{cls\dintero\dsMedlem.i}

CURRENT-WINDOW:WIDTH = 350.

KUNDELOOP:
FOR EACH Medlem WHERE 
        medlem.epostadresse > '':

    iAnt = iAnt + 1.

    DISPLAY 
        medlem.medlemsnr
        Medlem.forNavn
        Medlem.Etternavn
        Medlem.WebBrukerId
    WITH WIDTH 350.

    medlem.eTid = TIME. /* Skaper ELogg. */

    RUN fyllDatasettMedlem.p (Medlem.MedlemsNr, FALSE, INPUT-OUTPUT DATASET dsMedlem ).
    DATASET dsMedlem:WRITE-JSON('file','konv\medlem' + STRING(Medlem.MedlemsNr) + '.json', TRUE).

    IF iAnt >= 20 THEN 
        LEAVE KUNDELOOP.
END. /* KUNDELOOP */


