DEF VAR bVis AS LOG NO-UNDO.

DEF VAR piant1 AS INT NO-UNDO.
DEF VAR piant2 AS INT NO-UNDO.
DEF VAR cKortNr AS CHAR NO-UNDO.
DEF VAR lMedlemsNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR dDato      AS DATE NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.


FOR EACH Medlem NO-LOCK WHERE
    /*Medlem.MedlemsNr = 103897 AND*/ 
    CAN-FIND(FIRST MedTrans OF Medlem):

    ASSIGN
        bVis       = FALSE
        piAnt1     = piant1 + 1
        dDato      = ?
        lMedlemsNr = 0.

    VIS_MEDLEM:
    FOR EACH MedTrans OF Medlem EXCLUSIVE-LOCK WHERE
        MedTrans.Dato >= 04/13/2010 AND
        MedTrans.Dato <= 09/30/2010 AND 
        Medtrans.KortNr = '':

        /*
        IF NOT CAN-FIND(FIRST Medlemskort WHERE
                        MedlemsKort.KortNr    = MedTrans.KortNr AND
                        MedlemsKort.MedlemsNr = MedTrans.MedlemsNr) THEN
        DO:
            ASSIGN
                bVis    = TRUE
                piant2  = piAnt2 + 1
                dDato   = MedTrans.Dato            
                cKortNr = MedTrans.KortNr.

            FIND Medlemskort NO-LOCK WHERE
                Medlemskort.KortNr = MedTrans.KortNr NO-ERROR.
            IF (AVAILABLE MedlemsKort AND MedlemsKort.MedlemsNr <> 0) THEN
                ASSIGN
                /*MedTrans.MedlemsNr = Medlemskort.MedlemsNr*/
                lMedlemsNr         = MedlemsKort.MedlemsNr.
        END.
        */
        lMedlemsNr = MedTrans.MedlemsNr.
        bVis = TRUE.

    END. /*VIS_MEDLEM */
    
    IF (bVis AND lMedlemsNr <> 0) THEN
    DO:
        DISPLAY
            Medlem.Fornavn
            Medlem.Etternavn
            Medlem.MedlemsNr
            piAnt1 
            piAnt2
            cKortNr
            lMedlemsNr
            dDato FORMAT "9999/99/99"
            WITH WIDTH 250.

        /*
        OUTPUT TO VALUE('MedlemKorr20101205xx.txt') APPEND.
          EXPORT Medlem.MedlemsNr lMedlemsNr.
        OUTPUT CLOSE.

        /* Fikkser feil medlem. */
        RUN beregnmedlemsaldo.p (Medlem.MedlemsNr, 0).
        RUN oppdatmedlemstat.p  (Medlem.MedlemsNr).

        /* Fikser riktig medlem */
        RUN beregnmedlemsaldo.p (lMedlemsNr, 0).
        RUN oppdatmedlemstat.p  (lMedlemsNr).
        */

        bVis = FALSE.
    END.
END.

MESSAGE 
    piAnt1 
    piAnt2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
