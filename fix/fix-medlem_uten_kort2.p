DEF VAR bVis AS LOG NO-UNDO.

DEF VAR piant1 AS INT NO-UNDO.
DEF VAR piant2 AS INT NO-UNDO.
DEF VAR cKortNr AS CHAR NO-UNDO.
DEF VAR lMedlemsNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR dDato      AS DATE NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.


FOR EACH Medlem NO-LOCK WHERE
    /*Medlem.MedlemsNr = 100178 AND*/ 
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

        FIND Bonghode NO-LOCK WHERE 
          bonghode.butikknr = MedTrans.butik AND
          bonghode.gruppenr = 1 AND
          bonghode.kassenr  = MedTrans.kassanr AND
          bonghode.dato     = MedTrans.Dato AND
          BongHode.BongNr   = MedTrans.BongId NO-ERROR.
        IF AVAILABLE BongHode THEN
        DO:
            IF BongHode.MedlemsNr <> Medlem.MedlemsNr THEN
              ASSIGN
                MedTrans.MedlemsNr = BongHode.MedlemsNr
                MedTrans.KortNr    = BongHode.MedlemsKort
                Medtrans.SeqNr     = Medtrans.SeqNr + 20
                lMedlemsNr         = BongHode.MedlemsNr
                bVis               = TRUE.
        END.
    END. /*VIS_MEDLEM */
    
    IF (bVis AND lMedlemsNr <> 0) THEN
    DO:
        /*
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
         */
        OUTPUT TO VALUE('MedlemKorr_Bong20101205.txt') APPEND.
          EXPORT Medlem.MedlemsNr lMedlemsNr.
        OUTPUT CLOSE.

        /* Fikkser feil medlem. */
        RUN beregnmedlemsaldo.p (Medlem.MedlemsNr, 0).
        RUN oppdatmedlemstat.p  (Medlem.MedlemsNr).

        /* Fikser riktig medlem */
        RUN beregnmedlemsaldo.p (lMedlemsNr, 0).
        RUN oppdatmedlemstat.p  (lMedlemsNr).

        lMedlemsNr = 0.
        bVis = FALSE.
    END.
END.

MESSAGE 
    piAnt1 
    piAnt2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
