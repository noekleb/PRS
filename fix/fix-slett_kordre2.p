
/*
FIND KordreHode WHERE 
    KORdreHode.KOrdre_Id = 1160000008.
*/
FOR EACH KORdreHode WHERE 
    CAN-DO('3631,3645,3646,3649,3647,3650,3651,3652,3653,3654,3655,3656,3657,3658,3659,3660,3661' + 
           'RETUR 3631,RETUR 3645,RETUR 3646,RETUR 3649,RETUR 3647,RETUR 3650,RETUR 3651,RETUR 3652,RETUR 3653,RETUR 3654,RETUR 3655,RETUR 3656,RETUR 3657,RETUR 3658,RETUR 3659,RETUR 3660,RETUR 3661',KOrdreHode.EkstOrdreNr):
    RUN slettORdre.
END.

PROCEDURE slettOrdre:
IF AVAILABLE KORdreHode THEN
DO:
    FOR EACH KOrdreLinje OF KOrdreHode:
        DELETE KORdreLinje.
    END.
    FIND kunde OF KOrdreHode NO-ERROR.
    IF AVAILABLE Kunde THEN
    DO:
        FOR EACH Medlem WHERE 
            Medlem.KundeNr = Kunde.KundeNr:
            FOR EACH MedlemsKort OF MEdlem:
                DELETE Medlemskort.
            END.
            DELETE Medlem.
        END.

        FOR EACH KundeKort OF Kunde:
            DELETE Kundekort.
        END.
        FOR EACH KundeTrans OF Kunde:
            DELETE KundeTrans.
        END.
        FOR EACH KundeBetTrans OF Kunde:
            DELETE KundeBetTrans.
        END.
        DELETE Kunde.
    END.
    DELETE KORdreHode.
END.
END PROCEDURE.


