DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk     AS LOG  NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

/*
FOR EACH Kundereskontr:

    DISPLAY

        Kundereskontr
    WITH WIDTH 350.
END.
*/

FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlOpphav = 4,
    FIRST KundeReskontr NO-LOCK WHERE
        kundeReskontr.Bilagstype = 1 AND
        KundeReskontr.FakturaNr  = DEC(PkSdlHode.PkSdlNr), 
    FIRST FakturaHode NO-LOCK WHERE
        FakturaHode.FakturaNr = Kundereskontr.FakturaNr:

    DISPLAY
        PkSdlHode.PkSdlNr
        KundeReskontr.FakturaNr
        .

    RUN kunderes_krednota.p(
                           STRING(Kundereskontr.KundeNr) + '|' + 
                           STRING(Kundereskontr.Reskontro_id) + '|' + 
                           STRING(FakturaHode.Totalt) + '|' + 
                           'Angret overføring.',
                           ?,
                           '',
                           OUTPUT ocReturn,
                           OUTPUT obOk
                           ).
END.

