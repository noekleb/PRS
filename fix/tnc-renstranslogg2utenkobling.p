
DEF VAR X AS INT.
DEF STREAM fil .

OUTPUT STREAM fil TO VALUE("rens2Translogg.d") NO-ECHO.

FOR EACH ArtBas NO-LOCK WHERE
    CAN-FIND(FIRST TRansLogg WHERE
             TransLogg.ArtikkelNR = ArtBas.ArtikkelNR AND
             TransLogg.RegistrertDAto < ArtBas.RegistrertDato):
    FOR EACH TransLogg WHERE
        TransLogg.ArtikkelNR = ArtBAs.ARtikkelNr AND
        TransLogg.RegistrertDato < ArtBas.RegistrertDato:

        X = X + 1.
        PAUSE 0.
        IF X MODULO 10 = 0 THEN
            DISPLAY X WITH FRAME g.

        EXPORT STREAM fil TransLogg.
        DELETE Translogg.
    END.
END.
OUTPUT STREAM fil CLOSE.
