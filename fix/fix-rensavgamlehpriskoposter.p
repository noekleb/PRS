
DEF STREAM fil.
OUTPUT STREAM fil TO VALUE("hprisko.txt").

FOR EACH artbas NO-LOCK
    WHERE CAN-FIND (FIRST hprisko OF artbas WHERE
                    hprisko.RegistrertDato < ArtBas.RegistrertDato):

    FOR EACH HPrisKo OF ArtBAs WHERE
                    hprisko.RegistrertDato < ArtBas.RegistrertDato:
        DISPLAY 
            ArtBas.RegistrertDato
            hPrisKo.RegistrertDato
            .
        PAUSE 0.
        EXPORT STREAM fil hPrisko.
        DELETE hprisko.
    END.

END.

OUTPUT STREAM fil CLOSE.
