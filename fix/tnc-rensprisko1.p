FOR EACH ArtBas NO-LOCK WHERE
CAN-FIND(FIRST PrisKo OF ArtBas WHERE
         PrisKo.RegistrertDato < ArtBAs.RegistrertDato):
    PAUSE 0.
    DISPLAY
        ArtBas.ArtikkelNr
        ArtBAs.RegistrertDato
        .
    FOR EACH PrisKo OF ArtBAs WHERE
        PrisKo.RegistrertDato < ArtBas.RegistrertDato:
        PAUSE 0.
        DISPLAY
            PrisKo.ArtikkelNr
            PrisKo.RegistrertDato
            .
        DELETE prisko.
    END.
END.

FOR EACH PrisKo WHERE NOT CAN-FIND(ArtBas OF PrisKo):
  DELETE prisko.
END.

