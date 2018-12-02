CURRENT-WINDOW:WIDTH = 350.
FOR EACH ButMalHode,
    EACH butMalLinje OF ButMalHode:
    DISPLAY
        ButMalHode.butMalNr
        ButMalHode.ButMalNavn
        ButMalHode.DatoTidOpprettet
        ButMalLinje.AvdelingNr
        ButMalLinje.fordeling%
        ButMalLinje.DatoTidEndret
    WITH WIDTH 350.
END.
