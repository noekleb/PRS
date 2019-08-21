CURRENT-WINDOW:WIDTH = 350.
FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = 164061 NO-ERROR.
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE 
    PkSdlLinje.ArtikkelNr = 9855960,
    FIRST PkSdlPris OF PksdlLinje NO-LOCK:
    DISPLAY
        PkSdlHode.PkSdlNr
        PkSdlHode.RegistrertDato
        PkSdlHode.EDato
        '|'
        PkSdlLinje.RegistrertDato
        PkSdlLinje.EDato
        '|'
        PkSdlPris.registrertDato
        PkSdlPris.EDato
        STRING(PkSdlPris.ETid,"HH:MM:SS")
    WITH WIDTH 350.
END.
