DEF VAR fLevVerdi  AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.

DEF BUFFER bufPkSdlLinje FOR PkSdlLinje.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.RegistrertDato >= 01/01/2018 AND 
    CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE 
             PkSdlLinje.butikkNr = 16) AND 
    PkSdlHode.FakturaNr = ? AND 
    PkSdlHode.PkSdlStatus = 20,
    FIRST PkSdlLinje OF PkSdlHode NO-LOCK:

    RUN pksdl_levverdi (OUTPUT fLevVerdi).

    DISPLAY
        PkSdlLinje.butikkNr
        PkSdlHode.PkSdlNr
        PkSdlHode.RegistrertDato
        PkSdlHode.LeveringsDato
        PkSdlHode.FakturaNr
        PkSdlHode.PkSdlStatus
        fLevVerdi (TOTAL)
    WITH WIDTH 350.
END.

PROCEDURE pksdl_levverdi:
  DEF OUTPUT PARAMETER pfLevVerdi  AS DEC NO-UNDO.
  
  IF AVAIL PkSdlHode THEN 
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:

      FOR EACH bufPkSdlLinje OF PkSdlPris NO-LOCK:
        ASSIGN 
            pfLevVerdi    = pfLevVerdi + bufPkSdlLinje.AntLevert * PkSdlPris.NyVarekost
            .
      END.
    END.
  
END PROCEDURE.

