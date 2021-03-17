DEF VAR cTotal AS CHAR FORMAT "x(15)" NO-UNDO.
CURRENT-WINDOW:WIDTH = 350.
FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 20 AND 
    CAN-FIND(FIRST PkSdlLinje OF PkSdlhode NO-LOCK WHERE 
             PkSdlLinje.butikkNr = 10),
    FIRST PkSdlMottak OF PkSdlHode NO-LOCK
    BY PkSdlMottak.MottattDato DESC:

    FIND LAST FakturaHode NO-LOCK WHERE 
        FakturaHode.PkSdlNr = PkSdlHode.PkSdlNr NO-ERROR.

    RUN pksdl_levverdi ( INPUT ROWID(PkSdlhode), OUTPUT cTotal).
    DISPLAY
        PkSdlHode.PkSdlId
        PkSdlHode.PkSdlNr
        PkSdlHode.PkSdlStatus
        PkSdlHode.SendtDato
        PkSdlHode.RegistrertDato
        PkSdlHode.EDato
        PkSdlHode.FakturaNr
        PkSdlHode.OrdreType
        PkSdlHode.PkSdlOpphav
        DEC(cTotal) FORMAT "->>>,>>>,>>9.99"
        '|'
        PkSdlMottak.MottattDato
        '|'
        FakturaHode.FakturaNr WHEN AVAILABLE FakturaHode
        FakturaHode.Totalt WHEN AVAILABLE FakturaHode
        FakturaHode.EksportertDato WHEN AVAILABLE FakturaHode
    WITH WIDTH 350.

END.

PROCEDURE pksdl_levverdi:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR fLevVerdi  AS DEC NO-UNDO.

  DEF BUFFER bPkSdlHode FOR PkSdlhode.

  FIND bPkSdlHode NO-LOCK
       WHERE ROWID(bPkSdlHode) = irPkSdlHode
       NO-ERROR.
  IF AVAIL bPkSdlHode THEN 
    FOR EACH PkSdlPris OF bPkSdlHode NO-LOCK:

      FOR EACH PkSdlLinje OF PkSdlPris NO-LOCK:
        ASSIGN 
            fLevVerdi    = fLevVerdi + PkSdlLinje.AntLevert * PkSdlPris.NyVarekost
            .
               
      END.
    END.
  
  ocValue = STRING(fLevVerdi).
END PROCEDURE.

