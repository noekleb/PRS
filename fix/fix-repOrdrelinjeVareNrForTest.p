CURRENT-WINDOW:WIDTH = 350.
DEF VAR lDummy AS DEC NO-UNDO.
FOR EACH KOrdreLinje:
    ASSIGN lDummy = DEC(KOrdreLinje.VareNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        NEXT.
    END.
    IF NOT CAN-FIND(ArtBas WHERE 
                    ArtBas.ArtikkelNr = DEC(KORdreLinje.VareNr)) THEN
    DO:
        FIND Strekkode NO-LOCK WHERE 
          Strekkode.Kode = KOrdreLinje.Kode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
          FIND ArtBas WHERE 
            ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
            FIND FIRST ArtBas WHERE ArtBas.HovedKatNr > 0 NO-ERROR.
        IF AVAILABLE ArtBas THEN
            ASSIGN KORdreLinje.VareNr = STRING(ArtBas.ArtikkelNr).
        MESSAGE 'gurre'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE 
        FIND ArtBas WHERE 
             ArtBas.ArtikkelNr = DEC(KORdreLinje.VareNr).
    IF ArtBas.HovedKatNr = 0 THEN
        ArtBas.HovedKatNr = 4.
    
    FIND HovedKategori NO-LOCK WHERE 
      HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.

    DISPLAY
        KOrdreLinje.KOrdre_Id
        KOrdreLinje.VareNr
        KOrdreLinje.Kode
        ArtBas.HovedKatNr
        HovedKategori.HovedKatTekst WHEN AVAILABLE HovedKategori
        AVAILABLE ArtBAs
    WITH WIDTH 350.
    
END.
