DEF VAR cLst AS CHAR NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR iTelleNr AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    ibutNr = 9
    cLst   = '28547,28501,28499,28521,28494'
    .

DO iLoop = 1 TO NUM-ENTRIES(cLst):
    iTelleNr = INT(ENTRY(iLoop,cLst)).

    FIND TelleHode EXCLUSIVE-LOCK WHERE 
        TelleHode.TelleNr = itelleNr NO-ERROR.

    /*
    DISPLAY
        TelleHode.TelleNr
        TelleHode.LokasjonsId
        TelleHode.KobletTilTelleNr
        TelleHode.Oppdatert
        TelleHode.BatchNr
        TelleHode.TelleType
        TelleHode.TTId
        TelleHode.TbId
    WITH WIDTH 350.
    */
    /* Konverterer telleliste. */
    ASSIGN
        TelleHode.Oppdatert = ?
        TelleHode.BatchNr   = 0
        TelleHode.TTId      = 7
        TelleHode.TbId      = 1
        .

    FOR EACH TelleLinje OF TelleHode EXCLUSIVE-LOCK:
        /*
        DISPLAY
            TelleLinje.butik
            TelleLinje.LinjeNr
            TelleLinje.Oppdatert
            TelleLinje.AntallTalt
        WITH WIDTH 350.
        */
        ASSIGN
            TelleLinje.Oppdatert = FALSE
            .
    END.

END.
