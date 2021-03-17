DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cRecord AS CHAR NO-UNDO.
DEF VAR lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR iAnv-Id AS INT FORMAT ">>>9" NO-UNDO.
DEF VAR iHovedKatNr AS INT FORMAT ">>>>9" NO-UNDO.
DEF VAR iVmId AS INT FORMAT ">>>>>>9" NO-UNDO.

/* Endringer her skal ikke utløse ny ELogg post og resending av ordre. */    
ON CREATE OF ArtBas OVERRIDE DO: END.
ON WRITE  OF ArtBas OVERRIDE DO: END.
ON CREATE OF ArtBas OVERRIDE DO: END.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cFil = 'konv\varelisteAre271119.csv'
    .

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cRecord.

    ASSIGN 
        lArtikkelNr = DEC(ENTRY(1,cRecord,';'))
        iVmId = INT(ENTRY(7,cRecord,';'))
        iAnv-Id = INT(ENTRY(9,cRecord,';'))
        iHovedKatNr = INT(ENTRY(8,cRecord,';'))
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT.

    FIND ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas AND ArtBas.Anv-Id = 0 THEN
    DO:
        /*
        DISPLAY
            lArtikkelNr
            '|'
            ArtBas.ArtikkelNr WHEN AVAILABLE ArtBas
            ArtBas.VmId WHEN AVAILABLE ArtBas
            ArtBas.Anv-Id FORMAT ">>>9" WHEN AVAILABLE ArtBas
            ArtBas.HovedKatNr FORMAT ">>>9" WHEN AVAILABLE ArtBas
            '|'
            iVmId
            iAnv-Id
            iHovedKatNr
            '|'
            cRecord FORMAT "x(100)"
        WITH width 350.
        */
        ASSIGN 
            ArtBas.Anv-Id     = iAnv-Id
            ArtBas.HovedKatNr = iHovedKatNr
            .
    END.

END.
INPUT STREAM Inn CLOSE.
