DEF VAR lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR cEanLst AS CHAR FORMAT "x(120)" NO-UNDO.

DEF BUFFER bufArtBas FOR ArtBas.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.HG = 90:
    
    iAnt = 0.
    cEanLst = ''.
    FOR EACH Strekkode OF ArtBas NO-LOCK:
        IF Strekkode.Kode BEGINS '02000' THEN
        DO:
            NEXT.    
        END.
        iAnt = iant + 1.
        ASSIGN 
            cEanLst = cEanLst + 
                      (IF cEanLst <> '' THEN ',' ELSE '') + 
                      Strekkode.Kode.
    END.
    
    IF iAnt > 1 THEN
    DO:
        DISPLAY
            iAnt
            ArtBas.ArtikkelNr
            ArtBas.HG
            ArtBas.ModellFarge
            ArtBas.Beskr
            ArtBas.LevKod
            ArtBas.Farg FORMAT ">>>>>9"
            ArtBas.VmId
            ArtBas.HovedModellFarge
            ArtBas.RegistrertDato
            ArtBas.EDato
            cEanLst
        WITH WIDTH 350.
    END.
    
END.
    
