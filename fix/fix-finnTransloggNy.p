DEF VAR plArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR pcStorl AS CHAR NO-UNDO.
DEF VAR piButNr AS INT NO-UNDO.
DEF VAR piProfilNr AS INT NO-UNDO.
DEF VAR piDager AS INT NO-UNDO.   
DEF VAR plRab% AS DEC NO-UNDO.
DEF VAR piRabLimit AS INT NO-UNDO.

DEFINE BUFFER bufTranslogg FOR Translogg.
DEF BUFFER bufArtPris FOR ArtPris. 
DEF BUFFER bufButiker FOR Butiker.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    plArtikkelNr = 9854654
    pcStorl      = ' S'
    piButNr      = 17
    piDager      = (6 * 30)
    piRabLimit   = 9.5
    .
    
FIND bufButiker NO-LOCK WHERE 
    bufButiker.Butik = piButNr NO-ERROR.
IF AVAILABLE bufButiker THEN
DO:
    ASSIGN
        piProfilNr = bufButiker.ProfilNr
        .
    FIND bufArtPris NO-LOCK WHERE 
        bufArtPris.ArtikkelNr = plArtikkelNr AND 
        bufArtPris.ProfilNr   = piProfilNr NO-ERROR.
    IF NOT AVAILABLE bufArtPris THEN
    DO:
        MESSAGE 'Finner ikke ArtPris. Dvs. ingen innkjøp gjort.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        
        RETURN.
    END.
END.
FOR EACH bufTranslogg NO-LOCK WHERE 
    bufTranslogg.Butik      = pibutNr AND 
    bufTranslogg.ArtikkelNr = plArtikkelNr AND 
    bufTranslogg.Storl      = pcStorl AND 
    bufTranslogg.TTId       = 5 AND 
    bufTranslogg.Dato      >= TODAY - piDager:

    ASSIGN 
        plRab% = ROUND(((bufArtPris.InnkjopsPris[1] - bufTranslogg.Pris) * 100) / bufArtPris.InnkjopsPris[1],2)
        .
    IF plRab% > piRabLimit THEN
    DO:
        DISPLAY
            bufTranslogg.butik
            bufTranslogg.ArtikkelNr
            bufTranslogg.Storl
            bufTranslogg.Dato
            bufTranslogg.Antall
            bufTranslogg.RabKr
            bufTransLogg.Mva
            bufTranslogg.Pris
            '|'
            bufArtPris.InnkjopsPris[1]
            bufArtPris.Rab1%[1]
            bufArtPris.VareKost[1]
            bufArtPris.Pris[1]
            '|'
            plRab%
        WITH WIDTH 350.
    END.
END.


