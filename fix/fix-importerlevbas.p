DEF VAR pcLevNr   AS CHAR NO-UNDO.
DEF VAR pcNavn    AS CHAR NO-UNDO.
DEF VAR pcAdresse AS CHAR NO-UNDO.
DEF VAR pcPostNr  AS CHAR NO-UNDO.
DEF VAR pcSted    AS CHAR NO-UNDO.
DEF VAR pcTekst   AS CHAR NO-UNDO.
DEF VAR piLoop    AS INT  NO-UNDO.
DEF VAR pcReferens AS CHAR NO-UNDO.
DEF VAR cTelefon AS CHAR NO-UNDO.

FOR EACH LevBas:
    DELETE LevBas.
END.

DEF STREAM InnLev.
INPUT STREAM InnLev FROM VALUE("C:\c:\home\lindbak\ankommet\leverantorer.csv") NO-ECHO.
REPEAT:

    IMPORT STREAM InnLev UNFORMATTED pcTekst.
    ASSIGN
        piLoop     = piLoop + 1
        pcNavn     = TRIM(ENTRY(1,pcTekst,";"))
        pcLevNr    = TRIM(ENTRY(2,pcTekst,";"))
        pcReferens = TRIM(ENTRY(3,pcTekst,";"))
        pcAdresse  = TRIM(ENTRY(4,pcTekst,";"))
        pcPostNr   = TRIM(ENTRY(5,pcTekst,";"))
        pcSted     = TRIM(ENTRY(6,pcTekst,";"))
        cTelefon   = TRIM(ENTRY(7,pcTekst,";"))
        .

    STATUS DEFAULT "Behandler linje " + STRING(piLoop) + ".".

    FIND Post NO-LOCK WHERE
        Post.PostNr = pcPostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN
    DO TRANSACTION:
        CREATE Post.
        ASSIGN
            Post.PostNr      = pcPostNr
            Post.Beskrivelse = pcSted
            .
    END.

    FIND FIRST LevBAs NO-LOCK WHERE
        LevBas.LevNr = INT(pcLevNr) NO-ERROR.
    IF NOT AVAILABLE LevBas THEN
    DO TRANSACTION:
        CREATE LevBas.
        ASSIGN
            LevBAs.LevNr   = INT(pcLevNr)
            LevBAs.LevNamn = pcNavn
            LevBas.LevPoNr = pcPostNr
            LevBas.LevpAdr = pcSted
            LevBas.LevKon  = pcReferens
            LevBas.LevAdr  = pcAdresse 
            LevBas.LevPoNr = pcPostNr  
            /*pcSted*/    
            LevBas.LevTel  = cTelefon  
            NO-ERROR.
    END.

END.
INPUT STREAM InnLev CLOSE.
INPUT STREAM InnLev CLOSE.
