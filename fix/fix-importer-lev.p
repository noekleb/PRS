DEF VAR pcLevNr   AS CHAR NO-UNDO.
DEF VAR pcNavn    AS CHAR NO-UNDO.
DEF VAR pcAdresse AS CHAR NO-UNDO.
DEF VAR pcPostNr  AS CHAR NO-UNDO.
DEF VAR pcSted    AS CHAR NO-UNDO.
DEF VAR pcTekst   AS CHAR NO-UNDO.
DEF VAR piLoop    AS INT  NO-UNDO.

DEF STREAM InnLev.
INPUT STREAM InnLev FROM VALUE("C:\ArkivDokument\LRS\Kunder & Prospects\Norge\Kunder\Hjem & Hobby\DataOppsettAvDB\LEVERANDØRNUMMER TIL BIS.csv") NO-ECHO.
REPEAT:

    IMPORT STREAM InnLev UNFORMATTED pcTekst.
    ASSIGN
        piLoop    = piLoop + 1
        pcLevNr   = ENTRY(1,pcTekst,";")
        pcNavn    = ENTRY(2,pcTekst,";")
        /*
        pcAdresse = ENTRY(3,pcTekst,";")
        pcPostNr  = ENTRY(4,pcTekst,";")
        pcSted    = ENTRY(5,pcTekst,";")
        */
        .

    STATUS DEFAULT "Behandler linje " + STRING(piLoop) + ".".

    FIND Post NO-LOCK WHERE
        Post.PostNr = pcPostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN
    DO:
        CREATE Post.
        ASSIGN
            Post.PostNr      = pcPostNr
            Post.Beskrivelse = pcSted
            .
    END.

    FIND FIRST LevBAs EXCLUSIVE-LOCK WHERE
        LevBas.LevNamn = pcNavn NO-ERROR.
    IF AVAILABLE LevBas THEN
    DO:
        ASSIGN
            LevBas.LevNr = INT(pcLevNr)
            NO-ERROR.
    END.
    ELSE DO TRANSACTION:
        CREATE LevBas.
        ASSIGN
            LevBAs.LevNr   = INT(pcLevNr)
            LevBAs.LevNamn = pcNavn
            LevBas.LevPoNr = pcPostNr
            LevBas.LevpAdr = pcSted
            NO-ERROR.
    END.

END.
INPUT STREAM InnLev CLOSE.
INPUT STREAM InnLev CLOSE.
