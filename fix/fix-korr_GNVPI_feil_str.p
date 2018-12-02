DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cEan AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cStorl AS CHAR NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

DEF STREAM Inn.

ASSIGN 
    cFil = 'C:\tmp\tn\GNVPI.csv'
    .

INPUT STREAM Inn FROM VALUE(cFil).
iAnt = 0.
LOOPEN:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.
    
    iant = iant + 1.
    IF iant < 3 THEN
        NEXT LOOPEN.

    ASSIGN
        cEan   = ENTRY(2,cLinje,';')
        cStorl = ENTRY(5,cLinje,';')
        .

    RUN bibl_fixstorl.p(cStorl,?,'',OUTPUT cStorl,OUTPUT bOk).
    RUN bibl_chkean.p(INPUT-OUTPUT cEan).

    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.Storl = cStorl NO-ERROR.
    IF AVAILABLE StrKonv THEN
    DO:
        FIND Strekkode EXCLUSIVE-LOCK WHERE
            Strekkode.Kode = cEan NO-ERROR.
        IF AVAILABLE Strekkode THEN
        DO:
            FIND ArtBas OF Strekkode EXCLUSIVE-LOCK.
            Strekkode.StrKode = StrKonv.StrKode.
            ArtBas.eDato = TODAY.
            ArtBas.ETid  = TIME.
        END.
    END.

    DISPLAY
        cEan
        cStorl
        strekkode.Kode WHEN AVAILABLE Strekkode
        Strekkode.StrKode WHEN AVAILABLE Strekkode
        .
END. /* LOOPEN */
INPUT STREAM Inn CLOSE.
