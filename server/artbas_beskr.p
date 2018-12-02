DEF INPUT PARAM irMixMatchRad AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

FIND MixMatchRad WHERE ROWID(MixMatchRad) = irMixMatchRad NO-LOCK NO-ERROR.
IF AVAIL MixMatchRad THEN
DO:
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = MixMatchRad.Kode NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode NO-ERROR.
    ocValue = IF AVAILABLE ArtBas
                THEN ArtBAs.Beskr
                ELSE "".
END.
