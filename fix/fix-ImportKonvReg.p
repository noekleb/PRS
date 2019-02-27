/*
  TN 7/10-03
  rutine som konverterer PLU liste til brukbart csv format.
*/

DEF VAR cLinje  AS CHAR NO-UNDO.
DEF VAR cFelt1  AS CHAR NO-UNDO.
DEF VAR cFelt2  AS CHAR NO-UNDO.
DEF VAR cFelt3  AS CHAR NO-UNDO.
DEF VAR cFelt4  AS CHAR NO-UNDO.
DEF VAR cFelt5  AS CHAR NO-UNDO.
DEF VAR cFelt6  AS CHAR NO-UNDO.

DEF VAR cTekst1 AS CHAR NO-UNDO.
DEF VAR cTekst2 AS CHAR NO-UNDO.
DEF VAR cTekst3 AS CHAR NO-UNDO.

DEF VAR piLoop  AS INT  NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

DEF STREAM InnFil.

DEF TEMP-TABLE tmpImpKonv LIKE ImpKonv
    INDEX Internt InterntId
    .

INPUT STREAM InnFil FROM VALUE("pressbyran\ImpKonvOLD.d") NO-ECHO.
LESFRAFIL:
REPEAT :
  CREATE tmpImpKonv.
  IMPORT STREAM InnFil DELIMITER ";" tmpImpKonv. 

END. /* LESFRAFIL */

INPUT STREAM InnFil CLOSE.

FOR EACH tmpImpKonv:
    DISPLAY
        tmpImpKonv.EDB-System
        tmpImpKonv.Tabell
        tmpImpKonv.InterntId
        tmpImpKonv.EksterntId
        tmpImpKonv.Merknad
        WITH WIDTH 148 .

    FIND FIRST ImpKonv NO-LOCK WHERE
        ImpKonv.EDB-System = tmpImpKonv.EDB-System AND
        ImpKonv.Tabell     = tmpImpKonv.Tabell     AND
        ImpKonv.InterntId  = tmpImpKonv.InterntId  AND
        ImpKonv.EksterntId = tmpImpKonv.EksterntId NO-ERROR.
    IF NOT AVAILABLE ImpKonv THEN
    DO:
        CREATE impKonv.
        BUFFER-COPY tmpImpKonv TO ImpKonv.
        RELEASE ImpKonv.
    END.
    DELETE tmpImpKonv.
END.
