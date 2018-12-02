CURRENT-WINDOW:WIDTH = 200.

DEF VAR piLoop AS INT NO-UNDO.

FOR EACH StrTStr NO-LOCK WHERE  StrTStr.StrTypeId = 212 AND
  NOT CAN-FIND(StrKonv WHERE StrKonv.Storl = StrTStr.SoStorl):

    LOOPEN:
    REPEAT piLoop = 1 TO 999:
        IF NOT CAN-FIND(StrKonv WHERE
                        StrKonv.StrKode = piLoop) THEN
        DO:
           CREATE StrKonv.
           ASSIGN
               StrKonv.StrKode = piLoop
               StrKonv.Storl   = StrTStr.SoStorl
               NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
           DO:
               DELETE StrKonv.
               MESSAGE piLoop StrTStr.SoStorl
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
           END.
        END.
    END. /* LOOPEN */

    IF NOT CAN-FIND(StrKonv WHERE StrKonv.Storl = StrTStr.SoStorl) THEN
        DISPLAY
        StrTStr
        WITH WIDTH 200.
END.
