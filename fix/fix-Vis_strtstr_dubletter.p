DEF VAR piLoop AS INT NO-UNDO.
DEF VAR iStrTypeID AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.
FOR EACH StrTStr WHERE /*StrTStr.StrTypeid = 212*/
    BREAK BY StrTStr.StrTypeId
          BY StrTStr.SoStorl:
    IF FIRST-OF(StrTStr.SoStorl) THEN
        piLoop = 0.
    piLoop = piLoop + 1.
    IF piLoop > 1 THEN  DO:
        /*
        DISPLAY StrTStr.StrTypeId
                StrTStr.SoStorl
                piLoop
        WITH WIDTH 250.
        */
        iStrTypeID = StrTStr.StrTypeID.
        DELETE StrTStr.
        RUN settStrTypeFelt.p (iStrTypeID).
    END.
END.
