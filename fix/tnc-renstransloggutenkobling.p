DEF VAR X AS INT NO-UNDO.
DEF STREAM fil.
OUTPUT STREAM fil TO VALUE("rensTranslogg.d") NO-ECHO.
FOR EACH TransLogg WHERE NOT CAN-FIND(ArtBas WHERE
                                      ArtBas.ArtikkelNr = TransLogg.ArtikkelNR):
    EXPORT STREAM fil TRansLogg.
    X = X + 1.
    PAUSE 0.
    IF X MODULO 50 = 0 THEN
    DISPLAY x
        WITH FRAME g
        .
    DELETE translogg.
                                      
END.
OUTPUT STREAM fil CLOSE.
