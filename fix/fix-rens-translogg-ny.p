
DEF VAR X AS INT.

DEF STREAM Rens.

OUTPUT STREAM Rens TO VALUE("rensTransLogg.d").
FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
    NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = Translogg.ArtikkelNr):

    X = X + 1.
    IF X MODULO 50 = 0 THEN
    DO:
        PAUSE 0.
        DISPLAY 
            x
            WITH FRAME g
            .
    END.

    EXPORT STREAM Rens Translogg.
    DELETE TransLogg.
END.
OUTPUT STREAM Rens CLOSE.

