DEF VAR X AS INT.
DISPLAY "Transtype 10" WITH FRAME G.
FOR EACH BongHode NO-LOCK WHERE
    CAN-FIND(FIRST BongLinje OF bonghode WHERE BongLinje.TTId = 10):

    X = X + 1.


    IF X MODULO 50 = 0 THEN
    DO:
        DISPLAY X WITH FRAME g.
        PAUSE 0.
    END.
END.
DISPLAY "Transtype 03" WITH FRAME g.
FOR EACH BongHode NO-LOCK WHERE
    CAN-FIND(FIRST BongLinje OF bonghode WHERE BongLinje.TTId = 3):

    X = X + 1.

    IF X MODULO 50 = 0 THEN
    DO:
        DISPLAY X WITH FRAME g.
        PAUSE 0.
    END.
END.
PAUSE.

PROCEDURE FiksFortegn:

END PROCEDURE.
