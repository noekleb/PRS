DEF VAR lDec AS DEC NO-UNDO.
DEF VAR lMinst AS DEC INIT 9999999999 FORMAT "->>>>>>>>>>>>9" NO-UNDO.

FOR EACH Kunde NO-LOCK:
    lDec = DEC(Kunde.EksterntKundeNr) NO-ERROR.

    IF lDec = 0  THEN next.

    IF ldec < lMinst THEN
        lMinst = lDec.
END.

UPDATE lMinst.
