DEF VAR iAnt AS INT NO-UNDO.
FORM Medlem.MedlemSnr WITH FRAME g.    
FOR EACH Medlem NO-LOCK:
    RUN fix-oppdatmedlemstat.p (Medlem.MedlemsNr,2012).
    
    iAnt = iAnt + 1.

    IF IANT MODULO 100 = 0 THEN 
    DISPLAY
        iAnt 
        Medlem.MedlemsNr WITH FRAME g.
    PAUSE 0.
END.
