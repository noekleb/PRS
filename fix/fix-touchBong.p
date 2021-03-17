CURRENT-WINDOW:WIDTH = 350.
DELETE FROM ELogg.
DEF VAR iAnt AS INT NO-UNDO.

BLOKKEN:
FOR EACH BongHode:
    FIND FIRST BongLinje NO-LOCK WHERE 
        BongLinje.B_Id = BongHode.B_Id AND 
        CAN-DO('1,3,10',STRING(BongLinje.TTId)) NO-ERROR.
    IF NOT AVAILABLE BongLinje THEN
        NEXT.

    FIND FIRST BongCRMLogg EXCLUSIVE-LOCK WHERE 
      BongCRMLogg.ButikkNr = BongHode.butikkNr AND 
      BongCRMLogg.GruppeNr = BongHode.GruppeNr AND   
      BongCRMLogg.KasseNr = BongHode.KasseNr AND 
      BongCRMLogg.Dato = BongHode.Dato AND 
      BongCRMLogg.BongNr = BongHode.BongNr NO-ERROR.
    IF AVAILABLE BongCRMLogg THEN
        NEXT.

    iAnt = iAnt + 1.
    BongHode.ETid = TIME.
    IF iAnt = 1 THEN 
        LEAVE BLOKKEN.

    DISPLAY
        BongHode.ButikkNr
        BongHode.GruppeNr
        BongHode.KasseNr
        BongHode.Dato
        BongHode.BongNr
        BongHode.Belop
        DATETIME(BongHode.Dato, BongHode.Tid * 1000)
    WITH WIDTH 350.
END.
