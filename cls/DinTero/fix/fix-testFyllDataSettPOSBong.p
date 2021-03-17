DEF VAR iAnt AS INT NO-UNDO.

{cls\dintero\ttPOSBong.i}
{cls\dintero\dsPOSBong.i}

iAnt = 0.
BLOKKEN:
FOR  EACH POS.BongHode NO-LOCK USE-INDEX B_Id,
    FIRST POS.BongLinje NO-LOCK WHERE 
      POS.BongLinje.B_Id = POS.BongHode.B_Id AND 
      POS.BongLinje.TTId >= 1 AND 
      POS.BongLinje.TTId <= 11
    BREAK BY POS.BongHode.B_Id DESCENDING:



    iAnt = iAnt + 1.
    RUN cls\dintero\fylldatasettPOSBong.p ( POS.BongHode.B_Id,
                                            FALSE,
                                            INPUT-OUTPUT DATASET dsPOSBong
                                          ).
    
    DATASET dsPOSBong:WRITE-JSON('file',
                                 'konv\POSBonger\dsPOSBong' + 
                                 STRING(POS.BongHode.Butik,"999999") + '_' + 
                                 STRING(POS.BongHode.BongNr,"99999999") + 
                                 '.JSon').
    IF iAnt > 10 THEN 
        LEAVE BLOKKEN.
END. /* BLOKKEN */
